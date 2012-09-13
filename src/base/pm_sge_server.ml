open Core_extended.Std
open Async.Std
open Ort
open Ort.Function

module Job_status = Queue_job.Job_status

module Job_map = Map.Make(String)

type name   = string
type queue  = string

type msg =
  | Run of (name * queue * Fileutils.file_path * bool Ivar.t)
  | Stop
  | Status of (name * Job_status.t option Ivar.t)
  | Update_jobs
  | Ack of name

type t = { job_map : (Job_status.t * Sge_interface.t) Job_map.t
	 ; mq      : msg Tail.t
	 }

let sec = Core.Std.sec

let list_of_map m = Job_map.fold (fun k v a -> (k, v)::a) m []

let map_of_list =
  List.fold_left
    ~f:(fun m (k, v) -> Job_map.add k v m)
    ~init:Job_map.empty

(*
 * Map, y u no come with this?!?!
 * I should look into using Core's
 * polymorphic map, though
 *)
let fetch k m =
  try
    Some (Job_map.find k m)
  with Not_found ->
    None

let update_job_v = let module J = Job_status in function
  | (J.R J.Pending, id)   -> Sge_interface.status id
  | (J.R J.Running, id)   -> Sge_interface.status id
  | (J.D J.Completed, id) -> Deferred.return (Some (J.D J.Completed))
  | (J.D J.Failed, id)    -> Deferred.return (Some (J.D J.Failed))

let update_job_wrap (state, id) =
  update_job_v (state, id) >>= function
    | Some state ->
      Deferred.return (state, id)
    | None ->
      (* Where'd it go?!?! *)
      Deferred.return (Job_status.D Job_status.Failed, id)

let update_job (k, v) =
  update_job_wrap v >>= fun r ->
  Deferred.return (k, r)

(*
 * Kicks off an Update_jobs message every
 * 30 seconds.  There is no pusback on this
 * which may need to be added if updating
 * a job becomes costly.
 *)
let rec refresh_jobs_msg mq =
  after (sec 30.) >>> fun () ->
  if not (Tail.is_closed mq) then begin
    Tail.extend mq Update_jobs;
    refresh_jobs_msg mq
  end


(*
 * Runs a qsub command
 *)
let handle_run (n, q, script, retv) s =
  let job = { Queue_job.queue   = q
	    ;           payload = script
	    }
  in
  Sge_interface.submit job >>= function
    | Some job_id -> begin
      Ivar.fill retv true;
      let job_map = Job_map.add n (Job_status.R Job_status.Pending, job_id) s.job_map
      in
      Deferred.return {s with job_map = job_map}
    end
    | None -> begin
      Ivar.fill retv false;
      Deferred.return s
    end

(*
 * Returns the status of a job in the job map
 *)
let handle_status (n, retv) s =
  match fetch n s.job_map with
    | Some (state, _id) -> begin
      Ivar.fill retv (Some state);
      s
    end
    | None -> begin
      Ivar.fill retv None;
      s
    end

(*
 * A timer will kick this off every 30 seconds,
 * so hopefully this takes less than 30 seconds
 * execute (no pushback yet).
 *
 * This updates the state of all running jobs
 * in the map.
 *)
let handle_update_jobs s =
  let jobs = list_of_map s.job_map
  in
  Deferred.List.map ~f:update_job jobs >>= fun l ->
  Deferred.return {s with job_map = map_of_list l}

(*
 * Acking a message deletes it from the list
 * only if it is Completed or Failed
 *)
let handle_ack n s =
  match fetch n s.job_map with
    | Some (Job_status.D Job_status.Completed, _)
    | Some (Job_status.D Job_status.Failed, _) ->
      {s with job_map = Job_map.remove n s.job_map}
    | _ ->
      s

(*
 * Message dispatcher
 *)
let handle_msg s = function
  | Run msg     -> handle_run msg s
  | Stop        -> begin Tail.close_if_open s.mq; Deferred.return s end
  | Status msg  -> Deferred.return (handle_status msg s)
  | Update_jobs -> handle_update_jobs s
  | Ack n       -> Deferred.return (handle_ack n s)

(*
 * Message event loop
 *)
let rec loop s =
  (Stream.next (Tail.collect s.mq)) >>= function
    | Stream.Nil         -> Deferred.return ()
    | Stream.Cons (m, _) -> handle_msg s m >>= loop

let send mq msg = Tail.extend mq msg

let start_loop = loop

(*
 * **************************************************
 * API
 * **************************************************
 *)
let start () =
  let s = { job_map = Job_map.empty
	  ; mq      = Tail.create ()
	  }
  in
  refresh_jobs_msg s.mq;
  let _ = start_loop s
  in
  s

let stop s =
  send s.mq Stop;
  Deferred.return ()

let run ~n ~q script s =
  let ret = Ivar.create ()
  in
  send s.mq (Run (n, q, script, ret));
  Ivar.read ret

let status n s =
  let ret = Ivar.create ()
  in
  send s.mq (Status (n, ret));
  Ivar.read ret

let rec wait n s =
  let ret = Ivar.create () in
  let module J = Job_status in
  send s.mq (Status (n, ret));
  Ivar.read ret >>= function
    | None               -> Deferred.return None
    | Some (J.D J.Completed) -> Deferred.return (Some J.Completed)
    | Some (J.D J.Failed)    -> Deferred.return (Some J.Failed)
    | _                      -> after (sec 30.) >>= fun () -> wait n s

let ack n s =
  send s.mq (Ack n)
