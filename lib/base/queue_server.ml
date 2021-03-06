open Core.Std
open Async.Std

open Ort

module type TASK_DRIVER = sig
  type t

  val submit : Queue_job.t -> t option Deferred.t
  val status : t -> Queue_job.Job_status.t option Deferred.t
end

module Job_status = Queue_job.Job_status
module Name       = Queue_job.Name
module Queue      = Queue_job.Queue
module Job_map    = Name.Map


module Message = struct
  type t =
    | Run of (Name.t * Queue.t * Fileutils.file_path * bool Ivar.t)
    | Stop
    | Status of (Name.t * Job_status.t option Ivar.t)
    | Update_jobs
    | Ack of Name.t
end


let list_of_map = Job_map.to_alist

let map_of_list l =
  match Job_map.of_alist l with
    | `Duplicate_key _ -> failwith "dup key"
    | `Ok m            -> m

module Make = functor (Td : TASK_DRIVER) -> struct

  type t = { job_map : (Job_status.t * Td.t) Job_map.t
	   ; mq      : Message.t Tail.t
	   }

  let update_job_v = let module J = Job_status in function
    | (J.R J.Pending, id)   -> Td.status id
    | (J.R J.Running, id)   -> Td.status id
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
    after (sec 10.) >>> fun () ->
    if not (Tail.is_closed mq) then begin
      Tail.extend mq Message.Update_jobs;
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
    Td.submit job >>= function
      | Some job_id -> begin
	Ivar.fill retv true;
	let job_map =
	  Job_map.add
	    ~key:n
	    ~data:(Job_status.R Job_status.Pending, job_id)
	    s.job_map
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
    match Job_map.find s.job_map n with
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
    match Job_map.find s.job_map n with
      | Some (Job_status.D Job_status.Completed, _)
      | Some (Job_status.D Job_status.Failed, _) ->
	{s with job_map = Job_map.remove s.job_map n}
      | _ ->
	s

  (*
   * Message dispatcher
   *)
  let handle_msg s = function
    | Message.Run msg     -> handle_run msg s
    | Message.Stop        -> begin Tail.close_if_open s.mq; Deferred.return s end
    | Message.Status msg  -> Deferred.return (handle_status msg s)
    | Message.Update_jobs -> handle_update_jobs s
    | Message.Ack n       -> Deferred.return (handle_ack n s)

  (*
   * Message event loop
   *)
  let rec loop s =
    if not (Tail.is_closed s.mq) then
      let stream = Tail.collect s.mq in
      Stream.fold'
	~f:handle_msg
	~init:s
	stream >>= loop
    else
      Deferred.return ()

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
    ignore (start_loop s);
    s

  let stop s =
    send s.mq Message.Stop;
    Deferred.return ()

  let run ~n ~q script s =
    let ret = Ivar.create ()
    in
    send s.mq (Message.Run (n, q, script, ret));
    Ivar.read ret

  let status n s =
    let ret = Ivar.create ()
    in
    send s.mq (Message.Status (n, ret));
    Ivar.read ret

  let rec wait n s =
    let ret = Ivar.create () in
    let module J = Job_status in
    send s.mq (Message.Status (n, ret));
    Ivar.read ret >>= function
      | None                   -> Deferred.return None
      | Some (J.D J.Completed) -> Deferred.return (Some J.Completed)
      | Some (J.D J.Failed)    -> Deferred.return (Some J.Failed)
      | _                      -> after (sec 10.) >>= fun () -> wait n s

  let ack n s =
    send s.mq (Message.Ack n)
end
