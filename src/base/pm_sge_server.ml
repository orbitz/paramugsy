open Core_extended.Std
open Async.Std
open Ort
open Ort.Function

module Job_map = Map.Make(String)

type name   = string
type queue  = string
type job_id = string

type run_success = unit
type run_error   = Qsub_error
type job_running = Pending | Running
type job_done    = Completed | Failed

type job_status  = R of job_running | D of job_done

type msg =
  | Run of (name * queue * Fileutils.file_path * (run_success, run_error) Result.t Ivar.t)
  | Stop
  | Status of (name * job_status option Ivar.t)
  | Update_jobs
  | Ack of name

type t = { job_map : (job_status * job_id) Job_map.t
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

(*
 * The output of 'qacct' is key-value pairs,
 * this splits and stripps and returns a list
 * of typles
 *)
let split_lines s =
  let split_and_strip line =
    line
    |> String.split ~on:' '
    |> List.map ~f:String.strip
    |> List.filter ~f:((<>) "")
  and two_tuple  = function
    | [x1; x2] -> (x1, x2)
    | x1::xs   -> (x1, String.concat ~sep:" " xs)
    | _        -> raise (Failure "Unknown line")
  in
  s
  |> String.split ~on:'\n'
  |> List.filter ~f:(fun l -> not (String.is_empty l) && l.[0] <> '=')
  |> List.map ~f:split_and_strip
  |> List.map ~f:two_tuple


let get_done_job_state id =
  Async_cmd.get_output ~text:"" ~prog:"qacct" ~args:["-j"; id] >>= function
    | Result.Ok (stdout, _) -> begin
      let kv = split_lines stdout
      in
      match List.Assoc.find kv ~equal:(=) "exit_status" with
	| Some "0" -> Deferred.return (D Completed, id)
	| Some _   -> Deferred.return (D Failed, id)
	| None     -> Deferred.return (D Failed, id)
    end
    | Result.Error _ ->
      Deferred.return (D Failed, id)


let is_job_running id =
  Async_cmd.get_output ~text:"" ~prog:"qstat" ~args:["-j"; id] >>= function
    | Result.Ok _    -> Deferred.return true
    | Result.Error _ -> Deferred.return false

(*
 * In SGE, if a job is running or pending it will
 * show up in the 'qstat' command.  If it is not
 * there, we check qacct for its exist code.
 *
 * Note, this makes 'pending' a useless state, for now,
 * since we just set it when we submit and it will be
 * marked as 'running' as soon as we do the first update.
 *)
let update_job_id id =
  is_job_running id >>= function
    | true -> Deferred.return (R Running, id)
    | false -> get_done_job_state id

let update_job_v = function
  | (R Pending, id)   -> update_job_id id
  | (R Running, id)   -> update_job_id id
  | (D Completed, id) -> Deferred.return (D Completed, id)
  | (D Failed, id)    -> Deferred.return (D Failed, id)

let update_job (k, v) =
  update_job_v v >>= fun r ->
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
 * Just run a sript in a queue and return
 * the job id
 *)
let qsub_cmd queue script =
  let args =
    [ "-o"; "/mnt/scratch"
    ; "-e"; "/mnt/scratch"
    ; "-S"; "/bin/sh"
    ; "-b"; "n"
    ; "-sync"; "n"
    ; "-q"; queue
    ; script]
  in
  Async_cmd.get_output ~text:"" ~prog:"qsub" ~args:args >>= function
    | Result.Ok (stdout, _) -> begin
      match String.split_on_chars ~on:[' '] stdout with
	| "Your"::"job"::job_id::_ ->
	  Deferred.return (Result.Ok job_id)
	| _ ->
	  Deferred.return (Result.Error Qsub_error)
    end
    | Result.Error _ ->
      Deferred.return (Result.Error Qsub_error)

(*
 * Runs a qsub command
 *)
let handle_run (n, q, script, retv) s =
  qsub_cmd q script >>= function
    | Result.Ok job_id -> begin
      Ivar.fill retv (Result.Ok ());
      let job_map = Job_map.add n (R Pending, job_id) s.job_map
      in
      Deferred.return {s with job_map = job_map}
    end
    | Result.Error err -> begin
      Ivar.fill retv (Result.Error err);
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
    | Some (D Completed, _)
    | Some (D Failed, _) ->
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
  Tail.extend s.mq Stop;
  Deferred.return ()

let run ~n ~q script s =
  let ret = Ivar.create ()
  in
  Tail.extend s.mq (Run (n, q, script, ret));
  Ivar.read ret

let status n s =
  let ret = Ivar.create ()
  in
  Tail.extend s.mq (Status (n, ret));
  Ivar.read ret

let rec wait n s =
  let ret = Ivar.create ()
  in
  Tail.extend s.mq (Status (n, ret));
  Ivar.read ret >>= function
    | None               -> Deferred.return None
    | Some (D Completed) -> Deferred.return (Some Completed)
    | Some (D Failed)    -> Deferred.return (Some Failed)
    | _                  -> after (sec 30.) >>= fun () -> wait n s

let ack n s =
  Tail.extend s.mq (Ack n)
