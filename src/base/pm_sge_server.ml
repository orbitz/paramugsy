open Core_extended.Std
open Async.Std
open Ort
open Ort.Function

module Job_map = Map.Make(String)

type name   = string
type queue  = string
type job_id = string

type run_success = unit
type run_error   = Script_not_found | Qsub_error
type job_running = Pending | Running
type job_done    = Completed | Failed

type job_status  = R of job_running | D of job_done

type msg =
  | Run of (name * queue * Fileutils.file_path * (run_success, run_error) Result.t Ivar.t)
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

let rec refresh_jobs_msg mq =
  after (sec 30.) >>> fun () ->
  if not (Tail.is_closed mq) then begin
    Tail.extend mq Update_jobs;
    refresh_jobs_msg mq
  end

let rec loop s =
  (Stream.next (Tail.collect s.mq)) >>= function
    | Stream.Nil         -> Deferred.return ()
    | Stream.Cons (m, _) -> handle_msg s m
and handle_msg s = function
  | Run (n, q, script, retv) -> raise (Failure "not_implemented")
  | Status (n, retv) -> begin
    if Job_map.mem n s.job_map then begin
      Ivar.fill retv (Some (fst (Job_map.find n s.job_map)));
      loop s
    end
    else begin
      Ivar.fill retv None;
      loop s
    end
  end
  | Update_jobs -> refresh_jobs s
  | Ack n ->
    loop {s with job_map = Job_map.remove n s.job_map}
and refresh_jobs s =
  let jobs = list_of_map s.job_map
  in
  Deferred.List.map ~f:update_job jobs >>= fun l ->
  loop {s with job_map = map_of_list l}

let start_loop = loop

(*
 * API
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
