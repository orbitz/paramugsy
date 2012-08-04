open Core_extended.Std
open Async.Std
open Ort

module Job_map = Map.Make(String)

type name   = string
type queue  = string
type job_id = string

type run_success = [ `Pending ]
type run_error   = [ `Script_not_found | `Qsub_error ]
type job_status  = [ `Not_found | `Pending | `Running | `Completed | `Failed ]
type wait_t      = [ `Not_found | `Completed | `Failed ]

type job_state   = [ `Pending | `Running | `Completed | `Failed ]

type msg =
  | Run of (name * queue * Fileutils.file_path * (job_id, job_id) Result.t Ivar.t)
  | Status of (name * job_status Ivar.t)
  | Ack of name

type t = { job_map : job_id Job_map.t
	 ; mq      : msg Tail.t
	 }

let rec loop s =
  ()
and handle_msg s = function
  | Run (n, q, script, retv) -> raise (Failure "not_implemented")
  | Status (n, retv) -> begin
    if Job_map.mem n s.job_map then begin
      Ivar.fill retv (snd (Job_map.find n s.job_map));
      Deferred.return s
    end
    else begin
      Ivar.fill retv `Not_found;
      Deferred.return s
    end
  end
  | Ack n ->
    Deferred.return {s with job_map = Job_map.remove n s.job_map}
and refresh_jobs s =
  raise (Failure "not_implemented")

let start_loop = loop

(*
 * API
 *)
let start () =
  let s = { job_map = Job_map.empty
	  ; mq      = Tail.create ()
	  }
  in
  start_loop s;
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
  Tail.extend s.mq (n, ret);
  Ivar.read ret >>= function
    | `Not_found -> Deferred.return `Not_found
    | `Completed -> Deferred.return `Completed
    | `Failed    -> Deferred.return `Failed
    | _          -> wait n s

let ack n s =
  Tail.extend s.mq (Ack n)
