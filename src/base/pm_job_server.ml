open Core_extended.Std
open Async.Std

module Heap = Core.Std.Heap

module Job_map = Map.Make(Global_state.Ref_compare)

type job_result = Completed | Failed
type run_result = (Pm_sge_server.name * job_result) list
type run_size   = int
type priority   = int


type queued_job = { priority  : int
		  ; reference : Global_state.reference
		  ; job       : Pm_qsub.t
		  ; result    : run_result Ivar.t
		  }

type msg =
  | Run (priority * Pm_qsub.t * run_result Ivar.t)
  | Done (queued_job * job_result)
  | Stop


type t = { queue    : queued_job Heap.t
	 ; running  : queued_job Job_map.t
	 ; run_size : run_size
	 ; mq       : msg Tail.t
	 ; sge      : Pm_sge_server.t
	 }

let heap_cmp j1 j2 = compare j2.priority j1.priority

let send mq msg =
  Tail.extend mq msg

let run_job mq sge job =
  Pm_qsub.submit job.job sge >>= function
    | Result.Ok () ->
      send mq (Done (job, Completed))
    | Result.Error _ ->
      send mq (Done (job, Failed))

let rec run_if_possible s =
  if Job_map.cardinal s.running < s.run_size then begin
    match Heap.pop s.queue with
      | Some job -> begin
	run_job s.mq s.sge job;
	run_if_possible {s with running = s.running + 1}
      end
      | None ->
	Deferred.return s
  end
  else
    Deferred.return s

let handle_msg s = function
  | Run (pri, job, res) -> begin
    let qj = { priority  = pri
	     ; reference = Global_state.make_ref ()
	     ; job       = job
	     ; result    = res
	     }
    in
    Heap.push s.queue qj;
    run_if_possible s
  end
  | Done (job, res) -> begin
    Ivar.fill job.result res;
    Deferred.return {s with running = s.running - 1}
  end
  | Stop -> begin
    Tail.close_if_open s.mq;
    Deferred.return s
  end

let rec loop s =
  (Stream.next (Tail.collect s.mq)) >>= function
    | Stream.Nil         -> Deferred.return ()
    | Stream.Cons (m, _) -> handle_msg s m >>= loop

(*
 * **************************************************
 * API
 * **************************************************
 *)
let start run_size =
  Deferred.return { queue = Heap.create ()
		  ; running = Job_map.empty
		  ; run_size = run_size
		  ; mq = Tail.create ()
		  ; sge = Pm_sge_server.start ()
		  }

let stop s =
  send s.mq Stop;
  Deferred.return ()

let run priority job =
  Deferred.return ()
