open Core_extended.Std
open Async.Std

type job_result = Completed | Failed
type run_size   = int
type priority   = int

type t

(* Takes the number of jobs to run concurrently *)
val start : run_size -> t

val stop  : t -> unit Deferred.t

(*
 * Runs a job with a priority (higher runs sooner)
 * and blocks until the job is done
 *)
val run : priority -> Pm_qsub.t -> job_result Deferred.t
