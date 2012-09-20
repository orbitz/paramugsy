open Async.Std

module type JOB_DRIVER = sig
  type t

  val start  : unit -> t
  val stop   : t -> unit Deferred.t
  val submit : t -> Queue_job.t -> Queue_job.Job_status.job_done Deferred.t
end

module Make : functor (Qd : JOB_DRIVER) -> sig
  val run : unit -> unit
end

