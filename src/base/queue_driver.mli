open Core.Std
open Async.Std

open Ort




module Make : functor (Qs : QUEUE_SERVER) -> sig
  type t

  val start  : unit -> t
  val stop   : t -> unit Deferred.t
  val submit : t -> Task.t -> Queue_job.Job_status.job_done Deferred.t
end
