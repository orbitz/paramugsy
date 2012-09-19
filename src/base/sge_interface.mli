open Core.Std
open Async.Std

type t

val submit : Queue_job.t -> t option Deferred.t
val status : t -> Queue_job.Job_status.t option Deferred.t
