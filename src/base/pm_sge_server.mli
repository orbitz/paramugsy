open Core.Std
open Async.Std
open Ort

type t

val start : unit -> t
val stop  : t -> unit Deferred.t

val run :
  n:Queue_job.Name.t ->
  q:Queue_job.Queue.t ->
  Fileutils.file_path ->
  t ->
  bool Deferred.t

val status : Queue_job.Name.t -> t -> Queue_job.Job_status.t option Deferred.t
val wait   : Queue_job.Name.t -> t -> Queue_job.Job_status.job_done option Deferred.t
val ack    : Queue_job.Name.t -> t -> unit
