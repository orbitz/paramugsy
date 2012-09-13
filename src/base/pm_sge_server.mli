open Core_extended.Std
open Async.Std
open Ort

type name  = string

type t

val start : unit -> t
val stop  : t -> unit Deferred.t

val run :
  n:name ->
  q:Queue_job.Queue.t ->
  Fileutils.file_path ->
  t ->
  bool Deferred.t

val status : name -> t -> Queue_job.Job_status.t option Deferred.t
val wait   : name -> t -> Queue_job.Job_status.job_done option Deferred.t
val ack    : name -> t -> unit
