open Core_extended.Std
open Async.Std
open Ort

type name        = string
type queue       = string

type run_success = unit
type run_error   = Script_not_found | Qsub_error
type job_running = Pending | Running
type job_done    = Completed | Failed

type job_status  = R of job_running | D of job_done

type t

val start : unit -> t

val run :
  n:name ->
  q:queue ->
  Fileutils.file_path ->
  t ->
  (run_success, run_error) Result.t Deferred.t

val status : name -> t -> job_status option Deferred.t
val wait   : name -> t -> job_done option Deferred.t
val ack    : name -> t -> unit
