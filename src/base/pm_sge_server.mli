open Core_extended.Std
open Async.Std
open Ort

type name   = string
type queue  = string

type run_success = [ `Pending ]
type run_error   = [ `Script_not_found | `Qsub_error ]
type job_status  = [ `Not_found | `Pending | `Running | `Completed | `Failed ]
type wait_t      = [ `Not_found | `Completed | `Failed ]

type t

val start : unit -> t

val run :
  n:name ->
  q:queue ->
  Fileutils.file_path ->
  t ->
  (run_success, run_error) Result.t Deferred.t

val status : name -> t -> job_status Deferred.t

val wait : name -> t -> wait_t Deferred.t

val ack : name -> t -> unit
