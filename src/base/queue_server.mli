open Core_extended.Std
open Async.Std

open Ort

module type QUEUE_SERVER = sig
  type name        = string
  type queue       = string

  type run_success = unit
  type run_error   = Queue_error
  type job_running = Pending | Running
  type job_done    = Completed | Failed

  type job_status  = R of job_running | D of job_done

  type t

  val start : unit -> t
  val stop  : t -> unit Deferred.t

  val run :
    n:name ->
    q:queue ->
    Fileutils.file_path ->
    t ->
    (run_success, run_error) Result.t Deferred.t

  val status : name -> t -> job_status option Deferred.t
  val wait   : name -> t -> job_done option Deferred.t
  val ack    : name -> t -> unit

end


module Make : functor (Qs : QUEUE_SERVER) -> sig
  type job_done = Qs.job_done

  type copy_file = { file_list : Fileutils.file_path
		   ; src_path  : Fileutils.file_path
		   ; dst_path  : Fileutils.file_path
		   }

  type command = string

  type t  = { name          : Qs.name
	    ; verbose       : bool
	    ; template_file : Fileutils.file_path
            ; script_dir    : Fileutils.file_path
            ; exec_queue    : Qs.queue
            ; data_queue    : Qs.queue
	    ; pre           : command list
	    ; post          : command list
	    ; body          : command list
	    ; in_files      : copy_file list
	    ; out_files     : copy_file list
            }

  val start  : unit -> Qs.t
  val stop   : Qs.t -> unit Deferred.t
  val submit : t -> Qs.t -> job_done Deferred.t

end
