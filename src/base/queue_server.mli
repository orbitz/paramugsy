open Core_extended.Std
open Async.Std

open Ort

module type QUEUE_SERVER = sig
  type name = string

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

end


module Make : functor (Qs : QUEUE_SERVER) -> sig
  type copy_file = { file_list : Fileutils.file_path
		   ; src_path  : Fileutils.file_path
		   ; dst_path  : Fileutils.file_path
		   }

  type command = string

  type t  = { name          : Qs.name
	    ; verbose       : bool
	    ; template_file : Fileutils.file_path
            ; script_dir    : Fileutils.file_path
            ; exec_queue    : Queue_job.Queue.t
            ; data_queue    : Queue_job.Queue.t
	    ; pre           : command list
	    ; post          : command list
	    ; body          : command list
	    ; in_files      : copy_file list
	    ; out_files     : copy_file list
            }

  val start  : unit -> Qs.t
  val stop   : Qs.t -> unit Deferred.t
  val submit : t -> Qs.t -> Queue_job.Job_status.job_done Deferred.t

end
