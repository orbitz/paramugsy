open Core.Std
open Async.Std

open Ort

type copy_file = { file_list : Fileutils.file_path
		 ; src_path  : Fileutils.file_path
		 ; dst_path  : Fileutils.file_path
		 }

type command = string

type t  = { name          : Queue_job.Name.t
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


module type QUEUE_SERVER = sig
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

end

module Make : functor (Qs : QUEUE_SERVER) -> sig
  val start  : unit -> Qs.t
  val stop   : Qs.t -> unit Deferred.t
  val submit : t -> Qs.t -> Queue_job.Job_status.job_done Deferred.t
end
