open Async.Std

open Ort

type t = { seq_list       : Fileutils.file_path list
	 ; run_size       : int
	 ; exec_q         : Queue_job.Queue.t
	 ; data_q         : Queue_job.Queue.t option
	 ; tmp_dir        : Fileutils.file_path
	 ; distance       : int
	 ; minlength      : int
	 ; template_file  : Fileutils.file_path
	 ; seqs_per_mugsy : int
	 ; nucmer_chunk   : int
	 ; out_maf        : Fileutils.file_path
	 }


module Copy_file : sig
  type t = { file_list : Fileutils.file_path
	   ; src_path  : Fileutils.file_path
	   ; dst_path  : Fileutils.file_path
	   }
end

module Command : sig
  type t = string
end

module Task : sig
  type t  = { name          : Queue_job.Name.t
	    ; template_file : Fileutils.file_path
            ; script_dir    : Fileutils.file_path
            ; exec_queue    : Queue_job.Queue.t
            ; data_queue    : Queue_job.Queue.t option
	    ; task          : Script_task.t
            }
end


module Make : functor (Sts : SCRIPT_TASK_SERVER) -> sig
  val run : t -> int Deferred.t
end

