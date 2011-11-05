open Ort

type sge_options = { priority : int
		   ; template_file : Fileutils.file_path
		   ; tmp_dir : Fileutils.file_path
		   ; script_dir : Fileutils.file_path
		   ; exec_queue : string
		   ; data_queue : string
		   ; out_dir : Fileutils.file_path
		   }

type copy_files = (Fileutils.file_path * Fileutils.file_path)

type job_id = private string

exception Qsub_cmd_failure of string

val wait_on_job_id : job_id -> job_id Lwt.t

val qsub_with_datasync : 
  ?wait:bool -> 
  ?verbose:bool -> 
  options:sge_options -> 
  in_files:copy_files list -> 
  out_files:copy_files list -> 
  string list -> 
  job_id Lwt.t
