open Ort

type mugsy_job = { job_id : Pm_sge_utils.job_id
		 ; mugsy_maf : Fileutils.file_path
		 }

val run_mugsy : 
  distance:int -> 
  minlength:int -> 
  Pm_sge_utils.sge_options -> 
  Fileutils.file_path list -> 
  Fileutils.file_path list -> 
  mugsy_job Lwt.t

val run_mugsy_with_profiles :
  distance:int ->
  minlength:int ->
  Pm_sge_utils.sge_options ->
  Fileutils.file_path ->
  Fileutils.file_path ->
  Fileutils.file_path list ->
  mugsy_job Lwt.t

val wait_on_mugsy_job : mugsy_job -> mugsy_job Lwt.t
