open Ort

type search_files = (Fileutils.file_path * Fileutils.file_path)

type nucmer_job = { job_id : Pm_sge_utils.job_id
		  ; search_files : search_files
		  ; output_maf : Fileutils.file_path
		  ; output_delta : Fileutils.file_path
		  }

val run_nucmer : Pm_sge_utils.sge_options -> search_files list -> nucmer_job list Lwt.t
val wait_on_nucmer_jobs : nucmer_job list -> nucmer_job list Lwt.t
