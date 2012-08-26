open Core_extended.Std
open Async.Std
open Ort


type copy_file = { file_list : Fileutils.file_path list
		 ; src_path  : Fileutils.file_path
		 ; dst_path  : Fileutils.file_path
		 }

type command = string

type qsub_run  = { name          : Pm_sge_server.name
		 ; verbose       : bool
		 ; template_file : Fileutils.file_path
                 ; script_dir    : Fileutils.file_path
                 ; exec_queue    : Pm_sge_server.queue
                 ; data_queue    : Pm_sge_server.queue
		 ; pre           : command list
		 ; post          : command list
		 ; body          : command list
		 ; in_files      : copy_file list
		 ; out_files     : copy_file list
                 }

val submit : qsub_run -> Pm_sge_server.t -> (run_success, run_error) Result.t Deferred.t
