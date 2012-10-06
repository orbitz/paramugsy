open Core.Std
open Async.Std

open Ort

type t = { fasta   : Fileutils.file_path
	 ; tmp_dir : Fileutils.file_path
	 }

val make : t -> (Script_task.t, unit) Result.t Deferred.t
