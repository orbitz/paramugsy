open Core.Std
open Async.Std

open Ort

type t = { seqs      : Fileutils.file_path list
	 ; distance  : int
	 ; minlength : int
	 ; tmp_dir   : Fileutils.file_path
	 }

val make : t -> (Script_task.t, unit) Result.t Deferred.t
