open Core.Std
open Async.Std

open Ort

type t = { searches : (Fileutils.file_path * Fileutils.file_path) list
	 ; tmp_dir  : Fileutils.file_path
	 }

val make : t -> (Script_task.t, unit) Result.t Deferred.t
