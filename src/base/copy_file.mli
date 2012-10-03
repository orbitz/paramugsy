open Async.Std

open Ort

val copy_file :
  Fileutils.file_path ->
  Fileutils.file_path ->
  (Fileutils.file_path, Async_cmd.cmd_exit) Result.t Deferred.t
