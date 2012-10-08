open Async.Std

open Ort

val read_file   : Fileutils.file_path -> string Deferred.t
val write_lines : Fileutils.file_path -> string list -> unit Deferred.t
