open Core.Std

open Ort

module Copy_file : sig
  type t = { file_list : Fileutils.file_path
	   ; src_path  : Fileutils.file_path
	   ; dst_path  : Fileutils.file_path
	   }
end

module Command : sig
  type t = string
end

type t = { name      : Queue_job.Name.t
	 ; pre       : Command.t list
	 ; post      : Command.t list
	 ; body      : Command.t list
	 ; in_files  : Copy_file.t list
	 ; out_files : Copy_file.t list
	 ; out_paths : String.t String.Map.t
	 }

val to_string : ?data_queue:(Queue_job.Queue.t option) -> string -> t -> string
