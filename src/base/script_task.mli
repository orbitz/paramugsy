type t = { pre       : command list
	 ; post      : command list
	 ; body      : command list
	 ; in_files  : copy_file list
	 ; out_files : copy_file list
	 }

val to_string : t -> string
