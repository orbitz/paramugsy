type t = M_profile.t Stream.t
val profile_stream_of_maf_stream : ?start_count:int -> basename:string -> string Stream.t -> t
val profile_stream_of_maf : basename:string -> Ort.Fileutils.file_path -> t
val string_of_direction : [< `Forward | `Reverse ] -> string
val direction_of_string : string -> [ `Forward | `Reverse ]
val split_maf : string -> (string * int * int * [ `Forward | `Reverse ] * int * string)
