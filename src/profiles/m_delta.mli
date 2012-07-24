type t = { sequences : (Ort.Fileutils.file_path * Ort.Fileutils.file_path)
	 ; delta_type : [ `Promer | `Nucmer ]
	 ; header : ((string * int) * (string * int))
	 ; ref_range : M_range.t
	 ; query_range : M_range.t
	 ; ref_gaps : M_range.t list
	 ; query_gaps : M_range.t list
	 }
val parse_delta_stream : string Stream.t -> t Stream.t
val parse_delta_file : Ort.Fileutils.file_path -> t Stream.t
val ref_profile_of_delta : t -> M_profile.t
val query_profile_of_delta : t -> M_profile.t
val deltas_of_gaps : t -> int list
val reverse : t -> t
val print : out_channel -> t -> unit
