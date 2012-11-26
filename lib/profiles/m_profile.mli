type profile_name = (string * string)
type seq_idx = private int
type profile_idx = private int
type t = { p_name : profile_name
	 ; p_seq_name : string
	 ; p_range : M_range.t
	 ; p_length : int
	 ; p_gaps : M_range.t list
	 ; p_src_size : int
	 ; p_seq_text : string
	 }
exception Seq_idx_out_of_range of (int * M_range.t)
exception Profile_idx_out_of_range of (int * int)
exception Seq_idx_invalid of int
exception Profile_idx_invalid of int
val profile_of_maf_entry : name:profile_name -> seq_name:string -> range:M_range.t -> src_size:int -> text:string -> t
val read_profile_file : ?lite:bool -> string Stream.t -> t option
val write_profile_file : t -> out_channel -> unit
val seq_idx_of_int : int -> seq_idx
val profile_idx_of_int : int -> profile_idx
val int_of_seq_idx : seq_idx -> int
val int_of_profile_idx : profile_idx -> int
val lift_seq_idx : f:(int -> int) -> seq_idx -> seq_idx
val lift_profile_idx : f:(int -> int) -> profile_idx -> profile_idx
val profile_idx_of_seq_idx : t -> seq_idx -> profile_idx
val seq_idx_of_profile_idx : t -> profile_idx -> seq_idx option
val subset_profile : t -> (profile_idx * profile_idx) -> t option
val subset_seq : t -> (seq_idx * seq_idx) -> t
val reverse : t -> t
val diff_profile_idx : profile_idx -> profile_idx -> int
val diff_seq_idx : seq_idx -> seq_idx -> int
val print : out_channel -> t -> unit
