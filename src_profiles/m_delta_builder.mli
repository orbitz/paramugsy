type t
val create : 
  sequences:(string * string) ->
  delta_type:[ `Promer | `Nucmer] ->
  header:((string * int) * (string * int)) ->
  ref_start:M_profile.profile_idx -> 
  query_metaprofile:M_metaprofile.t -> 
  query_start:M_profile.profile_idx -> t
val add_gap : strand:[ `Ref | `Query ] -> diff:M_range.t -> t -> t
val add_offset : offset:int -> t -> t
val to_delta : t -> M_delta.t option
val print : out_channel -> t -> unit
