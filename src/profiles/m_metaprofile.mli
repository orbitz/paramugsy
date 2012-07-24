type t = private { reversed : bool
		 ; profile : M_profile.t
		 }
val metaprofile_of_profile : M_profile.t -> t
val reverse : t -> t
val is_reverse : t -> bool
val profile_idx_of_profile_idx : t -> M_profile.profile_idx -> M_profile.profile_idx
val profile_idx_by_len : pos:M_profile.profile_idx -> len:int -> t -> M_profile.profile_idx
val seq_idx_of_profile_idx : t -> M_profile.profile_idx -> M_profile.seq_idx option
val profile_idx_of_seq_idx : t -> M_profile.seq_idx -> M_profile.profile_idx
val subset_profile : t -> (M_profile.profile_idx * M_profile.profile_idx) -> t option
val subset_seq : t -> (M_profile.seq_idx * M_profile.seq_idx) -> t
