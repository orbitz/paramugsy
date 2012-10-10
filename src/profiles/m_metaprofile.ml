open Core

type t = { reversed : bool
	 ; profile : M_profile.t
	 }

let metaprofile_of_profile p =
  { reversed = false
  ; profile = p
  }

let reverse mp =
  { mp with reversed = not mp.reversed }

let is_reverse mp = mp.reversed

let profile_idx_of_profile_idx mp pi =
  if not mp.reversed then
    pi
  else
    M_profile.lift_profile_idx ~f:(fun pi -> mp.profile.M_profile.p_length - pi + 1) pi

let profile_idx_by_len ~pos ~len mp =
    if not mp.reversed then
      M_profile.lift_profile_idx ~f:(fun p -> p + len) pos
    else
      M_profile.lift_profile_idx ~f:(fun p -> p - len) pos

let seq_idx_of_profile_idx mp pi =
  M_profile.seq_idx_of_profile_idx mp.profile (profile_idx_of_profile_idx mp pi)

let profile_idx_of_seq_idx mp si =
  profile_idx_of_profile_idx mp (M_profile.profile_idx_of_seq_idx mp.profile si)

let subset_profile mp (s, e) =
  let se =
    if not mp.reversed then
      (s, e)
    else
      (e, s)
  in
  Option.map ~f:(fun p -> { mp with profile = p }) (M_profile.subset_profile mp.profile se)

let subset_seq mp se =
  { mp with profile = M_profile.subset_seq mp.profile se }
