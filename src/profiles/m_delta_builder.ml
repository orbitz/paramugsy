open Core_extended.Std

type t = { sequences : (string * string)
	 ; delta_type : [ `Promer | `Nucmer ]
	 ; header : ((string * int) * (string * int))
	 ; ref_start : M_profile.profile_idx
	 ; ref_pos : M_profile.profile_idx
	 ; query_metaprofile : M_metaprofile.t
	 ; query_start : M_profile.profile_idx
	 ; query_pos : M_profile.profile_idx
	 ; ref_gaps : M_range.t list
	 ; query_gaps : M_range.t list
	 }

let create ~sequences ~delta_type ~header ~ref_start ~query_metaprofile ~query_start =
  { sequences = sequences
  ; delta_type = delta_type
  ; header = header
  ; ref_start = ref_start
  ; ref_pos = ref_start
  ; query_metaprofile = query_metaprofile
  ; query_start = query_start
  ; query_pos = query_start
  ; ref_gaps = []
  ; query_gaps = []
  }

let add_gap ~strand ~diff db =
  let sum = List.fold_left ~f:(+) ~init:0 in
  match strand with
    | `Ref ->
      let ref_len = M_profile.diff_profile_idx db.ref_pos db.ref_start + sum (List.map ~f:M_range.length db.ref_gaps) in
      let gap = M_range.lift ~f:(fun (s, e) -> (s + ref_len + 1, e + ref_len + 1)) diff in
      { db with
	ref_pos = M_profile.lift_profile_idx ~f:((+) (M_range.get_start diff)) db.ref_pos;
	query_pos = M_profile.lift_profile_idx ~f:((+) (M_range.get_end diff + 1)) db.query_pos;
	ref_gaps = gap::db.ref_gaps;
      }
    | `Query ->
      let query_len = M_profile.diff_profile_idx db.query_pos db.query_start + sum (List.map ~f:M_range.length db.query_gaps) in
      let gap = M_range.lift ~f:(fun (s, e) -> (s + query_len + 1, e + query_len + 1)) diff in
      { db with
	ref_pos = M_profile.lift_profile_idx ~f:((+) (M_range.get_end diff + 1)) db.ref_pos;
	query_pos = M_profile.lift_profile_idx ~f:((+) (M_range.get_start diff)) db.query_pos;
	query_gaps = gap::db.query_gaps;
      }

let add_offset ~offset db =
  { db with
    ref_pos = M_profile.lift_profile_idx ~f:((+) offset) db.ref_pos;
    query_pos = M_profile.lift_profile_idx ~f:((+) offset) db.query_pos;
  }

let to_delta db =
  (* let gaps_of_metaprofile_gaps mp = *)
  (*   List.map ~f:(M_range.lift ~f:(fun (s, e) -> (M_profile.int_of_profile_idx *)
  (* 						   (M_metaprofile.profile_idx_of_profile_idx mp *)
  (* 						      (M_profile.profile_idx_of_int s)), *)
  (* 						 M_profile.int_of_profile_idx *)
  (* 						   (M_metaprofile.profile_idx_of_profile_idx mp *)
  (* 						      (M_profile.profile_idx_of_int e))))) *)
  (* in *)
  if db.ref_start <> db.ref_pos && db.query_start <> db.query_pos then
    let query_start =
      M_metaprofile.profile_idx_of_profile_idx
	db.query_metaprofile
	db.query_start
    in
    let query_pos =
      M_metaprofile.profile_idx_of_profile_idx
	db.query_metaprofile
	(M_profile.lift_profile_idx ~f:(fun pos -> pos - 1) db.query_pos)
    in
    Some
      { M_delta.sequences = db.sequences
      ; M_delta.delta_type = db.delta_type
      ; M_delta.header = db.header
      ; M_delta.ref_range = M_range.of_tuple (M_profile.int_of_profile_idx db.ref_start,
					      M_profile.int_of_profile_idx db.ref_pos - 1)
      ; M_delta.query_range = M_range.of_tuple (M_profile.int_of_profile_idx query_start,
						M_profile.int_of_profile_idx query_pos)
      ; M_delta.ref_gaps = List.rev db.ref_gaps
      ; M_delta.query_gaps = List.rev db.query_gaps
      (* ; M_delta.query_gaps = gaps_of_metaprofile_gaps db.query_metaprofile (List.rev db.query_gaps) *)
      }
  else
    None


let print out_channel db =
  Printf.fprintf
    out_channel
    "= sequences = (%s, %s)\n"
    (fst db.sequences)
    (snd db.sequences);
  Printf.fprintf
    out_channel
    "= header = ((%s, %d), (%s, %d))\n"
    (fst (fst db.header))
    (snd (fst db.header))
    (fst (snd db.header))
    (snd (snd db.header));
  Printf.fprintf
    out_channel
    "= ref_start = %d\n"
    (M_profile.int_of_profile_idx db.ref_start);
  Printf.fprintf
    out_channel
    "= ref_pos = %d\n"
    (M_profile.int_of_profile_idx db.ref_pos);
  Printf.fprintf
    out_channel
    "= query_start = %d\n"
    (M_profile.int_of_profile_idx db.query_start);
  Printf.fprintf
    out_channel
    "= query_pos = %d\n"
    (M_profile.int_of_profile_idx db.query_pos);
  Printf.fprintf
    out_channel
    "= ref_gaps =\n[ ";
  List.iter
    ~f:(fun g ->
      Printf.fprintf
	out_channel
	"(%d, %d) "
	(M_range.get_start g)
	(M_range.get_end g))
    (List.rev db.ref_gaps);
  Printf.fprintf
    out_channel
    " ]\n";
  Printf.fprintf
    out_channel
    "= query_gaps =\n[ ";
  List.iter
    ~f:(fun g ->
      Printf.fprintf
	out_channel
	"(%d, %d) "
	(M_range.get_start g)
	(M_range.get_end g))
    (List.rev db.query_gaps);
  Printf.fprintf
    out_channel
    " ]\n";
