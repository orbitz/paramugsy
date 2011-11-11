(*pp camlp4o *)
open Core_extended.Std
open Core_extended.Function
open Ort.Function
open Ort

open M_profile

module String_map = Map.Make(String)

type options = { profiles_left : Fileutils.file_path
	       ; profiles_right : Fileutils.file_path
	       ; nucmer_list : Fileutils.file_path list
	       ; out_delta : Fileutils.file_path
	       }

type gap_list = (M_range.t list * M_range.t list)

type gd_state = { ref_profile : M_profile.t
		; query_metaprofile : M_metaprofile.t
		; profile_gaps : gap_list
		; d_gaps : gap_list
		; ref_profile_pos : M_profile.profile_idx
		; query_metaprofile_pos : M_profile.profile_idx
		; d_profile_pos : M_profile.profile_idx
		; d_profile_end : M_profile.profile_idx
		}

let combinations l1 l2 =
  let rec inner o = function
    | [] ->
      [< >]
    | x::xs ->
      [< '(o, x)
      ;  inner o xs
      >]
  in
  let rec outter = function
    | [] ->
      [< >]
    | x::xs ->
      [< inner x l2
      ;  outter xs
      >]
  in
  outter l1

let gap_list_of_lists l1 l2 = (l1, l2)

let cons_gap_range strand gr (r_gaps, q_gaps) = 
  match strand with
    | `Ref -> (gr::r_gaps, q_gaps)
    | `Query -> (r_gaps, gr::q_gaps)

let next_nearest_gap ref_s query_s = function
  | ([], []) as gr ->
    (None, gr)
  | (rg::rgs, []) ->
    (Some (`Ref, rg), (rgs, []))
  | ([], qg::qgs) ->
    (Some (`Query, qg), ([], qgs))
  | ((rr::rs as rss), (qr::qs as qss)) ->
    let r_diff = abs (M_profile.int_of_profile_idx ref_s - M_range.get_start rr) in
    let q_diff = abs (M_profile.int_of_profile_idx query_s - M_range.get_start qr) in
    if r_diff <= q_diff then
      (Some (`Ref, rr), (rs, qss))
    else
      (Some (`Query, qr), (rss, qs))
  

let read_profile_set dir =
  let profile_name = Fileutils.join [dir; "profiles"] in
  let sin = Lazy_io.read_file_lines ~close:true (open_in profile_name) in
  let rec read_profiles acc sin =
    match M_profile.read_profile_file ~lite:true sin with
      | Some profile ->
	read_profiles (profile::acc) sin
      | None ->
	acc
  in
  read_profiles [] sin


let profile_map_of_profile_set =
  let add_to_list m p =
    if String_map.mem p.p_seq_name m then
      let l = String_map.find p.p_seq_name m in
      String_map.add p.p_seq_name (p::l) m
    else
      String_map.add p.p_seq_name [p] m
  in
  List.fold_left ~f:add_to_list ~init:String_map.empty

let string_of_strand = function
  | `Ref -> "Ref"
  | `Query -> "Query"

(*
 * Here we will take the following information
 * ref_profile - The entire profile for the reference (left)
 * query_metaprofile - The entire profile for the query (right)
 * profile_gaps - A gap_list value of the gaps from the profiles just in the subsection 
 * d_gaps - A gap_list value of the gaps from the delta alignment just in the subsection
 * ref_profile_pos - The current position in the ref profile
 * query_metaprofile_pos - The current position in the query profile
 * d_profile_pos - The current position in the ref delta alignment profile
 * d_profile_end - Where to end in the ref/query_metaprofile
 *)
let rec generate_delta_alignments delta_builder gd_state =
  let header gd_state = ((fst gd_state.ref_profile.p_name, gd_state.ref_profile.p_length),
			 (fst gd_state.query_metaprofile.M_metaprofile.profile.p_name, gd_state.query_metaprofile.M_metaprofile.profile.p_length))
  in
  let update_pos_by_d_gap_strand strand d_gr_diff gd_state =
    (let open M_profile in
	 match strand with
	   | `Ref ->
	     { gd_state with
	       ref_profile_pos = lift_profile_idx ~f:((+) (M_range.get_start d_gr_diff)) gd_state.ref_profile_pos;
	       query_metaprofile_pos = lift_profile_idx ~f:((+) (M_range.get_end d_gr_diff + 1)) gd_state.query_metaprofile_pos;
	       d_profile_pos = lift_profile_idx ~f:((+) (M_range.get_end d_gr_diff + 1)) gd_state.d_profile_pos;
	     }
	   | `Query ->
	     { gd_state with
	       ref_profile_pos = lift_profile_idx ~f:((+) (M_range.get_end d_gr_diff + 1)) gd_state.ref_profile_pos;
	       query_metaprofile_pos = lift_profile_idx ~f:((+) (M_range.get_start d_gr_diff)) gd_state.query_metaprofile_pos;
	       d_profile_pos = lift_profile_idx ~f:((+) (M_range.get_end d_gr_diff + 1)) gd_state.d_profile_pos;
	     })
  in
  let update_pos_by_p_gap_strand strand gr_diff gd_state =
    (let open M_profile in
	 match strand with
	   | `Ref ->
	     { gd_state with
	       ref_profile_pos = lift_profile_idx ~f:((+) (M_range.get_end gr_diff + 1)) gd_state.ref_profile_pos;
	       query_metaprofile_pos = lift_profile_idx ~f:((+) (M_range.get_start gr_diff)) gd_state.query_metaprofile_pos;
	       d_profile_pos = lift_profile_idx ~f:((+) (M_range.get_start gr_diff)) gd_state.d_profile_pos;
	     }
	   | `Query ->
	     { gd_state with
	       ref_profile_pos = lift_profile_idx ~f:((+) (M_range.get_start gr_diff)) gd_state.ref_profile_pos;
	       query_metaprofile_pos = lift_profile_idx ~f:((+) (M_range.get_end gr_diff + 1)) gd_state.query_metaprofile_pos;
	       d_profile_pos = lift_profile_idx ~f:((+) (M_range.get_start gr_diff)) gd_state.d_profile_pos;
	     })
  in
  let overlap_opposite_strand strand d_gr_diff profile_gaps gd_state =
    (* Get the strand and examine the first opposite gap, if it exists *)
    match strand with
      | `Ref -> begin
	match profile_gaps with
	  | (_, []) ->
	    false
	  | (_, qg::_) ->
	    let diff = abs (M_profile.int_of_profile_idx gd_state.query_metaprofile_pos - M_range.get_start qg) in
	    diff <= M_range.get_end d_gr_diff
      end
      | `Query -> begin
	match profile_gaps with
	  | ([], _) ->
	    false
	  | (rg::_, _) ->
	    let diff = abs (M_profile.int_of_profile_idx gd_state.ref_profile_pos - M_range.get_start rg) in
	    diff <= M_range.get_end d_gr_diff
      end
  in
  let get_gap_opposite_strand strand profile_gaps =
    match strand with
      | `Ref -> begin
	match profile_gaps with
	  | (_, []) ->
	    None
	  | (_, qg::_) ->
	    Some qg
      end
      | `Query -> begin
	match profile_gaps with
	  | ([], _) ->
	    None
	  | (rg::_, _) ->
	    Some rg
      end
  in
(*   Printf.eprintf "===\n"; *)
(*   Printf.eprintf "=== ref_profile_pos = %d\n" (M_profile.int_of_profile_idx gd_state.ref_profile_pos); *)
(*   Printf.eprintf "=== query_metaprofile_pos = %d\n" (M_profile.int_of_profile_idx gd_state.query_metaprofile_pos); *)
(*   Printf.eprintf "=== d_profile_pos = %d\n" (M_profile.int_of_profile_idx gd_state.d_profile_pos); *)
  match (next_nearest_gap 
	   gd_state.ref_profile_pos 
	   gd_state.query_metaprofile_pos 
	   gd_state.profile_gaps,
	 next_nearest_gap 
	   gd_state.d_profile_pos 
	   gd_state.d_profile_pos 
	   gd_state.d_gaps) with
    | ((None, _), (None, _)) -> begin
      (*
       * If there are no gaps on either side then the entire block can become an alignment
       * We only want to generate an alignment if our current position is not at the end
       * or if it is at the end we know that the start in the skeleton isn't where we are.
       * This is because it could be that we just had a gap go all the way to the end
       *)
(*       Printf.eprintf "- 1\n"; *)
      if gd_state.d_profile_pos <= gd_state.d_profile_end then
	let diff = M_profile.diff_profile_idx gd_state.d_profile_end gd_state.d_profile_pos + 1 in
	let db = M_delta_builder.add_offset ~offset:diff delta_builder in
	match M_delta_builder.to_delta db with
	  | Some d -> begin
(* 	    M_delta.print stderr d; *)
	    [< 'd >]
	  end
	  | None ->
	    [< >]
      else
	[< >]
    end
    | ((Some (strand, gr), profile_gaps), (None, _)) -> begin
      (*
       * We just have gaps left in the profiles we are producing alignments for.  In this case we have to
       * produce an alignment under some conditions.  Conditions where we do not want to produce
       * an alignment include:
       * 
       * 1 - We are at the start of a new alignment and the first thing we hit is a gap.  In this case
       *     we just want to advance on to creating the next alignment.
       * 2 - We have a situation where a d_gap spans multiple gaps.  For example:
       * 
       *     r |-----XX----XX------|
       *     q |-------------------|
       *    dr |----XXXXXXXXXX-----|
       *    dq |-------------------|
       * 
       *     In this case, the portion in r between the twp gaps is not actually a valid alignment because 
       *     the other strand is nothing but gaps.  That means we want to throw this alignment away
       *     as well and continue on to the next one.
       * 
       * Both of these situations are identifiable through the same method: the distance to the gap will
       * be 0 and one of the strands in the delta will have not moved at all.
       *)
      let gr_diff = 
	match strand with
	  | `Ref ->
	    M_range.lift ~f:(fun (s, e) -> (s - M_profile.int_of_profile_idx gd_state.ref_profile_pos, 
					    e - M_profile.int_of_profile_idx gd_state.ref_profile_pos)) gr
	  | `Query ->
	    M_range.lift ~f:(fun (s, e) -> (s - M_profile.int_of_profile_idx gd_state.query_metaprofile_pos, 
					    e - M_profile.int_of_profile_idx gd_state.query_metaprofile_pos)) gr
      in
(*       Printf.eprintf "- 2 - %s - (%d, %d) - (%d, %d) - just gr gap\n"  *)
(* 	(string_of_strand strand) *)
(* 	(M_range.get_start gr) *)
(* 	(M_range.get_end gr) *)
(* 	(M_range.get_start gr_diff) *)
(* 	(M_range.get_end gr_diff); *)
      let db = M_delta_builder.add_offset ~offset:(M_range.get_start gr_diff) delta_builder in
      let gd_state_new =
	{ update_pos_by_p_gap_strand strand gr_diff gd_state with
	  profile_gaps = profile_gaps;
	}
      in
      let db_new =
	M_delta_builder.create
	  ~sequences:("", "")
	  ~delta_type:`Nucmer
	  ~header:(header gd_state)
	  ~ref_start:gd_state_new.ref_profile_pos
	  ~query_metaprofile:gd_state.query_metaprofile
	  ~query_start:gd_state_new.query_metaprofile_pos
      in
      (*
       * Determine if we should generate this alignment or just move on to the next
       *)
      match M_delta_builder.to_delta db with
	| Some d -> begin
(* 	  M_delta.print stderr d; *)
	  [< 'd
	  ;  generate_delta_alignments db_new gd_state_new
	  >]
	end
	| None ->
	  generate_delta_alignments db_new gd_state_new
    end
    | ((None, _), (Some (d_strand, d_gr), d_gaps)) -> begin
      let d_gr_diff = M_range.lift ~f:(fun (s, e) -> (s - M_profile.int_of_profile_idx gd_state.d_profile_pos, 
						      e - M_profile.int_of_profile_idx gd_state.d_profile_pos)) d_gr 
      in
(*       Printf.eprintf "- 3\n"; *)
      let db_new = M_delta_builder.add_gap ~strand:d_strand ~diff:d_gr_diff delta_builder in
      let gd_state_new =
	{ update_pos_by_d_gap_strand d_strand d_gr_diff gd_state with
	  d_gaps = d_gaps;
	}
      in
      generate_delta_alignments db_new gd_state_new
    end
    | ((Some (strand, gr), profile_gaps), (Some (d_strand, d_gr), d_gaps)) -> begin
      let gr_diff = 
	match strand with
	  | `Ref ->
	    M_range.lift ~f:(fun (s, e) -> (s - M_profile.int_of_profile_idx gd_state.ref_profile_pos, 
					    e - M_profile.int_of_profile_idx gd_state.ref_profile_pos)) gr
	  | `Query ->
	    M_range.lift ~f:(fun (s, e) -> (s - M_profile.int_of_profile_idx gd_state.query_metaprofile_pos, 
					    e - M_profile.int_of_profile_idx gd_state.query_metaprofile_pos)) gr
      in
      let d_gr_diff = M_range.lift ~f:(fun (s, e) -> (s - M_profile.int_of_profile_idx gd_state.d_profile_pos, 
						      e - M_profile.int_of_profile_idx gd_state.d_profile_pos)) d_gr 
      in
      let diff = M_range.get_start gr_diff in
      let d_diff = M_range.get_start d_gr_diff in
      let d_diff_end = M_range.get_end d_gr_diff in
      (*
       * There are 2 situations here.  
       * 1 - One is that our gap starts before or at the same offset as
       *     the d_gap.
       * 2 - The d_gap comes before the gap, in that case we need to determine if the d_gap
       *     overlaps with the gap in any way, if so, generate an alignment and cut up the d_gap
       *     into the part we can handle and the part we need to put off until later.
       *)
      if diff <= d_diff then
	let db = M_delta_builder.add_offset ~offset:diff delta_builder in
(* 	Printf.eprintf "- 4 - %s - (%d, %d) - (%d, %d) - gr gap in front\n"  *)
(* 	  (string_of_strand strand) *)
(* 	  (M_range.get_start gr) *)
(* 	  (M_range.get_end gr) *)
(* 	  (M_range.get_start gr_diff) *)
(* 	  (M_range.get_end gr_diff); *)
	let gd_state_new =
	  { update_pos_by_p_gap_strand strand gr_diff gd_state with
	    profile_gaps = profile_gaps;
	  }
	in
	let db_new =
	  M_delta_builder.create
	    ~sequences:("", "")
	    ~delta_type:`Nucmer
	    ~header:(header gd_state)
	    ~ref_start:gd_state_new.ref_profile_pos
	    ~query_metaprofile:gd_state.query_metaprofile
	    ~query_start:gd_state_new.query_metaprofile_pos
	in
	match M_delta_builder.to_delta db with
	  | Some d -> begin
(* 	    M_delta.print stderr d; *)
	    [< 'd
	    ;  generate_delta_alignments db_new gd_state_new
	    >]
	  end
	  | None ->
	    generate_delta_alignments db_new gd_state_new
      else if d_diff_end < diff || (d_strand = strand && not (overlap_opposite_strand strand d_gr_diff profile_gaps gd_state)) then
	(*
	 * The entire d_gap is before the gap, we can handle this easily.
	 * See next else if for an explanation of overlap_opposite_strand
	 *)
	let db_new = M_delta_builder.add_gap ~strand:d_strand ~diff:d_gr_diff delta_builder in
(* 	Printf.eprintf "- 5 - %s - (%d, %d) - (%d, %d) - d gap in front\n"  *)
(* 	  (string_of_strand d_strand)  *)
(* 	  (M_range.get_start d_gr) *)
(* 	  (M_range.get_end d_gr) *)
(* 	  (M_range.get_start d_gr_diff) *)
(* 	  (M_range.get_end d_gr_diff); *)
(* 	Printf.eprintf "- %s gr = (%d, %d) - gr_diff = (%d, %d)\n" *)
(* 	  (string_of_strand strand) *)
(* 	  (M_range.get_start gr) *)
(* 	  (M_range.get_end gr) *)
(* 	  (M_range.get_start gr_diff) *)
(* 	  (M_range.get_end gr_diff); *)
	let gd_state_new =
	  { update_pos_by_d_gap_strand d_strand d_gr_diff gd_state with
	    d_gaps = d_gaps;
	  }
	in
	generate_delta_alignments db_new gd_state_new
      else if d_strand = strand then
	(*
	 * In this case the gr gap does not start infront of the d gap
	 * and the d gap does not finish before the gr gap.  We have some overlap
	 * between the gr gap and the d gap.  `overlap_opposite_strand` checks to see if
	 * our d gap is overlapping the gr gap on the opposite strand. If d_strand equals 
	 * strand then we know that the d gap does not end before the gr gap and that 
	 * we overlap the opposite strands gap. 
	 * We need to know this because when we add the d gap, we increment the profile 
	 * position on the strand opposite of the current gr gap by the total length of 
	 * the d gap.  Consider the situation below:
	 * 
	 * 
	 *     r |------XX-----------|
	 *     q |----------XX-------|
	 *    dr |----XXXXXXXXXX-----|
	 *    dq |-------------------|
	 * 
	 * In this case `strand` is r and `d_strand` is r.  If we blindly add the d gap, the following
	 * positions would look like:
	 * 
	 * 
	 *     r |------XX-----------|
	 *            ^
	 *     q |----------XX-------|
	 *                      ^
	 *    dr |----XXXXXXXXXX-----|
	 *                      ^
	 *    dq |-------------------|
	 *                      ^
	 * 
	 * We have steped over the gap on the q strand.  Instead what we will do is check to see if the current
	 * d gap overlaps the next gap on the opposing strand and, if so, break our d gap into 2 gaps.  At the
	 * end of this function we should look like:
	 * 
	 *     r |------XX-----------|
	 *            ^
	 *     q |----------XX-------|
	 *                  ^
	 *    dr |----XXXXXXXXXX-----|
	 *                  ^
	 *    dq |-------------------|
	 *                  ^
	 *)
	match get_gap_opposite_strand strand profile_gaps with
	  | Some gr_opp_gap ->
	    let gr_opp_diff =
	      match strand with
		| `Ref ->
		  M_range.lift ~f:(fun (s, e) -> (s - M_profile.int_of_profile_idx gd_state.query_metaprofile_pos, 
						  e - M_profile.int_of_profile_idx gd_state.query_metaprofile_pos)) gr_opp_gap
		| `Query ->
		  M_range.lift ~f:(fun (s, e) -> (s - M_profile.int_of_profile_idx gd_state.ref_profile_pos, 
						  e - M_profile.int_of_profile_idx gd_state.ref_profile_pos)) gr_opp_gap
	    in
	    let diff_to_opp_gap = M_range.get_start gr_opp_diff - d_diff in
	    let d_gr_diff_split = M_range.of_tuple (d_diff, d_diff + diff_to_opp_gap - 1) in
	    let db_new = M_delta_builder.add_gap ~strand:d_strand ~diff:d_gr_diff_split delta_builder in
	    let d_gr_back = M_range.lift ~f:(fun (s, e) -> (s + diff_to_opp_gap, e)) d_gr in
(* 	    Printf.eprintf "- 6\n"; *)
(* 	    Printf.eprintf "diff_to_opp_gap = %d\n" diff_to_opp_gap; *)
(* 	    Printf.eprintf "gr = %s - (%d, %d)\n" (string_of_strand strand) (M_range.get_start gr) (M_range.get_end gr); *)
(* 	    Printf.eprintf "d_gr = %s - (%d, %d)\n" (string_of_strand d_strand) (M_range.get_start d_gr) (M_range.get_end d_gr); *)
(* 	    Printf.eprintf "gr_opp_gap = (%d, %d)\n" (M_range.get_start gr_opp_gap) (M_range.get_end gr_opp_gap); *)
(* 	    Printf.eprintf "gr_diff = (%d, %d)\n" (M_range.get_start gr_diff) (M_range.get_end gr_diff); *)
(* 	    Printf.eprintf "d_gr_diff = (%d, %d)\n" (M_range.get_start d_gr_diff) (M_range.get_end d_gr_diff); *)
(* 	    Printf.eprintf "gr_opp_diff = (%d, %d)\n" (M_range.get_start gr_opp_diff) (M_range.get_end gr_opp_diff); *)
(* 	    Printf.eprintf "d_gr_diff = (%d, %d)\n" (M_range.get_start d_gr_diff) (M_range.get_end d_gr_diff); *)
(* 	    Printf.eprintf "d_gr_back = (%d, %d)\n" (M_range.get_start d_gr_back) (M_range.get_end d_gr_back); *)
	    let gd_state_new =
	      { update_pos_by_d_gap_strand d_strand d_gr_diff_split gd_state with
		d_gaps = cons_gap_range d_strand d_gr_back d_gaps
	      }
	    in
	    generate_delta_alignments db_new gd_state_new
	  | None ->
	    raise (Failure "Impossible situation")
      else
	(*
	 * The gap isn't before d_gap and d_gap isn't entirely before gap and the
	 * gaps aren't on the same strand.  This means that the d_gap overlaps
	 * the gap.  In this case we have to break up the d_gap, put part of it
	 * back into its gap list and then process what we can
	 *)
	let diff_to_gap = diff - d_diff in
(* 	Printf.eprintf "- 7\n"; *)
	let d_gr_diff_split = M_range.of_tuple (d_diff, d_diff + diff_to_gap - 1) in
	let db_new = M_delta_builder.add_gap ~strand:d_strand ~diff:d_gr_diff_split delta_builder in
	let d_gr_back = M_range.lift ~f:(fun (s, e) -> (s + diff_to_gap, e)) d_gr in
(* 	Printf.eprintf "gr = (%d, %d)\n" (M_range.get_start gr) (M_range.get_end gr); *)
(* 	Printf.eprintf "d_gr = (%d, %d)\n" (M_range.get_start d_gr) (M_range.get_end d_gr); *)
(* 	Printf.eprintf "gr_diff = (%d, %d)\n" (M_range.get_start gr_diff) (M_range.get_end gr_diff); *)
(* 	Printf.eprintf "d_gr_diff = (%d, %d)\n" (M_range.get_start d_gr_diff) (M_range.get_end d_gr_diff); *)
(* 	Printf.eprintf "diff_to_gap = %d\n" diff_to_gap; *)
(* 	Printf.eprintf "d_gr_diff = (%d, %d)\n" (M_range.get_start d_gr_diff) (M_range.get_end d_gr_diff); *)
(* 	Printf.eprintf "d_gr_back = (%d, %d)\n" (M_range.get_start d_gr_back) (M_range.get_end d_gr_back); *)
	let gd_state_new =
	  { update_pos_by_d_gap_strand d_strand d_gr_diff_split gd_state with
	    d_gaps = cons_gap_range d_strand d_gr_back d_gaps
	  }
	in
	generate_delta_alignments db_new gd_state_new
    end


let generate_delta d (ref_profile, (ref_seq_s, ref_seq_e)) (query_profile, (query_seq_s, query_seq_e)) =
  (*
   * Here we know that the *_profile and d_*_profile overlap.  Now we need to make sure that
   * the alignment still overlaps in those subsets that the *_profile portions overlap.
   * To do this we take the sequence index that we got in determining they overlap and get the
   * corresponding profile index in the d_*_profile.  We then look to see if these profile indecies
   * still overlap.
   *)
  let d_ref_profile = M_delta.ref_profile_of_delta d in
  let d_query_profile = M_delta.query_profile_of_delta d in
  let ref_profile_s = M_profile.profile_idx_of_seq_idx d_ref_profile ref_seq_s in
  let ref_profile_e = M_profile.profile_idx_of_seq_idx d_ref_profile ref_seq_e in
  let query_profile_s = M_profile.profile_idx_of_seq_idx d_query_profile query_seq_s in
  let query_profile_e = M_profile.profile_idx_of_seq_idx d_query_profile query_seq_e in

  let d_ref_range = M_range.of_tuple (M_profile.int_of_profile_idx ref_profile_s, M_profile.int_of_profile_idx ref_profile_e) in
  let d_query_range = M_range.of_tuple (M_profile.int_of_profile_idx query_profile_s, M_profile.int_of_profile_idx query_profile_e) in

  match M_range.overlap d_ref_range d_query_range with
    | Some overlap -> begin
      (*
       * Now we know that the coordinates in the d_*_profile that match the corresponding
       * *_profile still overlap.
       * 
       * Now we need to pull out the subsections of the *_profile and d_*_profile's so we can
       * get access to the gaps.
       * 
       * `overlap` start and end are profile indecies into d_*_profile
       *)
      let profiles = 
	(* parens here because taureg mode doesn't know how to indent let open, need to fix *)
	(let open Core.Option.Monad_infix in
	    M_profile.subset_profile
	      d_ref_profile
	      ((M_profile.profile_idx_of_int (M_range.get_start overlap)),
	       (M_profile.profile_idx_of_int (M_range.get_end overlap)))
	    >>= (fun d_ref_profile_sub ->
	    M_profile.subset_profile
	      d_query_profile
	      ((M_profile.profile_idx_of_int (M_range.get_start overlap)),
	       (M_profile.profile_idx_of_int (M_range.get_end overlap)))
	    >>= (fun d_query_profile_sub ->
	      let ref_profile_sub =
		M_profile.subset_seq
		  ref_profile
		  ((M_profile.seq_idx_of_int (M_range.get_start d_ref_profile_sub.p_range)),
		   (M_profile.seq_idx_of_int (M_range.get_end d_ref_profile_sub.p_range)))
	      in
	      let query_profile_sub =
		M_profile.subset_seq
		  query_profile
		  ((M_profile.seq_idx_of_int (M_range.get_start d_query_profile_sub.p_range)),
		   (M_profile.seq_idx_of_int (M_range.get_end d_query_profile_sub.p_range)))
	      in
	      Core.Option.return (ref_profile_sub, query_profile_sub, d_ref_profile_sub, d_query_profile_sub))))
      in
      match profiles with
	| Some (ref_profile_sub, query_profile_sub, d_ref_profile_sub, d_query_profile_sub) -> begin
	  (*
	   * By this point we have a subsets of *_profile and d_*_profile that match the overlapping
	   * coordinates.  We only need these subsets for their gaps because we are going to walk the
	   * gaps when creating the delta alignments.
	   * 
	   * We are wrapping the query_profile up in a metaprofile because we want to index it
	   * where we are always adding values to it, but in reality we might be subtracting
	   * from the actual profile value.
	   *)
	  let query_metaprofile =
	    if M_range.get_direction query_profile.p_range <> M_range.get_direction d_query_profile.p_range then
	      M_metaprofile.reverse (M_metaprofile.metaprofile_of_profile query_profile)
	    else
	      M_metaprofile.metaprofile_of_profile query_profile
	  in
	  let query_profile_gaps = 
	    if M_metaprofile.is_reverse query_metaprofile then
	      let conv_int = M_profile.int_of_profile_idx $ M_metaprofile.profile_idx_of_profile_idx query_metaprofile $ M_profile.profile_idx_of_int in
	      let conv_range = M_range.lift ~f:(fun (s, e) -> (conv_int e, conv_int s)) in
(* 	      Printf.eprintf "reversed_gaps = \n"; *)
(* 	      List.iter ~f:(fun g -> Printf.eprintf "(%d, %d) " (M_range.get_start g) (M_range.get_end g)) (List.rev (List.map ~f:conv_range query_profile_sub.p_gaps)); *)
(* 	      Printf.eprintf "\n"; *)
	      List.rev (List.map ~f:conv_range query_profile_sub.p_gaps)
	    else
	      query_profile_sub.p_gaps
	  in
	  let module Mp = M_profile in
	  (*
	   * I was a big foolish and I didn't make it so the subsetting functions for profiles returned a true
	   * profile, the gaps are translated as they should be.  In reality we are only subsetting so we can
	   * get the gaps in the range of the subset, because we need to translate back to the original profile
	   * anyways.  So here we find where the subset starts and then translate that back into the profile
	   *)
	  let ref_start = 
	    Mp.profile_idx_of_seq_idx 
	      ref_profile 
	      (Mp.seq_idx_of_int (M_range.get_start ref_profile_sub.p_range))
	  in
	  let query_start = 
	    if M_metaprofile.is_reverse query_metaprofile then
	      M_metaprofile.profile_idx_of_profile_idx
		query_metaprofile
		(Mp.profile_idx_of_seq_idx 
		   query_profile 
		   (Mp.seq_idx_of_int (M_range.get_end query_profile_sub.p_range)))
	    else
	      (Mp.profile_idx_of_seq_idx 
		 query_profile 
		 (Mp.seq_idx_of_int (M_range.get_start query_profile_sub.p_range)))
	  in
(* 	  Printf.eprintf "\n\n"; *)
(* 	  Printf.eprintf "- overlap = (%d, %d)\n\n" (M_range.get_start overlap) (M_range.get_end overlap); *)
(* 	  Printf.eprintf "d - \n"; *)
(* 	  M_delta.print stderr d; *)
(* 	  Printf.eprintf "\n"; *)
(* 	  Printf.eprintf "ref_profile - \n"; *)
(* 	  M_profile.print stderr ref_profile; *)
(* 	  Printf.eprintf "\n"; *)
(* 	  Printf.eprintf "query_profile - \n"; *)
(* 	  M_profile.print stderr query_profile; *)
(* 	  Printf.eprintf "\n"; *)
(* 	  Printf.eprintf "- ref_profile_sub =\n"; *)
(* 	  M_profile.print stderr ref_profile_sub; *)
(* 	  Printf.eprintf "\n"; *)
(* 	  Printf.eprintf "- query_profile_sub =\n"; *)
(* 	  M_profile.print stderr query_profile_sub; *)
(* 	  Printf.eprintf "\n"; *)
(* 	  Printf.eprintf "- d_ref_profile_sub =\n"; *)
(* 	  M_profile.print stderr d_ref_profile_sub; *)
(* 	  Printf.eprintf "\n"; *)
(* 	  Printf.eprintf "- d_query_profile_sub =\n"; *)
(* 	  M_profile.print stderr d_query_profile_sub; *)
(* 	  Printf.eprintf "\n\n"; *)
(* 	  Printf.eprintf "- ref_start = %d\n" (M_profile.int_of_profile_idx ref_start); *)
(* 	  Printf.eprintf "- query_start = %d\n" (M_profile.int_of_profile_idx query_start); *)
(* 	  Printf.eprintf "- d_profile_end = %d\n" (M_range.length overlap); *)
	  generate_delta_alignments
	    (M_delta_builder.create
	       ~sequences:("", "")
	       ~delta_type:`Nucmer
	       ~header:((fst ref_profile.p_name, ref_profile.p_length),
			(fst query_metaprofile.M_metaprofile.profile.p_name, query_metaprofile.M_metaprofile.profile.p_length))
	       ~ref_start:ref_start
	       ~query_metaprofile:query_metaprofile
	       ~query_start:query_start)
	    { ref_profile = ref_profile
	    ; query_metaprofile = query_metaprofile
	    ; profile_gaps = gap_list_of_lists ref_profile_sub.p_gaps query_profile_gaps
	    ; d_gaps = gap_list_of_lists d_ref_profile_sub.p_gaps d_query_profile_sub.p_gaps
	    ; ref_profile_pos = ref_start
	    ; query_metaprofile_pos = query_start
	    ; d_profile_pos = Mp.profile_idx_of_int (M_range.get_start overlap)
	    ; d_profile_end = Mp.profile_idx_of_int (M_range.get_end overlap)
	    }
	end
	| None -> begin
	  Printf.eprintf "Some how we got an empty profile\n";
	  [< >]
	end
    end
    | None ->
      [< >]



let rec translate_combinations d combos =
  match Seq.next combos with
    | Some (l, r) -> begin
      match (M_range.overlap d.M_delta.ref_range l.p_range, M_range.overlap d.M_delta.query_range r.p_range) with
	| (Some ref_overlap, Some query_overlap) -> begin
	  let ref_seq_s = M_profile.seq_idx_of_int (M_range.get_start ref_overlap) in
	  let ref_seq_e = M_profile.seq_idx_of_int (M_range.get_end ref_overlap) in
	  let query_seq_s = M_profile.seq_idx_of_int (M_range.get_start query_overlap) in
	  let query_seq_e = M_profile.seq_idx_of_int (M_range.get_end query_overlap) in
	  (* 
	   * Make sure the l profile (reference) is going the same direction as the ref in d.
	   * If not, reverse the delta so the delta we produce will be going in the right
	   * direction.
	   * 
	   * Passing the reversed d to the enxt iteration is what we want because the l
	   * profile will be the same for the entire r iteration
	   *)
	  let d = 
	    if M_range.get_direction l.p_range <> M_range.get_direction d.M_delta.ref_range then begin
(* 	      Printf.eprintf "-------\nReversing\n"; *)
(* 	      M_delta.print stderr d; *)
(* 	      Printf.eprintf "-------\n"; *)
(* 	      M_delta.print stderr (M_delta.reverse d); *)
(* 	      Printf.eprintf "-------\n"; *)
(* 	      M_delta.print stderr (M_delta.reverse (M_delta.reverse d)); *)
(* 	      Printf.eprintf "-------\n\n"; *)
	      M_delta.reverse d
	    end
	    else
	      d
	  in
	  [< generate_delta d (l, (ref_seq_s, ref_seq_e)) (r, (query_seq_s, query_seq_e))
	  ;  translate_combinations d combos
	  >]
	end
	| _ ->
	  (* At least one does not overlap *)
	  translate_combinations d combos
    end
    | None ->
      [< >]

let rec translate_delta left_map right_map sin =
  let translate_delta_by_profiles d =
    let pull_apart_name n =
      match String.split_on_chars ~on:[':'] n with
	| genome::species::_ ->
	  genome ^ "." ^ species
	| _ ->
	  n
    in
    let ((ref_seq, _), (query_seq, _)) = d.M_delta.header in
    let ref_seq = pull_apart_name ref_seq in
    let query_seq = pull_apart_name query_seq in
    try
      let ref_profiles = String_map.find ref_seq left_map in
      let query_profiles = String_map.find query_seq right_map in
      let combos = combinations ref_profiles query_profiles in
      translate_combinations d combos
    with 
      | Not_found -> begin
	Printf.eprintf "Not_found (%s, %s)\n" ref_seq query_seq;
	[< >]
      end
  in
  match Seq.next sin with
    | None ->
      [< >]
    | Some d ->
      [< translate_delta_by_profiles d
      ;  translate_delta left_map right_map sin
      >]


let translate_nucmer left_map right_map nucmer_list =
  let rec delta_stream = function
    | [] ->
      [< >]
    | n::ns ->
      [< M_delta.parse_delta_file n
      ;  delta_stream ns
      >]
  in
  nucmer_list |> delta_stream |> translate_delta left_map right_map

let translate ~left_dir ~right_dir ~nucmer_list =
  let left_profile_set = read_profile_set left_dir in
  let right_profile_set = read_profile_set right_dir in
  let left_profile_map = profile_map_of_profile_set left_profile_set in
  let right_profile_map = profile_map_of_profile_set right_profile_set in
  translate_nucmer left_profile_map right_profile_map nucmer_list


let string_stream_of_delta d =
  let gap_conv = d |> M_delta.deltas_of_gaps |> List.map ~f:string_of_int |> Seq.of_list in
  [< 'Printf.sprintf "%d %d %d %d 1 2 3" 
         (M_range.get_start d.M_delta.ref_range)
	 (M_range.get_end d.M_delta.ref_range)
	 (M_range.get_start d.M_delta.query_range)
	 (M_range.get_end d.M_delta.query_range)
  ;  gap_conv
  >]

let string_stream_of_delta_stream (s1, s2, dt) sin =
  let rec process_deltas prev_header sin =
      match Seq.next sin with
	| Some d when d.M_delta.header = prev_header ->
	  [< string_stream_of_delta d
	  ;  process_deltas d.M_delta.header sin
	  >]
	| Some d ->
	  let ((g1, l1), (g2, l2)) = d.M_delta.header in
	  [< 'Printf.sprintf ">%s %s %d %d" g1 g2 l1 l2
	  ;  string_stream_of_delta d
	  ;  process_deltas d.M_delta.header sin
	  >]
	| None ->
	  [< >]
  in    
  let first_delta sin =
    match Seq.next sin with
      | Some d ->
	let ((g1, l1), (g2, l2)) = d.M_delta.header in
	[< 'Printf.sprintf ">%s %s %d %d" g1 g2 l1 l2
	;  string_stream_of_delta d
	;  process_deltas d.M_delta.header sin
	>]
      | None ->
	[< >]
  in
  let dts = match dt with | `Promer -> "PROMER" | `Nucmer -> "NUCMER" in
  [< 'Printf.sprintf "%s %s" s1 s2
  ;  'dts
  ;  first_delta sin
  >]


let usage = ""

let parse_argv argv =
  let profiles_left = ref "" in
  let profiles_right = ref "" in
  let nucmer_list = ref "" in
  let out_delta = ref "" in
  
  let params = 
    Arg.align [ ("-profiles_left", Arg.Set_string profiles_left, "Path Path to directory containing left profiles")
	      ; ("-profiles_right", Arg.Set_string profiles_right, "Path Path to directory containing right profiles")
	      ; ("-nucmer_list", Arg.Set_string nucmer_list, "Path Path to file containing list of nucmer files")
	      ; ("-out_delta", Arg.Set_string out_delta, "Path Path to detla file to create")
	      ]
  in
  Arg.parse params (fun _ -> ()) usage;
  if !profiles_left = "" then
    raise (Failure "Must provide -profiles_left")
  else if !profiles_right = "" then
    raise (Failure "Must provide -profiles_right")
  else if !nucmer_list = "" then
    raise (Failure "Must provide -nucmer_list")
  else if !out_delta = "" then
    raise (Failure "Must provide -out_delta")
  else
    { profiles_right = !profiles_right
    ; profiles_left = !profiles_left
    ; nucmer_list = open_in !nucmer_list |> Lazy_io.read_file_lines ~close:true |> Seq.to_list
    ; out_delta = !out_delta
    }

let main argv =
  let options = parse_argv argv in
  Shell.mkdir_p (Fileutils.dirname options.out_delta);
  let delta_stream = 
    translate 
      ~left_dir:options.profiles_left 
      ~right_dir:options.profiles_right 
      ~nucmer_list:options.nucmer_list
  in
  let rec print_lines fout sin =
    match Seq.next sin with
      | Some l -> begin
	output_string fout l;
	output_char fout '\n';
	print_lines fout sin
      end
      | None ->
	()
  in
  let left_fasta = 
    options.profiles_left 
    |> Sys.readdir 
    |> Array.to_list 
    |> List.filter ~f:(String.is_suffix ~suffix:".fasta") 
    |> List.map ~f:(fun f -> Fileutils.join [options.profiles_left; f])
    |> List.hd
  in
  let right_fasta =
    options.profiles_right
    |> Sys.readdir 
    |> Array.to_list 
    |> List.filter ~f:(String.is_suffix ~suffix:".fasta") 
    |> List.map ~f:(fun f -> Fileutils.join [options.profiles_right; f])
    |> List.hd
  in
  match (left_fasta, right_fasta) with
    | (Some left_seq, Some right_seq) -> begin
      let fout = open_out options.out_delta in
      string_stream_of_delta_stream (left_seq, right_seq, `Nucmer) delta_stream |> print_lines fout;
      close_out fout
    end
    | _ ->
      raise (Failure "Fasta files do not exist")

