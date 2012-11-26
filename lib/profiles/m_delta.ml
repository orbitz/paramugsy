(*pp camlp4o *)
open Core_extended.Std
open Ort
open Ort.Function

type t = { sequences : (Ort.Fileutils.file_path * Ort.Fileutils.file_path)
	 ; delta_type : [ `Promer | `Nucmer ]
	 ; header : ((string * int) * (string * int))
	 ; ref_range : M_range.t
	 ; query_range : M_range.t
	 ; ref_gaps : M_range.t list
	 ; query_gaps : M_range.t list
	 }

let ($) g h x = g (h x)

let parse_gaps gaps =
  let rec squeeze_gaps' last = function
    | [] ->
      [(last, last)]
    | x::xs when x - last = 1 ->
      gap_run last x xs
    | x::xs ->
      (last, last)::(squeeze_gaps' x xs)
  and gap_run last curr = function
    | [] -> [(last, curr)]
    | x::xs when x - curr = 1 ->
      gap_run last x xs
    | x::xs ->
      (last, curr)::(squeeze_gaps' x xs)
  in
  let squeeze_gaps = function
    | [] -> []
    | x::xs -> squeeze_gaps' x xs
  in
  let negate_if x y = if x < 0 then -y else y
  in
  let rec positions_of_offsets accum offset = function
    | x::xs ->
      positions_of_offsets ((negate_if x (abs x + offset))::accum) (abs x + offset) xs
    | [] ->
      List.rev accum
  in
  let gap_range_of_ref = squeeze_gaps $ List.map ~f:(fun x -> -x) $ List.filter ~f:(fun x -> x < 0)
  in
  let gap_range_of_query = squeeze_gaps $ List.filter ~f:(fun x -> x > 0)
  in
  let gap_positions = positions_of_offsets [] 0 gaps in
  (gap_range_of_ref gap_positions |> List.map ~f:M_range.of_tuple,
   gap_range_of_query gap_positions |> List.map ~f:M_range.of_tuple)

let parse_delta_stream sin =
  let rec read_sequence_names sin =
    match Seq.next sin with
      | Some l -> begin
	match String.rsplit2 ~on:' ' l with
	  | Some spl ->
	    read_delta_type spl sin
	  | None ->
	    raise (Failure ("Expecting sequences: " ^ l))
      end
      | None ->
	raise (Failure ("Expecting sequences"))
  and read_delta_type sequences sin =
    match Seq.next sin with
      | Some "NUCMER" ->
	read_header (sequences, `Nucmer) sin
      | Some "PROMER" ->
	read_header (sequences, `Promer) sin
      | Some l ->
	raise (Failure ("Unknown delta type: " ^ l))
      | None ->
	raise (Failure "Expecting delta type")
  and read_header (sequences, delta_type) sin =
    match Seq.next sin with
      | Some l when l.[0] = '>' -> begin
	match String.split_on_chars ~on:[' '] (String.sub ~pos:1 ~len:(String.length l - 1) l) with
	  | [s1; s2; l1; l2] ->
	    read_alignment (sequences, delta_type, ((s1, int_of_string l1), (s2, int_of_string l2))) sin
	  | _ ->
	    raise (Failure ("Expecting header: " ^ l))
      end
      | Some l ->
	raise (Failure ("Unknown line: " ^ l))
      | None ->
	[< >]
  and read_alignment (sequences, delta_type, header) sin =
    match Seq.next sin with
      | Some l -> begin
	match String.split_on_chars ~on:[' '] l with
	  | [r_start; r_end; q_start; q_end; e1; e2; e3] ->
	    read_gaps
	      (sequences,
	       delta_type,
	       header,
	       M_range.of_tuple (int_of_string r_start, int_of_string r_end),
	       M_range.of_tuple (int_of_string q_start, int_of_string q_end))
	      sin
	  | _ ->
	    raise (Failure ("Unknown alignment line: " ^ l))
      end
      | None ->
	raise (Failure "Expecting alignment")
  and read_gaps (sequences, delta_type, header, ref_range, query_range) sin =
    let (ref_gaps, query_gaps) = gaps_of_delta ref_range query_range sin in
    [< '{ sequences = sequences
	; delta_type = delta_type
	; header = header
	; ref_range = ref_range
	; query_range = query_range
	; ref_gaps = ref_gaps
	; query_gaps = query_gaps
	}
    ;  read_alignment_or_header (sequences, delta_type, header) sin
    >]
  and gaps_of_delta ref_range query_range sin =
    let gaps = Seq.take_while ~f:(fun x -> x <> "0") sin |> Seq.to_list |> List.map ~f:int_of_string in
    match Seq.next sin with
      | Some "0" ->
	parse_gaps gaps
      | Some l ->
	raise (Failure ("Unknown line parsing delta: " ^ l))
      | None ->
	raise (Failure "Expected 0 got EOF")
  and read_alignment_or_header (sequences, delta_type, header) sin =
    match Seq.next sin with
      | Some l when l.[0] = '>' -> begin
	match String.split_on_chars ~on:[' '] (String.sub ~pos:1 ~len:(String.length l - 1) l) with
	  | [s1; s2; l1; l2] ->
	    read_alignment (sequences, delta_type, ((s1, int_of_string l1), (s2, int_of_string l2))) sin
	  | _ ->
	    raise (Failure ("Expecting header: " ^ l))
      end
      | Some l -> begin
	match String.split_on_chars ~on:[' '] l with
	  | [r_start; r_end; q_start; q_end; e1; e2; e3] ->
	    read_gaps
	      (sequences,
	       delta_type,
	       header,
	       M_range.of_tuple (int_of_string r_start, int_of_string r_end),
	       M_range.of_tuple (int_of_string q_start, int_of_string q_end))
	      sin
	  | _ ->
	    raise (Failure ("Unknown alignment line: " ^ l))
      end
      | None ->
	[< >]
  in
  read_sequence_names sin

let parse_delta_file fname =
  parse_delta_stream (Lazy_io.read_file_lines ~close:true (open_in fname))


let profile_of_delta range gaps =
  let sum = List.fold_left ~f:(+) ~init:0 in
  let length = (gaps |> List.map ~f:M_range.length |> sum) + M_range.length range in
  { M_profile.p_name = ("", "")
  ; M_profile.p_seq_name = ""
  ; M_profile.p_range = range
  ; M_profile.p_length = length
  ; M_profile.p_gaps = gaps
  ; M_profile.p_src_size = length
  ; M_profile.p_seq_text = ""
  }

let ref_profile_of_delta d =
  profile_of_delta d.ref_range d.ref_gaps

let query_profile_of_delta d =
  profile_of_delta d.query_range d.query_gaps


let deltas_of_gaps d =
  let rec ones = function
    | 0 ->
      []
    | n ->
      (1)::ones (n - 1)
  in
  let negative = List.map ~f:(fun x -> -x) in
  let rec delta_of_gaps' pos  = function
    | ([], []) ->
      [0]
    | (rg::rgs, []) ->
      (-(M_range.get_start rg - pos))::(negative (ones (M_range.length rg - 1))) @ (delta_of_gaps' (M_range.get_end rg) (rgs, []))
    | ([], qg::qgs) ->
      (M_range.get_start qg - pos)::(ones (M_range.length qg - 1)) @ (delta_of_gaps' (M_range.get_end qg) ([], qgs))
    | (rg::rgs, (qg::_ as qgss)) when M_range.get_start rg < M_range.get_start qg ->
      (-(M_range.get_start rg - pos))::(negative (ones (M_range.length rg - 1))) @ (delta_of_gaps' (M_range.get_end rg) (rgs, qgss))
    | ((rg::_ as rgss), qg::qgs)  ->
      (M_range.get_start qg - pos)::(ones (M_range.length qg - 1)) @ (delta_of_gaps' (M_range.get_end qg) (rgss, qgs))
  in
  delta_of_gaps' 0 (d.ref_gaps, d.query_gaps)

(*
 * Reverses the orientation of the delta
 *)
let reverse d =
  let rev_ref_profile = M_profile.reverse (ref_profile_of_delta d) in
  let rev_query_profile = M_profile.reverse (query_profile_of_delta d) in
  (let open M_profile in
       { d with
	 ref_range = rev_ref_profile.p_range;
	 query_range = rev_query_profile.p_range;
	 ref_gaps = rev_ref_profile.p_gaps;
	 query_gaps = rev_query_profile.p_gaps;
       })


let print out_channel d =
  Printf.fprintf out_channel "sequences = (%s, %s)\n" (fst d.sequences) (snd d.sequences);
  Printf.fprintf out_channel "header = ((%s, %d), (%s, %d))\n"
    (fst (fst d.header))
    (snd (fst d.header))
    (fst (snd d.header))
    (snd (snd d.header));
  Printf.fprintf out_channel "ref_range = (%d, %d)\n" (M_range.get_start d.ref_range) (M_range.get_end d.ref_range);
  Printf.fprintf out_channel "query_range = (%d, %d)\n" (M_range.get_start d.query_range) (M_range.get_end d.query_range);
  Printf.fprintf out_channel "ref_gaps =\n";
  List.iter ~f:(fun r -> Printf.fprintf out_channel "(%d, %d) " (M_range.get_start r) (M_range.get_end r)) d.ref_gaps;
  Printf.fprintf out_channel "\n";
  Printf.fprintf out_channel "query_gaps =\n";
  List.iter ~f:(fun r -> Printf.fprintf out_channel "(%d, %d) " (M_range.get_start r) (M_range.get_end r)) d.query_gaps;
  Printf.fprintf out_channel "\n"
