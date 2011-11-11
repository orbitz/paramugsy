open Core_extended.Std
open Ort
open Ort.Function

module Option_monad = Core.Monad.Make(Core.Option)

type profile_name = (string * string)

type seq_idx = int
type profile_idx = int

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

(*
 * Takes a string of text where '-' represents gaps and
 * produces a list of M_range.t representing the gaps.
 *)
let gaps_of_text text =
  let length = String.length text in
  let rec find_first_gap idx =
    if idx >= length then
      []
    else if text.[idx] = '-' then
      expand_gap idx
    else
      find_first_gap (idx + 1)
  and expand_gap idx =
    let eidx = find_end_gap (idx + 1) in
    (M_range.of_tuple (idx + 1, eidx))::(find_first_gap eidx)
  and find_end_gap idx =
    if idx < length && text.[idx] = '-' then
      find_end_gap (idx + 1)
    else
      idx
  in
  find_first_gap 0

(*
 * Gaps are always relative to profile indecies NOT sequence. That means
 * gaps will always be in ascending index order
 *)
let profile_of_maf_entry ~name ~seq_name ~range ~src_size ~text =
  { p_name = name
  ; p_seq_name = seq_name
  ; p_range = range
  ; p_length = String.length text
  ; p_gaps = gaps_of_text text
  ; p_src_size = src_size
  ; p_seq_text = text
  }

(*
 * Reads a profile from a file.  if lite is given than the sequence text is not
 * read.
 * 
 * NOTE: There is a known bug that does not close the input file on failure
 *)
let read_profile_file ?(lite = false) sin =
  let range_of_string str =
    match String.lsplit2 ~on:' ' str with
      | Some (i1, i2) ->
	M_range.of_tuple (int_of_string i1, int_of_string i2)
      | None ->
	raise (Failure ("Invalid string reading profile index: " ^ str))
  in
  match Seq.next sin with
    | Some l -> begin
      match String.split_on_chars ~on:[' '] l with
	| [major_name; minor_name; seq_name; seq_start; seq_end; length; src_size] ->
	  let until_zero = function 
	    | "0" -> false 
	    | _ -> true
	  in
	  let sequence_gaps = 
	    sin |> 
		Seq.take_while ~f:until_zero |> 
		    Seq.map ~f:range_of_string |> 
			Seq.to_list
	  in
	  (* read the 0 *)
	  let _ = Seq.next sin in
	  let text =
	    if lite then
	      (*
	       * Even though we're lite we have to read in the line of text so the next
	       * profile can be read.  Unfortunately there isn't a nice way to do this
	       * with the stream API that allows us to not allocate the memory for it
	       *)
	      let _ = Seq.next sin in
	      ""
	    else
	      match Seq.next sin with
		| Some text -> String.strip text
		| None -> raise (Failure "Early end of file")
	  in
	  Some { p_name = (major_name, minor_name)
	       ; p_seq_name = seq_name
	       ; p_range = M_range.of_tuple (int_of_string seq_start, int_of_string seq_end)
	       ; p_length = int_of_string length
	       ; p_gaps = sequence_gaps
	       ; p_src_size = int_of_string src_size
	       ; p_seq_text = text
	       }
	| _ ->
	  raise (Failure (Printf.sprintf "Error reading profile index file line %s" l))
    end
    | None ->
      None
	    

let write_profile_file profile fout =
  let (major_name, minor_name) = profile.p_name in
  Printf.fprintf fout
    "%s %s %s %d %d %d %d\n"
    major_name
    minor_name
    profile.p_seq_name 
    (M_range.get_start profile.p_range)
    (M_range.get_end profile.p_range)
    profile.p_length
    profile.p_src_size;
  List.iter ~f:(fun r -> Printf.fprintf fout "%d %d\n" (M_range.get_start r) (M_range.get_end r)) profile.p_gaps;
  Printf.fprintf fout "0\n";
  Printf.fprintf fout "%s\n" profile.p_seq_text

let seq_idx_of_int i = if i > 0 then i else raise (Seq_idx_invalid i)
let profile_idx_of_int i = if i > 0 then i else raise (Profile_idx_invalid i)

let int_of_seq_idx si = si
let int_of_profile_idx pi = pi

let lift_seq_idx ~f si = seq_idx_of_int (f si)
let lift_profile_idx ~f pi = profile_idx_of_int (f pi)

let profile_idx_of_seq_idx p si =
  let offset = abs (M_range.get_start p.p_range - si) + 1 in
  let rec profile_index gaps = function
    | gr::grs when M_range.get_start gr <= (offset + gaps) ->
      profile_index (gaps + M_range.length gr) grs
    | _::_ | [] -> 
      gaps + offset
  in
  if M_range.contains p.p_range si then
    profile_index 0 p.p_gaps
  else
    raise (Seq_idx_out_of_range (si, p.p_range))

let seq_idx_of_profile_idx p pi = 
  let rec seq_index gaps = function
    | gr::grs when M_range.get_end gr < pi ->
      seq_index (gaps + M_range.length gr) grs
    | gr::grs when M_range.get_start gr <= pi ->
      None
    | _::_ | [] -> begin
      let offset = pi - gaps - 1 in
      match M_range.get_direction p.p_range with
	| `Forward ->
	  Some (M_range.get_start p.p_range + offset)
	| `Reverse ->
	  Some (M_range.get_start p.p_range - offset)
    end
  in
  (* 
   * Because a profile index is 1-index we want to compare it to the length + 1
   * because lengths are start at 0
   *)
  if pi < p.p_length + 1 then
    seq_index 0 p.p_gaps
  else
    raise (Profile_idx_out_of_range (pi, p.p_length))


(*
 * There is a bug in here that gaps are returned not zero-indexed
 * as they would expect to be for a profile but indexed for the original
 * profile. However the rest of the code depends on this behavior right now
 *)
let subset_profile p (s, e) =
  let (s, e) = if s > e then (e, s) else (s, e) in
  if s < p.p_length + 1 && e < p.p_length + 1 then
    let profile_idx_range = M_range.of_tuple (s, e) in
    let rec filter_adjust_gaps = function
      | [] -> []
      | gr::grs -> begin
	match M_range.overlap gr profile_idx_range with
	  | Some r -> r::(filter_adjust_gaps grs)
	  | None -> filter_adjust_gaps grs
      end
    in
    let gaps = filter_adjust_gaps p.p_gaps in
    let text = if p.p_seq_text <> "" then String.sub ~pos:(s - 1) ~len:(e - s + 1) p.p_seq_text else "" in
    (*
     * Determine if the entire subset is a gap, if so return None
     * otherwise determine the new sequence boundaries and return
     * that
     *)
    match gaps with
	[gr] when M_range.get_start gr = s && M_range.get_end gr = e ->
	  None
      | _ -> begin
	let seq_s = 
	  match List.hd gaps with
	    | Some gr when M_range.get_start gr = s ->
	      seq_idx_of_profile_idx p (M_range.get_end gr + 1)
	    | _ ->
	      seq_idx_of_profile_idx p s
	in
	let seq_e =
	  match List.last gaps with
	    | Some gr when M_range.get_end gr = e ->
	      seq_idx_of_profile_idx p (M_range.get_start gr - 1)
	    | _ ->
	      seq_idx_of_profile_idx p e
	in
	(let open Option_monad in
	     seq_s >>= (fun s_s -> 
	       seq_e >>= (fun s_e ->
		 return
		   { p with
		     p_range = M_range.of_tuple (s_s, s_e);
		     p_gaps = gaps;
		     p_length = abs (s - e);
		     p_seq_text = text
		   })))
	end
  else if s > p.p_length then
    raise (Profile_idx_out_of_range (s, p.p_length))
  else
    raise (Profile_idx_out_of_range (e, p.p_length))

let subset_seq p (s, e) =
  match subset_profile p (profile_idx_of_seq_idx p s, profile_idx_of_seq_idx p e) with
    | Some v -> v
    | None -> raise (Failure "Should never happen")

let reverse p =
  let reverse_gaps =
    p.p_gaps
    |> List.rev
    |> List.map ~f:(M_range.lift ~f:(fun (s, e) ->
      (p.p_length - e + 1, p.p_length - s + 1)))
  in
  let reverse_text = 
    let text = String.copy p.p_seq_text in
    for i = 0 to String.length text - 1 do 
      String.set text i (String.nget p.p_seq_text (-(i + 1)))
    done;
    text
  in
  { p with
    p_range = M_range.reverse p.p_range;
    p_gaps = reverse_gaps;
    p_seq_text = reverse_text;
  }

let diff_profile_idx pi1 pi2 = pi1 - pi2

let diff_seq_idx si1 si2 = si1 - si2      

let print out_channel p =
  Printf.fprintf
    out_channel
    "p_name = (%s, %s)\n"
    (fst p.p_name)
    (snd p.p_name);
  Printf.fprintf
    out_channel
    "p_seq_name = %s\n"
    p.p_seq_name;
  Printf.fprintf 
    out_channel
    "p_range = (%d, %d)\n"
    (M_range.get_start p.p_range)
    (M_range.get_end p.p_range);
  Printf.fprintf
    out_channel
    "p_length = %d\n"
    p.p_length;
  Printf.fprintf
    out_channel
    "p_gaps =\n[ ";
  List.iter 
    ~f:(fun g -> 
      Printf.fprintf 
	out_channel 
	"(%d, %d) " 
	(M_range.get_start g)
	(M_range.get_end g))
    p.p_gaps;
  Printf.fprintf
    out_channel
    "]\n";
  Printf.fprintf
    out_channel
    "p_src_size = %d\n"
    p.p_src_size
