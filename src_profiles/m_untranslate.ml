(*pp camlp4o *)
open Core_extended.Std
open Ort
open Ort.Function

open M_profile

module String_map = Map.Make(String)

type options = { profile_paths : Fileutils.file_path list
	       ; in_maf : Fileutils.file_path
	       ; out_maf : Fileutils.file_path
	       }

let complement = function
  | 'A' -> 'T'
  | 'a' -> 't'
  | 'T' -> 'A'
  | 't' -> 'a'
  | 'C' -> 'G'
  | 'c' -> 'g'
  | 'G' -> 'C'
  | 'g' -> 'c'
  | n -> n


let profile_map_of_profile_set =
  let add_to_list m p =
    let name = fst p.p_name in
    if String_map.mem name m then
      let l = String_map.find name m in
      String_map.add name (p::l) m
    else
      String_map.add name [p] m
  in
  List.fold_left ~f:add_to_list ~init:String_map.empty

let expand_text p_text text =
  let ret = String.copy text in
  let length = String.length ret in
  let rec expand_text' pidx idx =
    if idx < length then
      if ret.[idx] = '-' then
	expand_text' pidx (idx + 1)
      else begin
	String.set ret idx p_text.[pidx];
	expand_text' (pidx + 1) (idx + 1)
      end
    else
      ret
  in
  expand_text' 0 0


let get_real_range overlap p p_sub =
  match M_range.get_direction overlap with
    | `Forward ->
      (p_sub.p_range, M_range.get_direction p.p_range)
    | `Reverse ->
      (M_range.reverse p_sub.p_range, M_range.get_direction (M_range.reverse p.p_range))

let get_start_size direction real_range p =
  match M_range.get_direction real_range with
    | `Forward ->
      (M_range.get_start real_range - 1, 
       M_range.length real_range)
    | `Reverse ->
      (p.p_src_size - M_range.get_start real_range,
       M_range.length real_range)

let rec untranslate_profiles overlap text = function
  | [] ->
    [< >]
  | p::ps -> begin
    let p_sub_option = 
      M_profile.subset_profile 
	p 
	((M_profile.profile_idx_of_int (M_range.get_start overlap)),
	 (M_profile.profile_idx_of_int (M_range.get_end overlap)))
    in
    match p_sub_option with
      | Some p_sub ->
	let (real_range, direction) = get_real_range overlap p p_sub in
	let (start, size) = get_start_size direction real_range p in
	let seq_text =
	  if M_range.get_direction p.p_range = direction then
	    p_sub.p_seq_text
	  else
	    (M_profile.reverse p_sub).p_seq_text
	in
	let maf_text = 
	  if M_range.get_direction p.p_range = direction then
	    expand_text seq_text text
	  else
	    String.map ~f:complement (expand_text seq_text text)
	in
	[< 'Printf.sprintf "s %s %d %d %s %d %s"
	    p.p_seq_name
	    start
	    size
	    (M_profile_stream.string_of_direction direction)
	    p.p_src_size
	    maf_text
	;  untranslate_profiles overlap text ps
	>]
      | None -> begin
	Printf.eprintf "All gaps found\n";
	untranslate_profiles overlap text ps
      end
  end


let untranslate_score profile_map score_line =
  let (name, start, size, d, src_size, text) = M_profile_stream.split_maf score_line in
  let overlap = 
    M_range.of_maf
      ~start:start
      ~size:size
      ~src_size:src_size
      ~direction:d
  in
  let profiles = String_map.find name profile_map in
  untranslate_profiles overlap text profiles

  

let rec untranslate_maf profile_map sin =
  match Seq.next sin with
    | Some l when String.is_prefix ~prefix:"##maf " l ->
      (*
       * Ignore the maf header
       *)
      untranslate_maf profile_map sin
    | Some l when String.is_empty l || l.[0] = '#' || String.is_prefix ~prefix:"a score=" l ->
      (*
       * If a line is a comment or empty line or score just pass it through
       *)
      [< 'l
      ;  untranslate_maf profile_map sin
      >]
    | Some l when String.is_prefix ~prefix:"s " l ->
      (*
       * We have an actual score, untranslate it
       *)
      [< untranslate_score profile_map l
      ;  untranslate_maf profile_map sin
      >]
    | Some l ->
      raise (Failure ("Unknown line: " ^ l))
    | None ->
      [< >]
      

let untranslate ~profile_paths ~in_maf =
  let profile_files = 
    profile_paths 
    |> List.map ~f:(fun p -> p |> Sys.readdir |> Array.to_list |> List.map ~f:(fun f -> Fileutils.join [p; f]))
    |> List.fold_left ~f:(@) ~init:[]
    |> List.filter ~f:(String.is_suffix ~suffix:".idx")
  in
  let profile_set = List.map ~f:M_profile.read_profile_file profile_files in
  (*
   * This is a mapping of profile names to profiles unlike in translate
   * which is a mapping of sequence names to profiles
   *)
  let profile_map = profile_map_of_profile_set profile_set in
  untranslate_maf profile_map (Lazy_io.read_file_lines ~close:true (open_in in_maf))

let usage = ""

let parse_argv argv =
  let profile_paths_list = ref "" in
  let in_maf = ref "" in
  let out_maf = ref "" in
  
  let params = 
    Arg.align [ ("-profile_paths_list", Arg.Set_string profile_paths_list, "Path Path to file list of directories containing profiles")
	      ; ("-in_maf", Arg.Set_string in_maf, "Path Input MAF file")
	      ; ("-out_maf", Arg.Set_string out_maf, "Path Output MAF file")
	      ]
  in
  Arg.parse params (fun _ -> ()) usage;
  if !profile_paths_list = "" then
    raise (Failure "Must provide -profile_paths_list")
  else if !in_maf = "" then
    raise (Failure "Must provide -in_maf")
  else if !out_maf = "" then
    raise (Failure "Must provide -out_maf")
  else
    { profile_paths = open_in !profile_paths_list |> Lazy_io.read_file_lines ~close:true |> Seq.to_list
    ; in_maf = !in_maf
    ; out_maf = !out_maf
    }


let main argv =
  let options = parse_argv argv in
  let fout = open_out options.out_maf in
  let rec print_lines sin =
    match Seq.next sin with
      | Some l -> begin
	output_string fout l;
	output_char fout '\n';
	print_lines sin
      end
      | None ->
	()
  in
  output_string fout "##maf version=1 scoring=paramugsy\n";
  print_lines (untranslate ~profile_paths:options.profile_paths ~in_maf:options.in_maf);
  close_out fout
