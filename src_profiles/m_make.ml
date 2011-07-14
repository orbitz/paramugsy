open Core_extended.Std
open Ort

open M_profile

type options = { basename : string
	       ; out_dir : Fileutils.file_path
	       ; in_maf : Fileutils.file_path
	       }

(*
 * Takes 2 texts that must be the same length and combines them.  A gap char is replaced
 * with a value in the other, if present.  Disagreements are turned to an n
 *)
let combine_text text1 text2 =
  assert (String.length text1 = String.length text2);
  let ns = String.create (String.length text1) in
  for i = 0 to String.length ns - 1 do
    if text1.[i] = text2.[i] then
      String.set ns i text1.[i]
    else if text1.[i] <> '-' && text2.[i] <> '-' then
      String.set ns i 'N'
    else if text1.[i] <> '-' then
      String.set ns i text1.[i]
    else
      String.set ns i text2.[i]
  done;
  ns


let write_profile_file out_dir profile =
  let (major_name, minor_name) = profile.p_name in
  let outputf = Fileutils.join [out_dir; Printf.sprintf "%s.%s.idx" major_name minor_name] in
  M_profile.write_profile_file profile outputf


let rec write_profiles fasta_out prev_major_name text out_dir psin =
  match Seq.next psin with
    | Some profile when prev_major_name = fst profile.p_name ->
      write_profile_file out_dir profile;
      write_profiles fasta_out prev_major_name (combine_text text profile.p_seq_text) out_dir psin
    | Some profile ->
      Printf.fprintf fasta_out ">%s\n%s\n\n" prev_major_name text;
      write_profile_file out_dir profile;
      write_profiles fasta_out (fst profile.p_name) profile.p_seq_text out_dir psin
    | None ->
      Printf.fprintf fasta_out ">%s\n%s\n\n" prev_major_name text

let read_first_profile fasta_out out_dir psin =
  match Seq.next psin with
    | Some profile ->
      write_profile_file out_dir profile;
      write_profiles fasta_out (fst profile.p_name) profile.p_seq_text out_dir psin
    | None ->
      ()


let profile_set_of_maf ~out_dir ~in_maf ~basename =
  let psin = M_profile_stream.profile_stream_of_maf ~basename:basename in_maf in
  let fasta_out = open_out (Fileutils.join [out_dir; basename ^ ".fasta"]) in
  read_first_profile fasta_out out_dir psin;
  close_out fasta_out

let usage = ""

let parse_argv argv =
  let basename = ref "" in
  let out_dir = ref "" in
  let in_maf = ref "" in
  
  let params = 
    Arg.align [ ("-basename", Arg.Set_string basename, "String Basename to give all output profiles.")
	      ; ("-out_dir", Arg.Set_string out_dir, "Path Location to output files.")
	      ; ("-in_maf", Arg.Set_string in_maf, "Path Path to the incoming MAF file.")
	      ]
  in
  Arg.parse params (fun _ -> ()) usage;
  if !basename = "" then
    raise (Failure "Must provide -basename")
  else if !out_dir = "" then
    raise (Failure "Must provide -out_dir")
  else if !in_maf = "" then
    raise (Failure "Must provide -in_maf")
  else
    { basename = !basename
    ; out_dir = !out_dir
    ; in_maf = !in_maf
    }

let main argv =
  let options = parse_argv argv in
  Shell.mkdir_p options.out_dir;
  profile_set_of_maf ~out_dir:options.out_dir ~in_maf:options.in_maf ~basename:options.basename

