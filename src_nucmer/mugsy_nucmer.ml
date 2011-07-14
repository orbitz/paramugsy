(*pp camlp4o *)
(*
 * The input to this is going to be a file list.  By default we will be taking the first element
 * on the file list and running individual nucmers with it against every other sequence given.
 *
 * If -searchall is defined, that means all one-way combinations of nucmers will be run.
 * Ex:
 * S1
 * S2
 * S3
 * nucmer(S1, S2), nucmer(S1, S3) -- At this point this is what would be run without --searchall
 * In addition to the above, with --searchall the following will be done:
 * nucmer(S2, S3)
 * With --fullsearch the following will be applied:
 * nucmer(S2, S1), nucmer(S3, S2), nucmer(S3, S2)
 *
 * --fullsearch implies --searchall
 * 
 * The output is a number of file lists
 *)


open Core_extended
open Core_extended.Std
open Core_extended.Function
open Ort
open Ort.Function

type file_path = string

type options = { ref_seq : file_path
	       ; query_seq : file_path
	       ; maf_out : file_path
	       ; delta_out : file_path
	       ; delta_pp : string option
	       ; nucmer_opts : string
	       ; out_dir : file_path
	       ; filter : bool
	       ; colinear : bool
	       ; debug : bool
	       ; tmp_dir : file_path
	       }


let set_option_ref r v = r := Some v

let parse_argv () =
  let debug = ref false in
  let out_dir = ref "/tmp" in
  let ref_seq = ref "" in
  let query_seq = ref "" in
  let argv_file_list = ref [] in
  let delta_pp = ref None in
  let nucmer_opts = ref "" in
  let filter = ref true in
  let colinear = ref false in
  let maf_out = ref "" in
  let delta_out = ref "" in
  let tmp_dir = ref "/tmp" in
  
  let accum_sequences s = argv_file_list := (s::!argv_file_list) in

  let params =
    Arg.align [ ("-out_dir", Arg.Set_string out_dir, "Path Directory to place output")
	      ; ("-ref_seq", Arg.Set_string ref_seq, "Path Fasta file for the reference sequence")
	      ; ("-query_seq", Arg.Set_string query_seq, "Path Fasta file for query sequence")
	      ; ("-maf_out", Arg.Set_string maf_out, "String Name to give MAF file")
	      ; ("-delta_out", Arg.Set_string delta_out, "String Name to five delta file")
	      ; ("-delta_pp", Arg.String (set_option_ref delta_pp), "String Program to postprocess on a delta file.  Will be after filter step if filter happens.")
	      ; ("-nucmer_opts", Arg.Set_string nucmer_opts, "String Options to pass to nucmer")
	      ; ("-nofilter", Arg.Clear filter, " Turn filtering off")
	      ; ("-colinear", Arg.Set colinear, " Turn colinear on")
	      ; ("-debug", Arg.Set debug, " Turn debugging on")
	      ; ("-tmp_dir", Arg.Set_string tmp_dir, "Path Path for temporary files, defaulsts to /tmp")
	      ]
  in

  Arg.parse params accum_sequences "";
  if !ref_seq = "" || !query_seq = "" then
    raise (Failure "Must specify -ref_seq and -query_seq")
  else if !maf_out = "" || !delta_out = "" then
    raise (Failure "Must specify -maf_out and -delta_out")
  else
    { ref_seq = !ref_seq
    ; query_seq = !query_seq
    ; maf_out = !out_dir ^ "/" ^ !maf_out
    ; delta_out = !out_dir ^ "/" ^ !delta_out
    ; delta_pp = !delta_pp
    ; nucmer_opts = !nucmer_opts
    ; out_dir = !out_dir
    ; filter = !filter
    ; colinear = !colinear
    ; debug = !debug
    ; tmp_dir = !tmp_dir
    }

let nucmer options ref_file query_file =
  let obname = Printf.sprintf "%s/nucmer" options.tmp_dir in
  let delta_file = Printf.sprintf "%s.delta" obname in
  let delta_filt_file = Printf.sprintf "%s.filt.delta" obname in
  Shell.sh ~verbose:options.debug ~echo:options.debug "nucmer %s %s -p %s %s" ref_file query_file obname options.nucmer_opts;
  let delta_file' = 
    if options.filter then begin
      let chaining_opt = if options.colinear then "-m" else "-1" in
      Shell.sh ~verbose:options.debug ~echo:options.debug "delta-filter %s %s > %s" chaining_opt delta_file delta_filt_file;
      delta_filt_file
    end
    else
      delta_file
  in
  match options.delta_pp with
    | Some pp ->
      let delta_pp_file = Printf.sprintf "%s.pp.delta" obname in
      Shell.sh ~verbose:options.debug ~echo:options.debug "%s < %s > %s" pp delta_file' delta_pp_file;
      delta_pp_file
    | None ->
      delta_file'
    
let generate_maf options =
  (* Shell.sh ~verbose:options.debug ~echo:options.debug "delta2maf %s | fixMAFnames.pl > %s" options.delta_out options.maf_out *)
  Shell.sh ~verbose:options.debug ~echo:options.debug "delta2maf %s > %s" options.delta_out options.maf_out
  

let rec run_search options =
  let delta_file = nucmer options options.ref_seq options.query_seq in
  Shell.cp delta_file options.delta_out;
  generate_maf options


let main () =
  let options = parse_argv () in
  Shell.mkdir_p options.out_dir;
  Shell.mkdir_p options.tmp_dir;
  run_search options;
  Shell.sh ~verbose:options.debug ~echo:options.debug "rm -rf %s" options.tmp_dir

let () = main ()
