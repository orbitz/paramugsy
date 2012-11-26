open Core_extended
open Core_extended.Std
open Ort
open Ort.Function
open Bio

type file_path = string

type options = { basename : string
	       ; out_dir : file_path
	       ; seq_list : file_path list
	       ; maf_list : file_path list
	       ; dup_list : file_path list option
	       ; distance : int
	       ; minlength : int
	       ; colinear : bool
	       ; skipunique : bool
	       ; debug : bool
	       }

let const x _ = x

let set_option_ref r v = r := Some v

let parse_argv () =
  let debug = ref false in
  let basename = ref "mugsy" in
  let out_dir = ref "/tmp" in
  let seq_list = ref "" in
  let maf_list = ref "" in
  let dup_list = ref None in
  let distance = ref 1000 in
  let minlength = ref 30 in
  let colinear = ref false in
  let skipunique = ref false in

  let params =
    Arg.align [ ("-basename", Arg.Set_string basename, "String Basename to create files with")
	      ; ("-out_dir", Arg.Set_string out_dir, "Path Directory to place output")
	      ; ("-seq_list", Arg.Set_string seq_list, "Path File list of fasta sequences")
	      ; ("-maf_list", Arg.Set_string maf_list, "Path A file list of maf files")
	      ; ("-dup_list", Arg.String (set_option_ref dup_list), "Path Optional file list of duplicates")
	      ; ("-distance", Arg.Set_int distance, "Int Distance")
	      ; ("-minlength", Arg.Set_int minlength, "Int Minimum length")
	      ; ("-colinear", Arg.Set colinear, " Set colinear")
	      ; ("-skipunique", Arg.Set skipunique, " Skip unique")
	      ; ("-debug", Arg.Set debug, " Set debugging")
	      ]
  in

  Arg.parse params (const ()) "";
  if !seq_list = "" || !maf_list = "" then
    raise (Failure "Must specify -seq_list AND -maf_list")
  else
    let dl = match !dup_list with
      | Some v -> Some (Lazy_io.read_file_lines ~close:true (open_in v) |> Seq.to_list)
      | None -> None
    in
    { basename = !basename
    ; out_dir = !out_dir
    ; seq_list = Lazy_io.read_file_lines ~close:true (open_in !seq_list) |> Seq.to_list
    ; maf_list = Lazy_io.read_file_lines ~close:true (open_in !maf_list) |> Seq.to_list
    ; dup_list = dl
    ; distance = !distance
    ; minlength = !minlength
    ; colinear = !colinear
    ; skipunique = !skipunique
    ; debug = !debug
    }

let basename f =
  match String.rsplit2 ~on:'/' f with
    | Some (_, nf) -> nf
    | None -> f

let rewrite_sequences fname seqs =
  let fout = open_out fname in
  let rewrite_header h =
    match String.split_on_chars ~on:[':'; '.'] h with
      | s1::s2::_ when s1 <> s2 ->
	Printf.sprintf "%s.%s %s" s1 s2 s1
      | s1::s2::_ when s1 = s2 ->
	Printf.sprintf "%s.%s" s1 s2
      | [s] | [s; _] ->
	Printf.sprintf "%s.%s" s s
      | _ ->
	raise (Failure ("Unknown header to parse: " ^ h))
  in
  let m = function
    | Fasta.Header h ->
      ">" ^ (rewrite_header (String.rstrip h))
    | Fasta.Sequence s ->
      s
  in
  let write l =
    output_string fout l;
    output_char fout '\n'
  in
  let write_f f =
    Fasta.read (Lazy_io.read_file_chunks ~close:true 10000 (open_in f))
    |> Seq.map ~f:m
    |> Seq.consume ~f:write
  in
  List.iter ~f:write_f seqs;
  close_out fout

let fasta_of_maf options fname mafs =
  List.iter
    ~f:(fun maf ->
      Shell.sh
	~verbose:options.debug
	~echo:options.debug
	"mugsy_profiles maf_to_xmfa -in_maf %s >> %s"
	maf
	fname)
    mafs

let write_dups options fname dups =
  match dups with
    | Some dups ->
      fasta_of_maf options fname dups
    | None ->
      ()

let mugsy options fname all_fasta pw_fasta pw_dupsfasta =
  let colinear_opt = if options.colinear then "--refine colinear" else "" in
  let unique_opt = if options.skipunique then "" else "--unique true" in
  let dups_opt = match options.dup_list with
    | Some l when l <> [] -> Printf.sprintf ",%s --duplications true" pw_dupsfasta
    | _ -> ""
  in
  Shell.sh ~verbose:options.debug ~echo:options.debug
    "mugsyWGA --outfile %s --seq %s --aln %s%s --distance %d --minlength %d %s %s > %s.stdout 2> %s.stderr"
    fname
    all_fasta
    pw_fasta
    dups_opt
    options.distance
    options.minlength
    colinear_opt
    unique_opt
    fname
    fname;
  fname ^ ".maf"

let rm fname =
  try
    Shell.rm fname
  with
    | Shell.Process.Failed _ ->
      ()

let main () =
  let options = parse_argv () in
  Shell.mkdir ~p:() options.out_dir;
  let prefix = Printf.sprintf "%s/%s" options.out_dir options.basename in
  let all_fasta = Printf.sprintf "%s.all.fsa" prefix in
  let pw_fasta = Printf.sprintf "%s.xmfa" prefix in
  let pw_dupsfasta = Printf.sprintf "%s.dups.xmfa" prefix in
  (* Mugsy adds .maf for us *)
  let out_maf = prefix in
  List.iter ~f:rm [all_fasta; pw_fasta; pw_dupsfasta];
  rewrite_sequences all_fasta options.seq_list;
  fasta_of_maf options pw_fasta options.maf_list;
  write_dups options pw_dupsfasta options.dup_list;
  let out_maf = mugsy options out_maf all_fasta pw_fasta pw_dupsfasta in
  print_endline out_maf

let () = main ()
