(*pp camlp4o `ocamlfind query -i-format lwt` `ocamlfind query -predicates syntax,preprocessor -a-format -r lwt.syntax` *)
open Core_extended
open Core_extended.Std
open Lwt
open Ort
open Ort.Function

open Mugsy_guide_tree

module Tuple_map = Map.Make(struct type t = (string * string) let compare = compare end)

type sge_mode = { out_dir : Fileutils.file_path
		; sequences : Fileutils.file_path list
		; exec_q : string
		; stage_data : bool
		; staging_q : string
		; tmp_dir : Fileutils.file_path
		; distance : int
		; minlength : int
		; template_file : Fileutils.file_path
		; seqs_per_mugsy : int
		; nucmer_chunk_size : int
		; out_maf : Fileutils.file_path
		}

let usage = ""

let parse_argv argv =
  let seq_list = ref "" in
  let out_dir = ref "" in
  let exec_q = ref "exec.q" in
  let stage_data = ref false in
  let staging_q = ref "staging.q" in
  let tmp_dir = ref "/tmp" in
  let distance = ref 1000 in
  let minlength = ref 30 in
  let template_file = ref "/usr/local/etc/pm_qsub_template.sh" in
  let seqs_per_mugsy = ref 2 in
  let nucmer_chunk_size = ref 1 in
  let out_maf = ref "" in

  let params = 
    Arg.align [ ("-seq_list", Arg.Set_string seq_list, "Path A file containing a list of paths to sequences.")
	      ; ("-out_dir", Arg.Set_string out_dir, "Path Location to output files.")
	      ; ("-exec_q", Arg.Set_string exec_q, "String Queue name to run exec jobs on, defaults to exec.q.")
	      ; ("-stage_data", Arg.Set stage_data, " Handle staging data out to nodes, off by default.")
	      ; ("-staging_q", Arg.Set_string staging_q, "String Queue to run data staging jobs in, defaults to staging.q (if -stage_data).")
	      ; ("-tmp_dir", Arg.Set_string tmp_dir, "String Directory to put temporary files in, defaults to /tmp")
	      ; ("-distance", Arg.Set_int distance, "Int Distance option for Mugsy (default 1000)")
	      ; ("-minlength", Arg.Set_int minlength, "Int Minlength for Mugsy (default 30)")
	      ; ("-template_file", Arg.Set_string template_file, "Path File to use for qsub templates")
	      ; ("-seqs_per_mugsy", Arg.Set_int seqs_per_mugsy, "Int Maximum number of sequences to have per Mugsy run (default 2)")
	      ; ("-nucmer_chunk_size", Arg.Set_int nucmer_chunk_size, "Int Maximum number of sequences to have per Nucmer run (default 1)")
	      ; ("-out_maf", Arg.Set_string out_maf, "Path File to output the final MAF")
	      ]
  in
  Arg.parse params (fun _ -> ()) usage;
  if !seq_list = "" then
    raise (Failure "Must set -seq_list")
  else if !out_dir = "" then
    raise (Failure "Must set -out_dir")
  else if !out_maf = "" then
    raise (Failure "Must set -out_maf")
  else if !seqs_per_mugsy < 2 then
    raise (Failure "-seqs_per_mugsy must be >= 2")
  else
    { out_dir = !out_dir
    ; sequences = Lazy_io.read_file_lines ~close:true (open_in !seq_list) |> Seq.to_list
    ; exec_q = !exec_q
    ; stage_data = !stage_data
    ; staging_q = !staging_q
    ; tmp_dir = !tmp_dir
    ; distance = !distance
    ; minlength = !minlength
    ; template_file = !template_file
    ; seqs_per_mugsy = !seqs_per_mugsy
    ; nucmer_chunk_size = !nucmer_chunk_size
    ; out_maf = !out_maf
    }

let tuple_map_add_all m1 m2 =
  Tuple_map.fold (fun k v a -> Tuple_map.add k v a) m1 m2

let list_of_tuple_map m =
  Tuple_map.fold (fun k v a -> v::a) m []

let rec print_lines fout sin =
  match Seq.next sin with
    | Some l -> begin
      output_string fout l;
      output_char fout '\n';
      print_lines fout sin
    end
    | None ->
      ()

let rewrite_sequences sequences tmp_dir =
  let rewrite_sequence seq =
    let species_name = M_rewrite_fasta.species_name seq in
    let fout_name =  Fileutils.join [tmp_dir; species_name] in
    let fout = open_out fout_name in
    print_lines fout (M_rewrite_fasta.rewrite_headers seq);
    close_out fout;
    fout_name
  in
  Shell.mkdir_p tmp_dir;
  List.map ~f:rewrite_sequence sequences

let searches left_seqs right_seqs =
  let inner_fold acc e =
    List.fold_left
      ~f:(fun acc' e' ->
	(e, e')::acc')
      ~init:acc
      right_seqs
  in
  List.fold_left
    ~f:inner_fold
    ~init:[]
    left_seqs

let basename_of_search (r, q) =
  (Fileutils.basename r, Fileutils.basename q)

let sge_options_of_options priority options =
  { Pm_sge_utils.priority = priority
  ; Pm_sge_utils.template_file = options.template_file
  ; Pm_sge_utils.tmp_dir = options.tmp_dir
  ; Pm_sge_utils.script_dir = options.tmp_dir
  ; Pm_sge_utils.exec_queue = options.exec_q
  ; Pm_sge_utils.data_queue = options.staging_q
  ; Pm_sge_utils.out_dir = options.out_dir
  }

(*
 * Takes a tree and converts it into a flat [(nucmer_seq * nucmer_seq) sge_command list]
 *)
let rec create_nucmers depth = function
  | H_taxonomic_unit (l, r) -> begin
    let left_seqs = Mugsy_guide_tree.list_of_guide_tree l in
    let right_seqs = Mugsy_guide_tree.list_of_guide_tree r in
    let results = List.map ~f:(fun s -> (depth, s)) (searches left_seqs right_seqs) in
    [< Seq.of_list results; create_nucmers (depth + 1) l; create_nucmers (depth + 1) r >]
  end
  | Taxonomic_unit _ ->
    [< >]

      
let run_nucmer_chunks options nucmer_chunks =
  let get_priority = function
    | (priority, _)::_ ->
      priority
    | [] ->
      raise (Failure "Could not get priority")
  in
  let rec qsub_nucmers' map_accum = function
    | [] ->
      Lwt.return map_accum
    | chunks::rest_chunks ->
      let priority = get_priority chunks in
      let seqs = List.map ~f:snd chunks in
      lwt jobs = 
	Pm_sge_nucmer.run_nucmer
	  (sge_options_of_options priority options)
	  seqs
      in
      let map_accum' =
	List.fold_left
	  ~f:(fun acc job -> Tuple_map.add (basename_of_search job.Pm_sge_nucmer.search_files) job acc)
	  ~init:map_accum
	  jobs
      in
      qsub_nucmers' map_accum' rest_chunks
  in
  qsub_nucmers' Tuple_map.empty nucmer_chunks

let wait_on_nucmer_jobs nucmer_job_map searches =
  let jobs = 
    List.map 
      ~f:(fun k -> Tuple_map.find (basename_of_search k) nucmer_job_map) 
      searches
  in
  Pm_sge_nucmer.wait_on_nucmer_jobs jobs


let run_mugsy_and_wait options depth sequences nucmer_mafs =
  lwt mugsy_job = 
    Pm_sge_mugsy.run_mugsy
      ~distance:options.distance 
      ~minlength:options.minlength
      (sge_options_of_options depth options)
      sequences
      nucmer_mafs
  in
  Pm_sge_mugsy.wait_on_mugsy_job mugsy_job

let run_mugsy_with_profiles_and_wait options depth left_maf right_maf nucmer_deltas =
  lwt mugsy_job =
    Pm_sge_mugsy.run_mugsy_with_profiles
      ~distance:options.distance
      ~minlength:options.minlength
      (sge_options_of_options depth options)
      left_maf
      right_maf
      nucmer_deltas
  in
  Pm_sge_mugsy.wait_on_mugsy_job mugsy_job

let run_fake_mugsy_and_wait options depth sequence =
  lwt mugsy_job =
    Pm_sge_mugsy.run_fake_mugsy
      (sge_options_of_options depth options)
      sequence
  in
  Pm_sge_mugsy.wait_on_mugsy_job mugsy_job

let rec run_mugsy_jobs options depth nucmer_job_map = function
  | H_taxonomic_unit (l, r) as subtree -> begin
    let sequences = Mugsy_guide_tree.list_of_guide_tree subtree in
    lwt () = 
      Lwt_io.printf 
	"--- Processing depth: %d Num seqs: %d\n" 
	depth 
	(List.length sequences)
    in
    if List.length sequences <= options.seqs_per_mugsy then
      process_mugsy_leaf options depth nucmer_job_map (l, r)
    else
      process_mugsy_internal options depth nucmer_job_map (l, r)
  end
  | Taxonomic_unit sequence ->
    run_fake_mugsy_and_wait
      options
      depth
      sequence
and process_mugsy_leaf options depth nucmer_job_map (l, r) =
  let left_seqs = Mugsy_guide_tree.list_of_guide_tree l in
  let right_seqs = Mugsy_guide_tree.list_of_guide_tree r in
  let sequences = 
    List.append
      left_seqs
      right_seqs
  in
  lwt nucmer_jobs = wait_on_nucmer_jobs nucmer_job_map (searches left_seqs right_seqs) in
  let nucmer_mafs =
    List.map
      ~f:(fun nucmer -> nucmer.Pm_sge_nucmer.output_maf)
      nucmer_jobs
  in
  run_mugsy_and_wait
    options
    depth
    sequences
    nucmer_mafs
and process_mugsy_internal options depth nucmer_job_map (l, r) =
  let left_seqs = Mugsy_guide_tree.list_of_guide_tree l in
  let right_seqs = Mugsy_guide_tree.list_of_guide_tree r in
  lwt left_mugsy = run_mugsy_jobs options (depth + 1) nucmer_job_map l
  and right_mugsy = run_mugsy_jobs options (depth + 1) nucmer_job_map r
  and nucmer_jobs = wait_on_nucmer_jobs nucmer_job_map (searches left_seqs right_seqs)
  in
  let nucmer_deltas =
    List.map
      ~f:(fun nucmer -> nucmer.Pm_sge_nucmer.output_delta)
      nucmer_jobs
  in
  run_mugsy_with_profiles_and_wait 
    options 
    depth 
    left_mugsy.Pm_sge_mugsy.mugsy_maf
    right_mugsy.Pm_sge_mugsy.mugsy_maf
    nucmer_deltas

let run_tree options tree =
  let nucmers = 
    List.sort 
      ~cmp:(fun (depth1, _) (depth2, _) -> compare depth2 depth1) 
      (Seq.to_list (create_nucmers 0 tree))
  in
  let nucmers_chunked = 
    nucmers |> 
	Seq.of_list |> 
	    Seq.chunk options.nucmer_chunk_size |> 
		Seq.to_list
  in
  lwt nucmer_job_map = run_nucmer_chunks options nucmers_chunked in
  run_mugsy_jobs options 0 nucmer_job_map tree

let run_sge argv =
  let options = parse_argv argv in
  Shell.mkdir_p options.out_dir;
  Shell.mkdir_p options.tmp_dir;
  let sequences = rewrite_sequences options.sequences options.tmp_dir in
  let guide_tree = Mugsy_guide_tree.guide_tree_of_sequences sequences in
  let final_maf = Lwt_main.run (run_tree options guide_tree >>= 
				  fun m -> Copy_file.copy_file m.Pm_sge_mugsy.mugsy_maf options.out_maf)
  in
  print_endline final_maf

