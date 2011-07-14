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
		; out_maf : Fileutils.file_path
		}


type job_id = string

type nucmer_job = { search_job_id : job_id
		  ; search_maf : Fileutils.file_path
		  ; search_delta : Fileutils.file_path
		  ; search_sequences : (string * string)
		  }

type mugsy_job = { mugsy_job_id : job_id
		 ; mugsy_maf : Fileutils.file_path
		 ; mugsy_out_dir : Fileutils.file_path
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


let searches sequences =
  let rec search s = function
    | [] -> [< >]
    | x::xs -> [< '(s, x); search s xs >]
  in
  let rec search_all = function
    | [] | [_] -> [< >]
    | x::xs -> [< search x xs; search_all xs >]
  in
  search_all sequences

let qsub_nucmer ~priority ~template_file ~datasync ~tmp_dir ~script_dir ~exec_queue ~data_queue ~out_dir ref_seq query_seq =
  let basename = Fileutils.basename ref_seq ^ "-" ^ Fileutils.basename query_seq in
  let cmd = [ Printf.sprintf 
		"mugsy_nucmer -ref_seq %s -query_seq %s -out_dir %s -tmp_dir %s/%s -maf_out %s.maf -delta_out %s.delta"
		ref_seq
		query_seq
		out_dir
		tmp_dir
		basename
		basename
		basename
	    ]
  in
  if datasync then
    Pm_sge_utils.qsub_with_datasync
      ~wait:false
      ~priority:priority
      ~template_file:template_file
      ~script_dir:script_dir
      ~exec_queue:exec_queue
      ~data_queue:data_queue
      ~in_files:[(ref_seq, ref_seq); (query_seq, query_seq)]
      ~out_files:[ (Fileutils.join [out_dir; basename ^ ".maf"], out_dir)
		 ; (Fileutils.join [out_dir; basename ^ ".delta"], out_dir)]
      cmd
  else
    Pm_sge_utils.qsub
      ~wait:false
      ~priority:priority
      ~template_file:template_file
      ~script_dir:script_dir
      ~queue:exec_queue
      cmd

let wait_on_nucmer_searches searches =
  let job_ids = Tuple_map.fold (fun k v a -> (v.search_job_id)::a) searches [] in
  Lwt_list.map_s Pm_sge_utils.wait_on_job_id job_ids

let run_nucmers options priority out_dir sequences =
  let nucmer_search (seq_ref, seq_query) =
    let basename = Fileutils.basename seq_ref ^ "-" ^ Fileutils.basename seq_query in
    lwt search_job = 
      qsub_nucmer
	~priority:priority
	~template_file:options.template_file
	~datasync:options.stage_data
	~tmp_dir:options.tmp_dir
	~script_dir:options.tmp_dir
	~exec_queue:options.exec_q
	~data_queue:options.staging_q
	~out_dir:out_dir
	seq_ref
	seq_query

    in
    let search_maf = Fileutils.join [out_dir; basename ^ ".maf"] in
    let search_delta = Fileutils.join [out_dir; basename ^ ".delta"] in
    Lwt.return { search_job_id = search_job
	       ; search_maf = search_maf
	       ; search_delta = search_delta
	       ; search_sequences = (Fileutils.basename seq_ref, Fileutils.basename seq_query)
	       }
  in
  lwt nucmer_jobs = searches sequences |> Seq.to_list |> Lwt_list.map_s nucmer_search in
  Lwt.return (List.fold_left 
		~f:(fun m x -> Tuple_map.add x.search_sequences x m) 
		~init:Tuple_map.empty 
		nucmer_jobs)

let run_nucmers_and_wait options priority out_dir sequences =
  lwt nucmer_runs = run_nucmers options priority out_dir sequences in
  lwt _ = wait_on_nucmer_searches nucmer_runs in
  Lwt.return nucmer_runs

let run_nucmer_left_against_right options priority out_dir left right =
  let nucmer_search l = Lwt_list.map_s (fun r -> run_nucmers options priority out_dir [l; r]) right in
  lwt nucmers = Lwt_list.map_s nucmer_search left in
  Lwt.return (nucmers |> List.flatten |> List.fold_left ~f:tuple_map_add_all ~init:Tuple_map.empty)

let run_nucmer_left_against_right_and_wait options priority out_dir left right =
  lwt nucmer_runs = run_nucmer_left_against_right options priority out_dir left right in
  lwt _ = wait_on_nucmer_searches nucmer_runs in
  Lwt.return nucmer_runs


let wait_on_mugsy_runs mugsys =
  let job_ids = List.map ~f:(fun v -> v.mugsy_job_id) mugsys in
  Lwt_list.map_s Pm_sge_utils.wait_on_job_id job_ids


let qsub_mugsy ~priority ~template_file ~datasync ~script_dir ~exec_queue ~data_queue ~tmp_dir ~out_dir ~distance ~minlength seqs mafs =
  let write_seqs seqs file =
    let fout = open_out file in
    List.iter ~f:(Printf.fprintf fout "%s\n") seqs;
    close_out fout
  in
  let write_mafs = write_seqs in
  let two_tuple t = (t, t) in
  let gs = Global_state.make_ref () in
  let seqs_list = Fileutils.join [tmp_dir; "mugsy-" ^ gs ^ ".seqs"] in
  let mafs_list = Fileutils.join [tmp_dir; "mugsy-" ^ gs ^ ".mafs"] in
  write_seqs seqs seqs_list;
  write_mafs mafs mafs_list;
  let cmd = [ Printf.sprintf "echo Start-Single %d %s `date %s`" priority out_dir "+%Y%m%d%H%M%S"
	    ; Printf.sprintf
	      "mugsy_mugsy -out_dir %s -seq_list %s -maf_list %s -minlength %d -distance %d -debug"
	      out_dir
	      seqs_list
	      mafs_list
	      minlength
	      distance
 	    ; Printf.sprintf "echo End-Single %d %s `date %s`" priority out_dir "+%Y%m%d%H%M%S"
	    ]
  in
  if datasync then
    Pm_sge_utils.qsub_with_datasync
      ~wait:false
      ~priority:priority
      ~template_file:template_file
      ~script_dir:script_dir
      ~exec_queue:exec_queue
      ~data_queue:data_queue
      ~in_files:(List.map ~f:two_tuple (seqs @ mafs @ [seqs_list] @ [mafs_list]))
      ~out_files:[two_tuple (Fileutils.join [out_dir; "mugsy.maf"])]
      cmd
  else
    Pm_sge_utils.qsub
      ~wait:false
      ~priority:priority
      ~template_file:template_file
      ~script_dir:script_dir
      ~queue:exec_queue
      cmd

let run_mugsy options priority out_dir sequences mafs =
  lwt mugsy_job_id =
    qsub_mugsy
      ~priority:priority
      ~template_file:options.template_file
      ~datasync:options.stage_data
      ~script_dir:options.tmp_dir
      ~exec_queue:options.exec_q
      ~data_queue:options.staging_q
      ~tmp_dir:options.tmp_dir
      ~out_dir:out_dir
      ~distance:options.distance
      ~minlength:options.minlength
      sequences
      mafs
  in
  Lwt.return { mugsy_job_id = mugsy_job_id
	     ; mugsy_maf = Fileutils.join [out_dir; "mugsy.maf"]
	     ; mugsy_out_dir = out_dir
	     }
    
let run_mugsy_and_wait options priority out_dir sequences mafs =
  lwt mugsy_run = run_mugsy options priority out_dir sequences mafs in
  lwt _ = wait_on_mugsy_runs [mugsy_run] in
  Lwt.return mugsy_run

let qsub_mugsy_with_profiles ~priority ~template_file ~datasync ~script_dir ~exec_queue ~data_queue ~in_files ~out_files cmd =
  let two_tuple t = (t, t) in
  if datasync then
    Pm_sge_utils.qsub_with_datasync
      ~wait:false
      ~priority:priority
      ~template_file:template_file
      ~script_dir:script_dir
      ~exec_queue:exec_queue
      ~data_queue:data_queue
      ~in_files:(List.map ~f:two_tuple in_files)
      ~out_files:(List.map ~f:two_tuple out_files)
      cmd
  else
    Pm_sge_utils.qsub
      ~wait:false
      ~priority:priority
      ~template_file:template_file
      ~script_dir:script_dir
      ~queue:exec_queue
      cmd


let run_mugsy_with_profiles options priority out_dir left_maf right_maf nucmer_deltas =
  let write_files files fname =
    let fout = open_out fname in
    List.iter ~f:(Printf.fprintf fout "%s\n") files;
    close_out fout
  in    
  let profile_left = Fileutils.join [out_dir; "profiles-l"] in
  let profile_right = Fileutils.join [out_dir; "profiles-r"] in
  let nucmer_list = Fileutils.join [out_dir; "nucmer.list"] in
  let seqs_list = Fileutils.join [out_dir; "seqs.list"] in
  let maf_list = Fileutils.join [out_dir; "maf.list"] in
  let untranslate_list = Fileutils.join [out_dir; "untranslate.list"] in
  let untranslate_maf = Fileutils.join [out_dir; "untranslated.maf"] in
  write_files nucmer_deltas nucmer_list;
  write_files 
    [ Fileutils.join [profile_left; "l.fasta"]
    ; Fileutils.join [profile_right; "r.fasta"]
    ]
    seqs_list;
  write_files [Fileutils.join [out_dir; "profile.maf"]] maf_list;
  write_files [profile_left; profile_right] untranslate_list;
  let cmds = [ Printf.sprintf "echo Start-Multi %d %s `date %s`" priority out_dir "+%Y%m%d%H%M%S"
	     ; Printf.sprintf "mugsy_profiles make -in_maf %s -out_dir %s -basename l" left_maf profile_left
	     ; Printf.sprintf "mugsy_profiles make -in_maf %s -out_dir %s -basename r" right_maf profile_right
	     ; Printf.sprintf 
	       "mugsy_profiles translate -profiles_left %s -profiles_right %s -nucmer_list %s -out_delta %s/profile.delta" 
	       profile_left 
	       profile_right 
	       nucmer_list 
	       out_dir
	     ; Printf.sprintf "delta2maf %s/profile.delta > %s/profile.maf" out_dir out_dir
	     ; Printf.sprintf 
	       "mugsy_mugsy -out_dir %s/mugsy -seq_list %s -maf_list %s -minlength %d -distance %d -debug" 
	       out_dir 
	       seqs_list 
	       maf_list 
	       options.minlength 
	       options.distance
	     ; Printf.sprintf 
	       "mugsy_profiles untranslate -profile_paths_list %s -in_maf %s/mugsy/mugsy.maf -out_maf %s" 
	       untranslate_list 
	       out_dir
	       untranslate_maf
	     ; Printf.sprintf
	       "rm -rf %s %s %s/profile.delta %s/profile.maf %s/mugsy/mugsy.xmfa"
	       profile_left
	       profile_right
	       out_dir
	       out_dir
	       out_dir
	     ; Printf.sprintf "echo End-Multi %d %s `date %s`" priority out_dir "+%Y%m%d%H%M%S"
	     ]
  in
  lwt mugsy_with_profiles_id =
    qsub_mugsy_with_profiles
      ~priority:priority
      ~template_file:options.template_file
      ~datasync:options.stage_data
      ~script_dir:options.tmp_dir
      ~exec_queue:options.exec_q
      ~data_queue:options.staging_q
      ~in_files:([ left_maf
		 ; right_maf
		 ; nucmer_list
		 ; seqs_list
		 ; maf_list
		 ; untranslate_list
		 ] @ nucmer_deltas)
      ~out_files:[untranslate_maf]
      cmds
  in
  Lwt.return { mugsy_job_id = mugsy_with_profiles_id
	     ; mugsy_maf = untranslate_maf
	     ; mugsy_out_dir = out_dir
	     }

let run_mugsy_with_profiles_and_wait options priority out_dir left_maf right_maf nucmer_deltas =
  lwt mugsy_with_profiles_run = run_mugsy_with_profiles options priority out_dir left_maf right_maf nucmer_deltas in
  lwt _ = wait_on_mugsy_runs [mugsy_with_profiles_run] in
  Lwt.return mugsy_with_profiles_run


let qsub_fake_mugsy ~priority ~template_file ~datasync ~script_dir ~exec_queue ~data_queue in_fasta out_maf =
  let cmd = [Printf.sprintf
		"mugsy_profiles fasta_to_maf -in_fasta %s -out_maf %s"
		in_fasta
		out_maf
	    ]
  in
  if datasync then
    Pm_sge_utils.qsub_with_datasync
      ~wait:false
      ~priority:priority
      ~template_file:template_file
      ~script_dir:script_dir
      ~exec_queue:exec_queue
      ~data_queue:data_queue
      ~in_files:([(in_fasta, in_fasta)])
      ~out_files:([(out_maf, out_maf)])
      cmd
  else
    Pm_sge_utils.qsub
      ~wait:false
      ~priority:priority
      ~template_file:template_file
      ~script_dir:script_dir
      ~queue:exec_queue
      cmd
     

let run_fake_mugsy options priority in_fasta out_maf =
  lwt fake_mugsy_job =
    qsub_fake_mugsy
      ~priority:priority
      ~template_file:options.template_file
      ~datasync:options.stage_data
      ~script_dir:options.tmp_dir
      ~exec_queue:options.exec_q
      ~data_queue:options.staging_q
      in_fasta
      out_maf
  in
  Lwt.return { mugsy_job_id = fake_mugsy_job
	     ; mugsy_maf = out_maf
	     ; mugsy_out_dir = Fileutils.dirname out_maf
	     }

let run_fake_mugsy_and_wait options priority in_fasta out_maf =
  lwt fake_mugsy_run = run_fake_mugsy options priority in_fasta out_maf in
  lwt _ = wait_on_mugsy_runs [fake_mugsy_run] in
  Lwt.return fake_mugsy_run
  
let can_split_child max_seqs child =
  List.length (Mugsy_guide_tree.list_of_guide_tree child) > max_seqs

let rec run_tree options depth = function
  | H_taxonomic_unit (l, r) as subtree -> begin
    lwt () = 
      Lwt_io.printf 
	"--- Processing depth: %d Num seqs: %d\n" 
	depth 
	(List.length (Mugsy_guide_tree.list_of_guide_tree subtree))
    in
    let node_name = Global_state.make_ref () in
    let can_split_subtree = can_split_child options.seqs_per_mugsy subtree in
    if not can_split_subtree then begin
      (* An ancestral node is one where we cannot break up the tree any more *)
      let sequences = 
	List.append 
	  (Mugsy_guide_tree.list_of_guide_tree l) 
	  (Mugsy_guide_tree.list_of_guide_tree r) 
      in
      lwt nucmers = run_nucmers_and_wait options depth (Fileutils.join [options.out_dir; node_name]) sequences in
      lwt mugsy_run = 
	run_mugsy_and_wait
	  options
	  depth
	  (Fileutils.join [options.out_dir; node_name])
	  sequences 
	  (List.map ~f:(fun x -> x.search_maf) (list_of_tuple_map nucmers)) in
      Lwt.return mugsy_run
    end
    else begin
      (* Run everything underneath us and then work on it *)
      let run_tree_or_mugsy (subtree, splitable) =
	match Mugsy_guide_tree.list_of_guide_tree subtree with
	  | [seq] ->
	    run_fake_mugsy_and_wait 
	      options 
	      depth
	      seq 
	      (Fileutils.join [options.out_dir; node_name; "fake_mugsy.maf"])
	  | _seqs when splitable ->
	    run_tree options (depth + 1) subtree
	  | seqs ->
	    lwt nucmers = run_nucmers_and_wait options depth (Fileutils.join [options.out_dir; node_name]) seqs in
	    let node_name' = Global_state.make_ref () in
	    run_mugsy_and_wait
	      options
	      depth
	      (Fileutils.join [options.out_dir; node_name'])
	      seqs
	      (List.map ~f:(fun x -> x.search_maf) (list_of_tuple_map nucmers)) 
      in
      let can_split_l = can_split_child options.seqs_per_mugsy l in
      let can_split_r = can_split_child options.seqs_per_mugsy r in
      let left_sequences = Mugsy_guide_tree.list_of_guide_tree l in
      let right_sequences = Mugsy_guide_tree.list_of_guide_tree r in
      lwt subtree = Lwt_list.map_p run_tree_or_mugsy [(l, can_split_l); (r, can_split_r)]
      and remaining_nucmers =
	run_nucmer_left_against_right_and_wait
	  options 
	  depth
	  (Fileutils.join [options.out_dir; node_name])
	  left_sequences
	  right_sequences
      in
      match subtree with
	| [left_mugsy; right_mugsy] ->
	  run_mugsy_with_profiles_and_wait
	    options
	    depth
	    (Fileutils.join [options.out_dir; node_name])
	    left_mugsy.mugsy_maf
	    right_mugsy.mugsy_maf
	    (List.map ~f:(fun x -> x.search_delta) (list_of_tuple_map remaining_nucmers))
	| _ -> raise (Failure "This should never happen")
    end
  end
  | Taxonomic_unit _ ->
    raise (Failure "Should never get here")

  

let run_sge argv =
  let options = parse_argv argv in
  Shell.mkdir_p options.out_dir;
  Shell.mkdir_p options.tmp_dir;
  let sequences = rewrite_sequences options.sequences options.tmp_dir in
  let guide_tree = Mugsy_guide_tree.guide_tree_of_sequences sequences in
  let copy_file f = 
    Lwt_process.exec 
      ~stdin:`Close 
      ~stdout:`Dev_null 
      (Lwt_process.shell 
	 (Printf.sprintf "cp %s %s" f options.out_maf)) >>= fun _ -> Lwt.return options.out_maf
  in
  let final_maf = Lwt_main.run (run_tree options 0 guide_tree >>= fun m -> copy_file m.mugsy_maf) in
  print_endline final_maf

