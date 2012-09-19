(*pp camlp4o `ocamlfind query -i-format lwt` `ocamlfind query -predicates syntax,preprocessor -a-format -r lwt.syntax` *)
open Core_extended
open Core.Std
open Lwt
open Ort

type search_files = (Fileutils.file_path * Fileutils.file_path)

type nucmer_job = { job_id : Pm_sge_utils.job_id
		  ; search_files : search_files
		  ; output_maf : Fileutils.file_path
		  ; output_delta : Fileutils.file_path
		  }


let write_lines lines file =
  let fout = open_out file in
  List.iter ~f:(Printf.fprintf fout "%s\n") lines;
  close_out fout


let run_nucmer sge_options sequences =
  let node_name = Global_state.make_ref () in
  let base_dir = Fileutils.join [sge_options.Pm_sge_utils.tmp_dir; node_name] in
  let create_command (ref_seq, query_seq) =
    let basename = Fileutils.basename ref_seq ^ "-" ^ Fileutils.basename query_seq in
    let tmp_dir = Fileutils.join [base_dir; basename] in
    Printf.sprintf 
      "mugsy_nucmer -ref_seq %s -query_seq %s -out_dir %s -tmp_dir %s -maf_out %s.maf -delta_out %s.delta"
      ref_seq
      query_seq
      base_dir
      tmp_dir
      basename
      basename
  in
  let out_sequences =
    List.fold_left
      ~f:(fun acc (ref_seq, query_seq) ->
	let basename = Fileutils.basename ref_seq ^ "-" ^ Fileutils.basename query_seq in
	let maf = Fileutils.join [base_dir; basename ^ ".maf"] in
	let delta = Fileutils.join [base_dir; basename ^ ".delta"] in
	maf::delta::acc)
      ~init:[]
      sequences
  in
  let in_file_list = Fileutils.join [sge_options.Pm_sge_utils.tmp_dir; node_name ^ "_in.list"] in
  let out_file_list = Fileutils.join [sge_options.Pm_sge_utils.tmp_dir; node_name ^ "_out.list"] in
  write_lines 
    (List.fold_left
       ~f:(fun acc (ref_seq, query_seq) -> ref_seq::query_seq::acc)
       ~init:[]
       sequences)
    in_file_list;
  write_lines out_sequences out_file_list;
  let in_files = [{ Pm_sge_utils.file_list = in_file_list
		  ; Pm_sge_utils.src_path = "/"
		  ; Pm_sge_utils.dst_path = "/"}]
  in
  let out_files = [{ Pm_sge_utils.file_list = out_file_list
		   ; Pm_sge_utils.src_path = "/"
		   ; Pm_sge_utils.dst_path = "/"}]
  in
  let commands =
    List.fold_left
      ~f:(fun acc s ->
	(create_command s)::acc)
      ~init:[]
      sequences
  in
  lwt job_id = 
    Pm_sge_utils.qsub_with_datasync
      ~wait:false
      ~options:sge_options
      ~in_files:in_files
      ~out_files:out_files
      commands
  in
  Lwt.return (List.fold_left
		~f:(fun acc (ref_seq, query_seq) ->
		  let basename = Fileutils.basename ref_seq ^ "-" ^ Fileutils.basename query_seq in
		  let out_maf = Fileutils.join [base_dir; basename ^ ".maf"] in
		  let out_delta = Fileutils.join [base_dir; basename ^ ".delta"] in
		  { job_id = job_id
		  ; search_files = (ref_seq, query_seq)
		  ; output_maf = out_maf
		  ; output_delta = out_delta
		  }::acc)
		~init:[]
		sequences)
  

let wait_on_nucmer_jobs nucmer_jobs =
  Lwt_list.map_s (fun job -> Pm_sge_utils.wait_on_job_id job.job_id) nucmer_jobs >>= fun _ -> Lwt.return nucmer_jobs
