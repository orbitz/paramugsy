(*pp camlp4o `ocamlfind query -i-format lwt` `ocamlfind query -predicates syntax,preprocessor -a-format -r lwt.syntax` *)
open Core_extended
open Core_extended.Std
open Lwt
open Ort

type search_files = (Fileutils.file_path * Fileutils.file_path)

type nucmer_job = { job_id : Pm_sge_utils.job_id
		  ; search_files : search_files
		  ; output_maf : Fileutils.file_path
		  ; output_delta : Fileutils.file_path
		  }

let run_nucmer sge_options sequences =
  let node_name = Global_state.make_ref () in
  let base_dir = Fileutils.join [sge_options.Pm_sge_utils.tmp_dir; node_name] in
  lwt _ = Copy_file.mkdir_p base_dir in
  lwt seqs_copied =
    Lwt_list.map_s
      (fun (ref_seqs, query_seqs) -> 
	let ref_seq_dest = Fileutils.join [base_dir; Fileutils.basename ref_seqs] in
	let query_seq_dest = Fileutils.join [base_dir; Fileutils.basename query_seqs] in
	lwt _ = Copy_file.copy_file ref_seqs ref_seq_dest in
	lwt _ = Copy_file.copy_file query_seqs query_seq_dest in
	Lwt.return (ref_seq_dest, query_seq_dest))
      sequences
  in
  let create_command (ref_seq, query_seq) =
    let ref_in = Fileutils.join [base_dir; Fileutils.basename ref_seq] in
    let query_in = Fileutils.join [base_dir; Fileutils.basename query_seq] in
    let basename = Fileutils.basename ref_seq ^ "-" ^ Fileutils.basename query_seq in
    Printf.sprintf 
      "mugsy_nucmer -ref_seq %s -query_seq %s -out_dir %s -tmp_dir %s/%s -maf_out %s.maf -delta_out %s.delta"
      ref_in
      query_in
      base_dir
      sge_options.Pm_sge_utils.tmp_dir
      basename
      basename
      basename
  in
  let in_files = [(base_dir, base_dir)] in
  let out_files = [(base_dir, Fileutils.dirname base_dir)]in
  let commands =
    List.fold_left
      ~f:(fun acc s ->
	(create_command s)::acc)
      ~init:[]
      seqs_copied
  in
  lwt job_id = 
    Pm_sge_utils.qsub_with_datasync
      ~options:sge_options
      ~wait:false
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
		seqs_copied)
  

let wait_on_nucmer_jobs nucmer_jobs =
  Lwt_list.map_s (fun job -> Pm_sge_utils.wait_on_job_id job.job_id) nucmer_jobs >>= fun _ -> Lwt.return nucmer_jobs
