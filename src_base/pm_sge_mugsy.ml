(*pp camlp4o `ocamlfind query -i-format lwt` `ocamlfind query -predicates syntax,preprocessor -a-format -r lwt.syntax` *)
open Core_extended
open Core_extended.Std
open Lwt
open Ort

type mugsy_job = { job_id : Pm_sge_utils.job_id
		 ; mugsy_maf : Fileutils.file_path
		 }

let write_lines lines file =
  let fout = open_out file in
  List.iter ~f:(Printf.fprintf fout "%s\n") lines;
  close_out fout

let run_mugsy ~distance ~minlength options seqs mafs =
  let node_name = Global_state.make_ref () in
  let base_dir = Fileutils.join [options.Pm_sge_utils.tmp_dir; node_name] in
  lwt _ = Copy_file.mkdir_p base_dir in
  let seqs_file_list = Fileutils.join [base_dir; "mugsy.seqs"] in
  let mafs_file_list = Fileutils.join [base_dir; "mugsy.mafs"] in
  lwt seqs_copied =
    Lwt_list.map_s
      (fun f -> Copy_file.copy_file f (Fileutils.join [base_dir; Fileutils.basename f]))
      seqs
  in
  lwt mafs_copied =
    Lwt_list.map_s
      (fun f -> Copy_file.copy_file f (Fileutils.join [base_dir; Fileutils.basename f]))
      mafs
  in
  write_lines seqs_copied seqs_file_list;
  write_lines mafs_copied mafs_file_list;
  let commands = [ Printf.sprintf "echo Start-Single %d %s `date %s`" 
		     options.Pm_sge_utils.priority 
		     base_dir
		     "+%Y%m%d%H%M%S"
		 ; Printf.sprintf
		   "mugsy_mugsy -out_dir %s -seq_list %s -maf_list %s -minlength %d -distance %d -debug"
		   base_dir
		   seqs_file_list
		   mafs_file_list
		   minlength
		   distance
 		 ; Printf.sprintf "echo End-Single %d %s `date %s`" 
		   options.Pm_sge_utils.priority 
		   base_dir 
		   "+%Y%m%d%H%M%S"
		 ]
  in
  let in_files = [(base_dir, Fileutils.dirname base_dir)] in
  let out_files = in_files in
  lwt job_id = 
    Pm_sge_utils.qsub_with_datasync
      ~options:options
      ~wait:false
      ~in_files:in_files
      ~out_files:out_files
      commands
  in
  Lwt.return { job_id = job_id
	     ; mugsy_maf = Fileutils.join [base_dir; "mugsy.maf"]
	     }


let run_mugsy_with_profiles ~distance ~minlength options left_maf right_maf nucmer_deltas =
  let node_name = Global_state.make_ref () in
  let base_dir = Fileutils.join [options.Pm_sge_utils.tmp_dir; node_name] in
  lwt _ = Copy_file.mkdir_p base_dir in
  lwt left_maf_copied = 
    Copy_file.copy_file 
      left_maf 
      (Fileutils.join [base_dir; Fileutils.basename left_maf ^ Global_state.make_ref ()])
  in
  lwt right_maf_copied = 
    Copy_file.copy_file 
      right_maf 
      (Fileutils.join [base_dir; Fileutils.basename right_maf ^ Global_state.make_ref ()])
  in
  let profiles_left = Fileutils.join [base_dir; "profiles-l"] in
  let profiles_right = Fileutils.join [base_dir; "profiles-r"] in
  let nucmer_file_list = Fileutils.join [base_dir; "nucmer.list"] in
  let seqs_file_list = Fileutils.join [base_dir; "seqs.list"] in
  let maf_file_list = Fileutils.join [base_dir; "maf.list"] in
  let untranslate_file_list = Fileutils.join [base_dir; "untranslate.list"] in
  let untranslate_maf = Fileutils.join [base_dir; "untranslated.maf"] in
  write_lines nucmer_deltas nucmer_file_list;
  write_lines
    [ Fileutils.join [profiles_left; "l.fasta"]
    ; Fileutils.join [profiles_right; "r.fasta"]
    ]
    seqs_file_list;
  write_lines [Fileutils.join [base_dir; "profile.maf"]] maf_file_list;
  write_lines [profiles_left; profiles_right] untranslate_file_list;
  let commands = [ Printf.sprintf 
		     "echo Start-Multi %d %s `date %s`" 
		     options.Pm_sge_utils.priority 
		     base_dir 
		     "+%Y%m%d%H%M%S"
		 ; Printf.sprintf "mugsy_profiles make -in_maf %s -out_dir %s -basename l" left_maf_copied profiles_left
		 ; Printf.sprintf "mugsy_profiles make -in_maf %s -out_dir %s -basename r" right_maf_copied profiles_right
		 ; Printf.sprintf
		   "m_translate %s %s %s %s/profile.delta"
		   profiles_left
		   profiles_right
		   nucmer_file_list
		   base_dir
		 ; Printf.sprintf "delta2maf %s/profile.delta > %s/profile.maf" base_dir base_dir
		 ; Printf.sprintf
		   "mugsy_mugsy -out_dir %s/mugsy -seq_list %s -maf_list %s -minlength %d -distance %d -debug"
		   base_dir
		   seqs_file_list
		   maf_file_list
		   minlength
		   distance
		 ; Printf.sprintf 
		   "mugsy_profiles untranslate -profile_paths_list %s -in_maf %s/mugsy/mugsy.maf -out_maf %s" 
		   untranslate_file_list 
		   base_dir
		   untranslate_maf
		 ; Printf.sprintf "echo End-Multi %d %s `date %s`" options.Pm_sge_utils.priority base_dir "+%Y%m%d%H%M%S"
		 ]
  in
  let in_files = [(base_dir, Fileutils.dirname base_dir)] in
  let out_files = [(untranslate_maf, Fileutils.dirname untranslate_maf)] in
  lwt job_id = 
    Pm_sge_utils.qsub_with_datasync
      ~options:options
      ~wait:false
      ~in_files:in_files
      ~out_files:out_files
      commands
  in
  Lwt.return { job_id = job_id
	     ; mugsy_maf = untranslate_maf
	     }
  
let run_fake_mugsy options in_fasta =
  let node_name = Global_state.make_ref () in
  let base_dir = Fileutils.join [options.Pm_sge_utils.tmp_dir; node_name] in
  lwt _ = Copy_file.mkdir_p base_dir in
  lwt in_fasta_copied = 
    Copy_file.copy_file 
      in_fasta 
      (Fileutils.join [base_dir; Fileutils.basename in_fasta])
  in
  let out_maf = Fileutils.join [base_dir; "fake_mugsy.maf"] in
  let commands = [Printf.sprintf
		     "mugsy_profiles fasta_to_maf -in_fasta %s -out_maf %s"
		     in_fasta
		     out_maf
		 ]
  in
  let in_files = [(base_dir, Fileutils.basename base_dir)] in
  let out_files = in_files in
  lwt job_id = 
    Pm_sge_utils.qsub_with_datasync
      ~options:options
      ~wait:false
      ~in_files:in_files
      ~out_files:out_files
      commands
  in
  Lwt.return { job_id = job_id
	     ; mugsy_maf = out_maf
	     }  
  

let wait_on_mugsy_job mugsy_job =
  Pm_sge_utils.wait_on_job_id mugsy_job.job_id >>= fun _ -> Lwt.return mugsy_job
