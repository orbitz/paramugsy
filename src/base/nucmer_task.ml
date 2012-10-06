open Core.Std
open Async.Std

open Ort

type t = { searches : (Fileutils.file_path * Fileutils.file_path) list
	 ; tmp_dir  : Fileutils.file_path
	 }

let basename ref_seq query_seq =
  Fileutils.basename ref_seq ^ "-" ^ Fileutils.basename query_seq

let write_in_file_list fname sequences =
  let files =
    List.fold_left
      ~f:(fun acc (ref_seq, query_seq) ->
	ref_seq::query_seq::acc)
      ~init:[]
      sequences
  in
  let file_str = String.concat ~sep:"\n" files in
  Writer.with_file
    fname
    ~f:(fun w -> Deferred.return (Writer.write w file_str))

let write_out_file_list fname base_dir sequences =
  let files =
    List.fold_left
      ~f:(fun acc (ref_seq, query_seq) ->
	let bname = basename ref_seq query_seq in
	let maf   = Fileutils.join [base_dir; bname ^ ".maf"] in
	let delta = Fileutils.join [base_dir; bname ^ ".delta"] in
	maf::delta::acc)
      ~init:[]
      sequences
  in
  let files_str = String.concat ~sep:"\n" files in
  Writer.with_file
    fname
    ~f:(fun w -> Deferred.return (Writer.write w files_str))

let make t =
  let node_name = Global_state.make_count () in
  let base_dir  = Fileutils.join [t.tmp_dir; node_name] in
  let create_command (ref_seq, query_seq) =
    let bname    = basename ref_seq query_seq in
    let out_dir  = Fileutils.join [base_dir; bname] in
    Printf.sprintf
      "mugsy_nucmer -ref_seq %s -query_seq %s -out_dir %s -tmp_dir %s -maf_out %s.maf -delta_out %s.delta"
      ref_seq
      query_seq
      base_dir
      out_dir
      bname
      bname
  in
  let in_file_list =
    Fileutils.join [ t.tmp_dir
		   ; node_name ^ "_in.list"
		   ]
  in
  let out_file_list =
    Fileutils.join [ t.tmp_dir
		   ; node_name ^ "_out.list"
		   ]
  in
  let nucmer_cmds = List.map ~f:create_command t.searches in
  write_in_file_list in_file_list t.searches            >>= fun () ->
  write_out_file_list out_file_list base_dir t.searches >>= fun () ->
  let module S = Script_task in
  Deferred.return
    (Result.Ok
       { S.name      = Queue_job.Name.of_string (Global_state.make_count ())
       ;   pre       = []
       ;   post      = []
       ;   body      = nucmer_cmds
       ;   in_files  = [ { S.Copy_file.file_list = in_file_list
			 ;             src_path  = "/"
			 ;             dst_path  = "/"
			 }]
       ;   out_files = [ { S.Copy_file.file_list = out_file_list
			 ;             src_path  = "/"
			 ;             dst_path  = "/"
			 }]
       })
