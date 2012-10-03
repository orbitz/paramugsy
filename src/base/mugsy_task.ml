open Core.Std

open Ort

module Shell = Core_extended.Std.Shell

type t = { seqs      : Fileutils.file_path list
	 ; mafs      : Fileutils.file_path list
	 ; distance  : int
	 ; minlength : int
	 ; tmp_dir   : Fileutils.file_path
	 }


let make t =
  let node_name      = Global_state.make_count () in
  let base_dir       = Fileutils.join [t.tmp_dir; node_name] in
  let seqs_file_list = Fileutils.join [base_dir; "mugsy.seqs"] in
  let mafs_file_list = Fileutils.join [base_dir; "mugsy.mafs"] in
  Shell.mkdir ~p:() base_dir;
  Pm_file.write_lines seqs_file_list t.seqs >>= fun () ->
  Pm_file.write_lines mafs_file_list t.mafs >>= fun () ->
  let cmds = [ Printf.sprintf
	       "echo Start-Single %s `date %s`"
	       base_dir
	       "+%Y%m%d%H%M%S"
	     ; Printf.sprintf
	       "mugsy_mugsy -out_dir %s -seq_list %s -maf_list %s -minlenght %d -distance %d -debug"
	       base_dir
	       seqs_file_list
	       mafs_file_list
	       minlength
	       distance
	     ; Printf.sprintf
	       "echo End-Single %s `date %s`"
	       base_dir
	       "+%Y%m%d%H%M%S"
	     ]
  in
  let in_file_list = Fileutils.join [t.tmp_dir; node_name ^ "_in.list"] in
  Pm_file.write_lines in_file_list (t.seqs @ t.mafs @ [seqs_file_list; mafs_file_list]);
  let in_files = [{
