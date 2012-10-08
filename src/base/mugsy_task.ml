open Core.Std
open Async.Std

open Ort

type t = { seqs      : Fileutils.file_path list
	 ; mafs      : Fileutils.file_path list
	 ; distance  : int
	 ; minlength : int
	 ; tmp_dir   : Fileutils.file_path
	 }


let make t =
  let module S       = Script_task in
  let module Cf      = S.Copy_file in
  let seqs_file_list = Fileutils.join [t.tmp_dir; "mugsy.seqs"] in
  let mafs_file_list = Fileutils.join [t.tmp_dir; "mugsy.mafs"] in
  Pm_file.write_lines seqs_file_list t.seqs >>= fun () ->
  Pm_file.write_lines mafs_file_list t.mafs >>= fun () ->
  let cmds =
    [ Printf.sprintf
      "echo Start-Single %s `date %s`"
      t.tmp_dir
      "+%Y%m%d%H%M%S"
    ; Printf.sprintf
      "mugsy_mugsy -out_dir %s -seq_list %s -maf_list %s -minlength %d -distance %d -debug"
      t.tmp_dir
      seqs_file_list
      mafs_file_list
      t.minlength
      t.distance
    ; Printf.sprintf
      "echo End-Single %s `date %s`"
      t.tmp_dir
      "+%Y%m%d%H%M%S"
    ]
  in
  let in_file_list  = Fileutils.join [t.tmp_dir; "in.list"] in
  let out_file_list = Fileutils.join [t.tmp_dir; "out.list"] in
  let in_files      = t.seqs @ t.mafs @ [seqs_file_list; mafs_file_list] in
  let out_maf       = Fileutils.join [t.tmp_dir; "mugsy.maf"] in
  Pm_file.write_lines in_file_list in_files                                   >>= fun () ->
  Pm_file.write_lines out_file_list [out_maf] >>= fun () ->
  let in_files = [{ Cf.file_list = in_file_list
		  ;    src_path  = "/"
		  ;    dst_path  = "/"
		  }]
  in
  let out_files = [{ Cf.file_list = out_file_list
		   ;    src_path  = "/"
		   ;    dst_path  = "/"
		   }]
  in
  Deferred.return
    (Result.Ok
       { S.name      = Queue_job.Name.of_string (Global_state.make_count ())
       ;   pre       = []
       ;   post      = []
       ;   body      = cmds
       ;   in_files  = in_files
       ;   out_files = out_files
       ;   out_paths = String.Map.add ~key:"maf" ~data:out_maf String.Map.empty
       })
