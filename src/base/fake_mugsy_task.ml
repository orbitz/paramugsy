open Core.Std
open Async.Std

open Ort

type t = { fasta   : Fileutils.file_path
	 ; tmp_dir : Fileutils.file_path
	 }

let make t =
  let module S  = Script_task in
  let module Cf = S.Copy_file in
  let out_maf   = Fileutils.join [t.tmp_dir; "fake_mugsy.maf"] in
  let cmds      = [ Printf.sprintf
		    "mugsy_profiles fasta_to_maf -in_fasta %s -out_maf %s"
		    t.fasta
		    out_maf
		  ]
  in
  let in_file_list = Fileutils.join [t.tmp_dir; "in.list"] in
  let out_file_list = Fileutils.join [t.tmp_dir; "out.list"] in
  Pm_file.write_lines in_file_list [t.fasta]  >>= fun () ->
  Pm_file.write_lines out_file_list [out_maf] >>= fun () ->
  Deferred.return
    (Result.Ok
       { S.name      = Queue_job.Name.of_string (Global_state.make_count ())
       ;   pre       = []
       ;   post      = []
       ;   body      = cmds
       ;   in_files  = [{ Cf.file_list = in_file_list
			;    src_path  = "/"
			;    dst_path  = "/"
			}]
       ;   out_files = [{ Cf.file_list = out_file_list
			;    src_path  = "/"
			;    dst_path  = "/"
			}]
       ;   out_paths = String.Map.add ~key:"maf" ~data:out_maf String.Map.empty
       })
