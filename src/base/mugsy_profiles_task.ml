open Core.Std
open Async.Std

open Ort

type t = { left_maf      : Fileutils.file_path
	 ; right_maf     : Fileutils.file_path
	 ; nucmer_deltas : Fileutils.file_path list
	 ; distance      : int
	 ; minlength     : int
	 ; tmp_dir       : Fileutils.file_path
	 }

let make t =
  let module S       = Script_task in
  let module Cf      = S.Copy_file in
  let profiles_left  = Fileutils.join [t.tmp_dir; "profiles-l"] in
  let profiles_right = Fileutils.join [t.tmp_dir; "profiles-r"] in
  let fasta_left = Fileutils.join [profiles_left; "sequences.fasta"] in
  let fasta_right = Fileutils.join [profiles_right; "sequences.fasta"] in
  let nucmer_file_list = Fileutils.join [t.tmp_dir; "nucmer.list"] in
  let seqs_file_list = Fileutils.join [t.tmp_dir; "seqs.list"] in
  let maf_file_list = Fileutils.join [t.tmp_dir; "maf.list"] in
  let untranslate_file_list = Fileutils.join [t.tmp_dir; "untranslate.list"] in
  let untranslated_maf = Fileutils.join [t.tmp_dir; "untranslated.maf"] in
  let in_file_list = Fileutils.join [t.tmp_dir; "in.list"] in
  let in_files = [t.left_maf; t.right_maf; nucmer_file_list; seqs_file_list; maf_file_list; untranslate_file_list] @ t.nucmer_deltas in
  let out_file_list = Fileutils.join [t.tmp_dir; "out.list"] in
  let out_files = [untranslated_maf] in
  Pm_file.write_lines nucmer_file_list t.nucmer_deltas >>= fun () ->
  Pm_file.write_lines seqs_file_list [fasta_left; fasta_right] >>= fun () ->
  let cmds =
    [ Printf.sprintf
      "echo Start-Multi %s `date %s`"
      t.tmp_dir
      "+%Y%m%d%H%M%S"
    ; Printf.sprintf
      "mugsy_profiles make -in_maf %s -out_dir %s -basename l"
      t.left_maf
      profiles_left
    ; Printf.sprintf
      "mugsy_profiles make -in_maf %s -out_dir %s -basename r"
      t.right_maf
      profiles_right
    ; Printf.sprintf
      "m_translate %s %s %s %s/profile.delta"
      profiles_left
      profiles_right
      nucmer_file_list
      t.tmp_dir
    ; Printf.sprintf
      "delta2maf %s/profile.delta > %s/profile.maf"
      t.tmp_dir
      t.tmp_dir
    ; Printf.sprintf
      "mugsy_mugsy -out_dir %s/mugsy -seq_list %s -maf_list %s -minlength %d -distance %d -debug"
      t.tmp_dir
      seqs_file_list
      maf_file_list
      t.minlength
      t.distance
    ; Printf.sprintf
      "mugsy_profiles untranslate -profile_paths_list %s -in_maf %s/mugsy/mugsy.maf -out_maf %s"
      untranslate_file_list
      t.tmp_dir
      untranslated_maf
    ; Printf.sprintf
      "rm -rf %s/profile.delta %s/profile.maf %s/mugsy/mugsy.all.fsa %s/mugsy/mugsy.xmfa %s %s"
      t.tmp_dir
      t.tmp_dir
      t.tmp_dir
      t.tmp_dir
      profiles_left
      profiles_right
    ; Printf.sprintf
      "echo End-Multi %s `date %s`"
      t.tmp_dir
      "+%Y%m%d%H%M%S"
    ]
  in
  Pm_file.write_lines in_file_list in_files >>= fun () ->
  Pm_file.write_lines out_file_list out_files >>= fun () ->
  Deferred.return
    (Result.Ok
       { S.name      = Queue_job.Name.of_string (Global_state.make_count ())
       ;   pre       = []
       ;   post      = []
       ;   body      = cmds
       ;   in_files  = [{ Cf.file_list = in_file_list
			;    src_path = "/"
			;    dst_path = "/"
			}]
       ;   out_files = [{ Cf.file_list = out_file_list
			;    src_path = "/"
			;    dst_path = "/"
			}]
       ;   out_paths = String.Map.add ~key:"maf" ~data:untranslated_maf String.Map.empty
       })
