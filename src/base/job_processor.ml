open Core.Std
open Async.Std

open Ort
open Ort.Function

open Script_task_server

module Shell = Core_extended.Std.Shell

type t = { seq_list       : Fileutils.file_path list
	 ; run_size       : int
	 ; exec_q         : Queue_job.Queue.t
	 ; data_q         : Queue_job.Queue.t option
	 ; tmp_dir        : Fileutils.file_path
	 ; distance       : int
	 ; minlength      : int
	 ; template_file  : Fileutils.file_path
	 ; seqs_per_mugsy : int
	 ; nucmer_chunk   : int
	 ; out_maf        : Fileutils.file_path
	 }

module Task = struct
  type t  = { template_file : Fileutils.file_path
            ; script_dir    : Fileutils.file_path
            ; exec_queue    : Queue_job.Queue.t
            ; data_queue    : Queue_job.Queue.t option
	    ; task          : Script_task.t
            }
end

let chunk n l =
  l |> Seq.of_list |> Seq.chunk n |> Seq.to_list

module Make = functor (Sts : SCRIPT_TASK_SERVER) -> struct
  module Qts = Queued_task_server.Make(Sts)

  let run_task qts task =
    Pm_file.read_file task.Task.template_file >>= fun template ->
    let cmds =
      Script_task.to_string
        ~data_queue:task.Task.data_queue
	template
	task.Task.task
    in
    let dir    = Fileutils.join [task.Task.script_dir; "q_job"] in
    let script = Fileutils.join [dir; Printf.sprintf "q%s.sh" (Global_state.make_count ())] in
    Shell.mkdir ~p:() dir;
    Writer.with_file
      script
      ~f:(fun w -> Deferred.return (Writer.write w cmds)) >>= fun () ->
    ignore (Unix.chmod ~perm:0o555 script);
    Qts.submit
      ~n:task.Task.task.Script_task.name
      ~q:task.Task.exec_queue
      script
      qts

  let make_job_tree seqs_per_mugsy seq_list =
    In_thread.run
      (fun () -> Pm_job.make_job seqs_per_mugsy seq_list)

  let process_tree t = function
    | Pm_job.Job_tree.Nil ->
      Deferred.return ()
    | Pm_job.Job_tree.Mugsy_profile (left, right) as job_tree ->
      let nucmer_searches = chunk t.nucmer_chunk (Pm_job.pairwise job_tree) in
      Deferred.return ()
    | Pm_job.Job_tree.Mugsy genomes as job_tree ->
      let nucmer_searches = chunk t.nucmer_chunk (Pm_job.pairwise job_tree) in
      Deferred.return ()
    | Pm_job.Job_tree.Fake_mugsy genome ->
      Deferred.return ()


  let run t =
    make_job_tree t.seqs_per_mugsy t.seq_list  >>= fun job_tree ->
    ignore (Pm_job.pp_stdout job_tree);
    let qts = Qts.start t.run_size in
    Qts.stop qts >>= fun () ->
    Deferred.return 0
end

