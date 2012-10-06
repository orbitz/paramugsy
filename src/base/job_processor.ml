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


  let run_nucmers qts tmp_dir nucmer_chunk job_tree =
    let nucmer_searches = chunk nucmer_chunk (Pm_job.pairwise job_tree) in
    let module Nt = Nucmer_task in
    let mk_task searches =
      let node_name = Global_state.make_count () in
      let base_dir  = Fileutils.join [tmp_dir; node_name] in
      Shell.mkdir ~p:() base_dir;
      Nt.make {Nt.searches = searches; tmp_dir = base_dir}
    in
    let tasks   = List.map ~f:mk_task nucmer_searches in
    let running = List.map ~f:(run_task qts) tasks in
    Deferred.List.all running >>= fun res ->
    Deferred.return (Result.all res)

  let make_job_tree seqs_per_mugsy seq_list =
    In_thread.run
      (fun () -> Pm_job.make_job seqs_per_mugsy seq_list)

  let rec process_tree t qts genome_map = function
    | Pm_job.Job_tree.Nil ->
      Deferred.return (Result.Ok ())
    | Pm_job.Job_tree.Mugsy_profile (left, right) ->
      process_mugsy_profile t qts (left, right)
    | Pm_job.Job_tree.Mugsy genomes ->
      process_genomes t qts genomes
    | Pm_job.Job_tree.Fake_mugsy genome ->
      Deferred.return (Result.Ok ())
  and process_mugsy_profile t qts (left, right) =
    let job_tree = Pm_job.Job_tree.Mugsy_profile (left, right) in
    run_nucmers qts t.tmp_dir t.nucmer_chunk job_tree >>= function
      | Result.Ok () ->
	Deferred.return (Result.Ok ())
      | Result.Error err ->
	Deferred.return (Result.Error err)
  and process_genomes t qts genomes =
    let job_tree = Pm_job.Job_tree.Mugsy genomes in
    run_nucmers qts t.tmp_dir t.nucmer_chunk job_tree >>= function
      | Result.Ok () ->
	Deferred.return (Result.Ok ())
      | Result.Error err ->
	Deferred.return (Result.Error err)

  let run t =
    make_job_tree t.seqs_per_mugsy t.seq_list  >>= fun job ->
    ignore (Pm_job.pp_stdout job);
    let qts        = Qts.start t.run_size in
    let job_tree   = job.Pm_job.job_tree in
    let genome_map = job.Pm_job.genome_map in
    process_tree t qts genome_map job_tree >>= fun res ->
    Qts.stop qts                           >>= fun () ->
    match res with
      | Result.Ok ()   -> Deferred.return 0
      | Result.Error _ -> Deferred.return 1
end

