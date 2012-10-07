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
	 ; logger         : 'a . ('a, unit, string, unit) format4 -> 'a
	 }

module Task = struct
  type t  = { template_file : Fileutils.file_path
            ; exec_queue    : Queue_job.Queue.t
            ; data_queue    : Queue_job.Queue.t option
	    ; task          : Script_task.t
            }
end

let chunk n l =
  l |> Seq.of_list |> Seq.chunk n |> Seq.to_list

module Make = functor (Sts : SCRIPT_TASK_SERVER) -> struct
  module Qts = Queued_task_server.Make(Sts)

  let run_task tmp_dir qts priority task =
    Pm_file.read_file task.Task.template_file >>= fun template ->
    let cmds =
      Script_task.to_string
        ~data_queue:task.Task.data_queue
	template
	task.Task.task
    in
    let dir    = Fileutils.join [tmp_dir; "q_job"] in
    let script = Fileutils.join [dir; Printf.sprintf "q%s.sh" (Global_state.make_count ())] in
    Shell.mkdir ~p:() dir;
    Writer.with_file
      script
      ~f:(fun w -> Deferred.return (Writer.write w cmds)) >>= fun () ->
    ignore (Unix.chmod ~perm:0o555 script);
    Qts.submit
      ~p:priority
      ~n:task.Task.task.Script_task.name
      ~q:task.Task.exec_queue
      script
      qts

  let background d =
    let ret = Ivar.create () in
    whenever (d >>| fun r -> Ivar.fill ret r);
    ret

  let collect_nucmer tasks results =
    let rec cn accum tasks results =
      let module Js = Queue_job.Job_status in
      match (tasks, results) with
	| (t::ts, Js.Completed::rs) ->
	  cn (t.Task.task.Script_task.out_paths::accum) ts rs
	| (_, Js.Failed::_) ->
	  Result.Error ()
	| ([], []) ->
	  Result.Ok accum
	| (_, _) ->
	  failwith "Number of jobs don't match"
    in
    cn [] tasks results

  let run_nucmers t qts priority job_tree =
    let nucmer_searches = chunk t.nucmer_chunk (Pm_job.pairwise job_tree) in
    let module Nt = Nucmer_task in
    let mk_task searches =
      let node_name = Global_state.make_count () in
      let base_dir  = Fileutils.join [t.tmp_dir; node_name] in
      Shell.mkdir ~p:() base_dir;
      Nt.make {Nt.searches = searches; tmp_dir = base_dir} >>= function
	| Result.Ok task ->
	  Deferred.return
	    (Result.Ok
	       { Task.template_file = t.template_file
	       ;      exec_queue    = t.exec_q
	       ;      data_queue    = t.data_q
	       ;      task          = task
	       })
	| Result.Error err ->
	  Deferred.return (Result.Error err)
    in
    Deferred.List.map ~f:mk_task nucmer_searches >>= fun task_result ->
    match Result.all task_result with
      | Result.Ok tasks ->
	let running = List.map ~f:(run_task t.tmp_dir qts priority) tasks in
	Deferred.List.all running >>= fun res ->
	Deferred.return (collect_nucmer tasks res)
      | Result.Error err ->
	Deferred.return (Result.Error err)

  let make_job_tree seqs_per_mugsy seq_list =
    Pm_job.make_job seqs_per_mugsy seq_list

  let rec process_tree t qts priority = function
    | Pm_job.Nil ->
      Deferred.return (Result.Ok ())
    | Pm_job.Mugsy_profile (left, right) ->
      process_mugsy_profile t qts priority (left, right)
    | Pm_job.Mugsy genomes ->
      process_genomes t qts priority genomes
    | Pm_job.Fake_mugsy genome ->
      Deferred.return (Result.Ok ())
  and process_mugsy_profile t qts priority (left, right) =
    let job_tree  = Pm_job.Mugsy_profile (left, right) in
    let left_ret  = background (process_tree t qts (priority + 1) left) in
    let right_ret = background (process_tree t qts (priority + 1) right) in
    run_nucmers t qts priority job_tree >>= function
      | Result.Ok nucmers -> begin
	Ivar.read left_ret  >>= fun left_val ->
	Ivar.read right_ret >>= fun right_val ->
	Deferred.return (Result.Ok ())
      end
      | Result.Error err ->
	Deferred.return (Result.Error err)
  and process_genomes t qts priority genomes =
    let job_tree = Pm_job.Mugsy genomes in
    run_nucmers t qts priority job_tree >>= function
      | Result.Ok nucmers ->
	Deferred.return (Result.Ok ())
      | Result.Error err ->
	Deferred.return (Result.Error err)

  let run t =
    t.logger "HERE? %s\n" "there";
    make_job_tree t.seqs_per_mugsy t.seq_list >>= fun job_tree ->
    t.logger "THERE\n";
    ignore (Pm_job.pp_stdout job_tree);
    let qts = Qts.start t.run_size in
    process_tree t qts 0 job_tree >>= fun res ->
    Qts.stop qts                  >>| fun () ->
    match res with
      | Result.Ok ()   -> never_returns (Shutdown.shutdown_and_raise 0)
      | Result.Error _ -> never_returns (Shutdown.shutdown_and_raise 1)
end

