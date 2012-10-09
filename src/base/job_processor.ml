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
	 ; logger         : string -> unit
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

  let collect_nucmer_mafs nucmers =
    let mash ~init =
      String.Map.fold
	~f:(fun ~key ~data acc ->
	  String.Map.add ~key ~data acc)
	~init
    in
    let mashed =
      List.fold_left
	~f:(fun acc -> mash ~init:acc)
	~init:String.Map.empty
	nucmers
    in
    List.fold_left
      ~f:(fun acc k ->
	if String.is_suffix ~suffix:"-maf" k then
	  (String.Map.find_exn mashed k)::acc
	else
	  acc)
      ~init:[]
      (String.Map.keys mashed)

  let collect_nucmer_deltas nucmers =
    let mash ~init =
      String.Map.fold
	~f:(fun ~key ~data acc ->
	  String.Map.add ~key ~data acc)
	~init
    in
    let mashed =
      List.fold_left
	~f:(fun acc -> mash ~init:acc)
	~init:String.Map.empty
	nucmers
    in
    List.fold_left
      ~f:(fun acc k ->
	if String.is_suffix ~suffix:"-delta" k then
	  (String.Map.find_exn mashed k)::acc
	else
	  acc)
      ~init:[]
      (String.Map.keys mashed)

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

  let run_mugsy t qts priority genomes nucmers =
    let module Mt = Mugsy_task in
    let node_name = Global_state.make_count () in
    let base_dir = Fileutils.join [t.tmp_dir; node_name] in
    Shell.mkdir ~p:() base_dir;
    Mt.make { Mt.seqs      = genomes
	    ;    mafs      = nucmers
	    ;    distance  = t.distance
	    ;    minlength = t.minlength
	    ;    tmp_dir   = base_dir
	    }
    >>= function
      | Result.Ok script_task -> begin
	let task = { Task.template_file = t.template_file
		   ;      exec_queue    = t.exec_q
		   ;      data_queue    = t.data_q
		   ;      task          = script_task
		   }
	in
	t.logger "Running Mugsy";
	run_task t.tmp_dir qts priority task >>= function
	  | Queue_job.Job_status.Completed ->
	    Deferred.return (Result.Ok task.Task.task.Script_task.out_paths)
	  | Queue_job.Job_status.Failed ->
	    Deferred.return (Result.Error ())
      end
      | Result.Error () ->
	Deferred.return (Result.Error ())

  let run_mugsy_profiles t qts priority left_maf right_maf nucmer_deltas =
    let module Mpt = Mugsy_profiles_task in
    let node_name  = Global_state.make_count () in
    let base_dir   = Fileutils.join [t.tmp_dir; node_name] in
    Shell.mkdir ~p:() base_dir;
    Mpt.make { Mpt.left_maf      = left_maf
	     ;     right_maf     = right_maf
	     ;     nucmer_deltas = nucmer_deltas
	     ;     distance      = t.distance
	     ;     minlength     = t.minlength
	     ;     tmp_dir       = base_dir
	     }
    >>= function
      | Result.Ok script_task -> begin
	let task = { Task.template_file = t.template_file
		   ;      exec_queue    = t.exec_q
		   ;      data_queue    = t.data_q
		   ;      task          = script_task
		   }
	in
	t.logger "Running Mugsy profiles";
	run_task t.tmp_dir qts priority task >>= function
	  | Queue_job.Job_status.Completed ->
	    Deferred.return (Result.Ok task.Task.task.Script_task.out_paths)
	  | Queue_job.Job_status.Failed ->
	    Deferred.return (Result.Error ())
      end
      | Result.Error () ->
	Deferred.return (Result.Error ())

  let run_fake_mugsy t qts priority genome =
    let module Fmt = Fake_mugsy_task in
    let node_name = Global_state.make_count () in
    let base_dir = Fileutils.join [t.tmp_dir; node_name] in
    Shell.mkdir ~p:() base_dir;
    Fmt.make {Fmt.fasta = genome; tmp_dir = base_dir} >>= function
      | Result.Ok script_task -> begin
	let task = { Task.template_file = t.template_file
		   ;      exec_queue    = t.exec_q
		   ;      data_queue    = t.data_q
		   ;      task          = script_task
		   }
	in
	run_task t.tmp_dir qts priority task >>= function
	  | Queue_job.Job_status.Completed ->
	    Deferred.return (Result.Ok task.Task.task.Script_task.out_paths)
	  | Queue_job.Job_status.Failed ->
	    Deferred.return (Result.Error ())
      end
      | Result.Error () ->
	Deferred.return (Result.Error ())

  let make_job_tree seqs_per_mugsy seq_list =
    Pm_job.make_job seqs_per_mugsy seq_list

  let rec process_tree t qts priority = function
    | Pm_job.Nil ->
      Deferred.return (Result.Error ())
    | Pm_job.Mugsy_profile (left, right) ->
      process_mugsy_profile t qts priority (left, right)
    | Pm_job.Mugsy genomes ->
      process_genomes t qts priority genomes
    | Pm_job.Fake_mugsy genome ->
      process_fake_mugsy t qts priority genome
  and process_mugsy_profile t qts priority (left, right) =
    let job_tree  = Pm_job.Mugsy_profile (left, right) in
    let left_ret  = background (process_tree t qts (priority + 1) left) in
    let right_ret = background (process_tree t qts (priority + 1) right) in
    t.logger (Printf.sprintf "Mugsy_profile Priority: %d - Nucmer: Running" priority);
    run_nucmers t qts priority job_tree >>= function
      | Result.Ok nucmers -> begin
	t.logger (Printf.sprintf "Priority: %d - Nucmer: Finished" priority);
	let nucmer_deltas = collect_nucmer_deltas nucmers in
	Ivar.read left_ret  >>= fun left_val ->
	Ivar.read right_ret >>= fun right_val ->
	match (left_val, right_val) with
	  | (Result.Ok left_maf, Result.Ok right_maf) -> begin
	    run_mugsy_profiles t qts priority left_maf right_maf nucmer_deltas >>= function
	      | Result.Ok out_paths ->
		Deferred.return (Result.Ok (String.Map.find_exn out_paths "maf"))
	      | Result.Error _ -> begin
		t.logger (Printf.sprintf "Priority: %d - Mugsy_profiles: Failed" priority);
		Deferred.return (Result.Error ())
	      end
	  end
	  | _ -> begin
	    t.logger (Printf.sprintf "Priority: %d - Mugsy_profiles: Failed" priority);
	    Deferred.return (Result.Error ())
	  end
      end
      | Result.Error _ -> begin
	t.logger (Printf.sprintf "Priority: %d - Nucmer: Failed" priority);
	Deferred.return (Result.Error ())
      end
  and process_genomes t qts priority genomes =
    let job_tree = Pm_job.Mugsy genomes in
    t.logger (Printf.sprintf "Mugsy Priority: %d - Nucmer: Running" priority);
    run_nucmers t qts priority job_tree >>= function
      | Result.Ok nucmers -> begin
	t.logger (Printf.sprintf "Priority: %d - Nucmer: Finished" priority);
	let nucmer_mafs = collect_nucmer_mafs nucmers in
	run_mugsy t qts priority genomes nucmer_mafs >>= function
	  | Result.Ok out_paths ->
	    Deferred.return (Result.Ok (String.Map.find_exn out_paths "maf"))
	  | Result.Error _ -> begin
	    t.logger (Printf.sprintf "Priority: %d - Mugsy: Failed" priority);
	    Deferred.return (Result.Error ())
	  end
      end
      | Result.Error _ -> begin
	t.logger (Printf.sprintf "Priority: %d - Nucmer: Failed" priority);
	Deferred.return (Result.Error ())
      end
  and process_fake_mugsy t qts priority genome =
    t.logger "Fake Mugsy";
    run_fake_mugsy t qts priority genome >>= function
      | Result.Ok out_paths ->
	Deferred.return (Result.Ok (String.Map.find_exn out_paths "maf"))
      | Result.Error _ ->
	Deferred.return (Result.Error ())

  let run t =
    make_job_tree t.seqs_per_mugsy t.seq_list >>= fun job_tree ->
    ignore (Pm_job.pp t.logger job_tree);
    let qts = Qts.start t.run_size in
    process_tree t qts 0 job_tree >>= fun res ->
    Qts.stop qts                  >>| fun () ->
    match res with
      | Result.Ok maf -> begin
	t.logger "Succeeded";
	Copy_file.copy_file maf t.out_maf >>> fun _ ->
	t.logger t.out_maf;
	never_returns (Shutdown.shutdown_and_raise 0)
      end
      | Result.Error _ -> begin
	t.logger "Failed";
	never_returns (Shutdown.shutdown_and_raise 1)
      end
end
