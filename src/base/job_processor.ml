open Core.Std
open Async.Std

open Ort

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


module Copy_file : sig
  type t = { file_list : Fileutils.file_path
	   ; src_path  : Fileutils.file_path
	   ; dst_path  : Fileutils.file_path
	   }
end

module Command : sig
  type t = string
end

module Task : sig
  type t  = { name          : Queue_job.Name.t
	    ; template_file : Fileutils.file_path
            ; script_dir    : Fileutils.file_path
            ; exec_queue    : Queue_job.Queue.t
            ; data_queue    : Queue_job.Queue.t option
	    ; task          : Script_task.t
            }
end

module Make = functor (Sts : SCRIPT_TASK_SERVER) -> struct
  module Queued_task_server = Queued_task_server.Make(Sts)

  let run_task qts task =
    let cmds   = Script_task.to_string ~data_queue:task.Task.data_queue task.Task.task in
    let dir    = Fileutils.join [task.Task.script_dir; "q_job"] in
    let script = Fileutils.join [dir; Printf.sprintf "q%05s.sh" (Global_state.make_count ())] in
    Shell.mkdir ~p:() dir;
    submit_count := !submit_count + 1;
    Writer.with_file
      script
      ~f:(fun w -> Deferred.return (Writer.write w cmds)) >>= fun () ->
    ignore (Unix.chmod ~perm:0o555 script);
    Queued_task_server.submit ~n:task.Task.name ~q:task.Task.exec_queue script qts

  let run t =
    let qts = Queued_task_server.start t.run_size in
    Queued_task_server.stop qts

end

