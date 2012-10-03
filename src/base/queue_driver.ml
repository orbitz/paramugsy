open Core.Std
open Async.Std
open Ort

module Shell = Core_extended.Std.Shell

type copy_file = { file_list : Fileutils.file_path
		 ; src_path  : Fileutils.file_path
		 ; dst_path  : Fileutils.file_path
		 }

type command = string

module Task = struct
  type t  = { name          : Queue_job.Name.t
	    ; verbose       : bool
	    ; template_file : Fileutils.file_path
            ; script_dir    : Fileutils.file_path
            ; exec_queue    : Queue_job.Queue.t
            ; data_queue    : Queue_job.Queue.t option
	    ; pre           : command list
	    ; post          : command list
	    ; body          : command list
	    ; in_files      : copy_file list
	    ; out_files     : copy_file list
            }
end

module type QUEUE_SERVER = sig
  type t

  val start : unit -> t
  val stop  : t -> unit Deferred.t

  val run :
    n:Queue_job.Name.t ->
    q:Queue_job.Queue.t ->
    Fileutils.file_path ->
    t ->
    bool Deferred.t

  val wait : Queue_job.Name.t -> t -> Queue_job.Job_status.job_done option Deferred.t
  val ack  : Queue_job.Name.t -> t -> unit

end

module Make = functor (Qs : QUEUE_SERVER) -> struct
  type t = Qs.t

  (* We want a little counter for submissions *)
  let submit_count = ref 0

  let start () = Qs.start ()
  let stop qs  = Qs.stop qs

  let submit qs job =
    let cmds = Script_task.to_string ~data_queue:job.Task.data_queue job.Task.script in
    let dir  = Fileutils.join [job.Task.script_dir; "q_job"] in
    Shell.mkdir ~p:() dir;
    let script = Fileutils.join [dir; Printf.sprintf "q%05d.sh" !submit_count] in
    submit_count := !submit_count + 1;
    Writer.with_file
      script
      ~f:(fun w -> Deferred.return (Writer.write w cmds)) >>= fun () ->
    ignore (Unix.chmod ~perm:0o555 script);
    Qs.run job.Task.name job.Task.exec_queue script qs >>= function
      | true -> begin
	Qs.wait job.Task.name qs >>= function
	  | Some status -> begin
	    Qs.ack job.Task.name qs;
	    Deferred.return status
	  end
	  | None ->
	    Deferred.return Queue_job.Job_status.Failed
      end
      | false ->
	Deferred.return Queue_job.Job_status.Failed

end
