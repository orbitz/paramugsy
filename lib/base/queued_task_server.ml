open Core.Std
open Async.Std

open Ort

open Script_task_server

module Make = functor (Sts : SCRIPT_TASK_SERVER) -> struct
  type task =
      ( int
      * Queue_job.Name.t
      * Queue_job.Queue.t
      * Fileutils.file_path
      * Queue_job.Job_status.job_done Ivar.t
      )

  type msg =
    | Run of task
    | Done
    | Stop

  type t = { sts      : Sts.t
	   ; run_size : int
	   ; running  : int
	   ; queue    : task Heap.t
	   ; mq       : msg Tail.t
	   }


  let task_compare (pri1, _, _, _, _) (pri2, _, _, _, _) = Int.compare pri2 pri1

  let send mq msg =
    Tail.extend mq msg

  let run_job mq n q script sts ret =
    Sts.run ~n:n ~q:q script sts >>= function
      | true -> begin
	Sts.wait n sts >>= function
	  | Some status -> begin
	    Sts.ack n sts;
	    Ivar.fill ret status;
	    send mq Done;
	    Deferred.return ()
	  end
	  | None -> begin
	    Ivar.fill ret Queue_job.Job_status.Failed;
	    Deferred.return ()
	  end
      end
      | false -> begin
	Ivar.fill ret Queue_job.Job_status.Failed;
	Deferred.return ()
      end

  let handle_msg t = function
    | Run (p, n, q, script, ret) -> begin
      if t.running < t.run_size then begin
	ignore (run_job t.mq n q script t.sts ret);
	Deferred.return {t with running = t.running + 1}
      end
      else begin
	ignore (Heap.add t.queue (p, n, q, script, ret));
	Deferred.return t
      end
    end
    | Done -> begin
      match Heap.pop t.queue with
	| Some (_, n ,q, script, ret) -> begin
	  ignore (run_job t.mq n q script t.sts ret);
	  Deferred.return t
	end
	| None ->
	  Deferred.return {t with running = t.running - 1}
    end
    | Stop -> begin
      Tail.close_if_open t.mq;
      Deferred.return t
    end

  let rec loop s =
    if not (Tail.is_closed s.mq) then
      let stream = Tail.collect s.mq in
      Stream.fold'
	~f:handle_msg
	~init:s
	stream >>= loop
    else
      Deferred.return ()

  let start run_size =
    let t = { sts      = Sts.start ()
	    ; run_size = run_size
	    ; running  = 0
	    ; queue    = Heap.create task_compare ()
	    ; mq       = Tail.create ()
	    }
    in
    ignore (loop t);
    t

  let stop t =
    send t.mq Stop;
    Sts.stop t.sts

  let submit ~p ~n ~q script t =
    let ret = Ivar.create () in
    send t.mq (Run (p, n, q, script, ret));
    Ivar.read ret
end
