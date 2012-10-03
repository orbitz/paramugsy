open Core.Std
open Async.Std

open Ort

open Script_task_server

module Make = functor (Sts : SCRIPT_TASK_SERVER) -> struct
  type msg =
    | Run of ( Queue_job.Name.t
	     * Queue_job.Queue.t
	     * Fileutils.file_path
	     * Queue_job.Job_status.job_done Ivar.t
             )
    | Done
    | Stop

  type t = { sts      : Sts.t
	   ; run_size : int
	   ; running  : int
	   ; queue    : int Heap.t
	   ; mq       : msg Tail.t
	   }

  let run_job n q script sts ret =
    Sts.run ~n:n ~q:q script sts >>= function
      | true -> begin
	Sts.wait n sts >>= function
	  | Some status -> begin
	    Sts.ack n sts;
	    Ivar.fill ret status;
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
    | Run (n, q, script, ret) -> begin
      whenever (run_job n q script t.sts ret);
      Deferred.return {t with running = t.running + 1}
    end
    | Done ->
      Deferred.return {t with running = t.running - 1}
    | Stop -> begin
      Tail.close_if_open t.mq;
      Deferred.return t
    end

  let rec loop t =
    (Stream.next (Tail.collect t.mq)) >>= function
      | Stream.Nil         -> Deferred.return ()
      | Stream.Cons (m, _) -> handle_msg t m >>= loop

  let send mq msg =
    Tail.extend mq msg

  let start run_size =
    let t = { sts      = Sts.start ()
	    ; run_size = run_size
	    ; running  = 0
	    ; queue    = Heap.create Int.compare
	    ; mq       = Tail.create ()
	    }
    in
    whenever (loop t);
    t

  let stop t =
    send t.mq Stop;
    Sts.stop t.sts

  let submit ~n ~q script t =
    let ret = Ivar.create () in
    send t.mq (Run (n, q, script, ret));
    Ivar.read ret
end
