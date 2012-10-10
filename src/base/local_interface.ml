open Core.Std
open Async.Std

module Job_status = Queue_job.Job_status

type t = Job_status.job_done Ivar.t

let retries = 100

let rec run_job retries t job =
  Async_cmd.get_output ~text:"" ~prog:job.Queue_job.payload ~args:[] >>> function
    | Result.Ok (_, _) ->
      Ivar.fill t Job_status.Completed
    | Result.Error (`Exited 127, _) when retries > 0 -> begin
      (*
       * Linux doesn't let you execute files that are open for writing
       * and under heavy I/O load the might take a few seconds for
       * an open file to be closed.  So if we get this error wait
       * a bit and try again.  We have a retry count because there
       * could be a legitimate issue and we want to give up in
       * that case
       *)
      Global_state.logger
	(Printf.sprintf "%s Failed with 127, retrying"
	   job.Queue_job.payload);
      after (sec 5.) >>> fun () -> run_job (retries - 1) t job
    end
    | Result.Error (`Exited n, _) when retries > 0 -> begin
      Global_state.logger
	(Printf.sprintf "%s Failed with %d, retrying"
	   job.Queue_job.payload
	   n);
      let retries = if retries <= 10 then retries else 10 in
      after (sec 5.) >>> fun () -> run_job (retries - 1) t job
    end
    | Result.Error (err, (stdout, stderr)) -> begin
      let () =
	match err with
	  | `Exited i -> Global_state.logger (Printf.sprintf "Exited: %d" i)
	  | `Signal i -> Global_state.logger (Printf.sprintf "Signal: %d" i)
	  | `Unknown  -> Global_state.logger "Unknown"
      in
      Global_state.logger (Printf.sprintf "Failed: %s" job.Queue_job.payload);
      Global_state.logger (Printf.sprintf "Stdout: %s\nStderr: %s" stdout stderr);
      Ivar.fill t Job_status.Failed
    end

(*
 * **************************************************
 * API
 * **************************************************
 *)
let submit job =
  let t = Ivar.create () in
  run_job retries t job;
  Deferred.return (Some t)

let status t =
  if Ivar.is_full t then begin
    Ivar.read t >>= fun r ->
    Deferred.return (Some (Job_status.D r))
  end
  else begin
    Deferred.return (Some (Job_status.R Job_status.Running))
  end
