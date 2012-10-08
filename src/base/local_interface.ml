open Core.Std
open Async.Std

module Job_status = Queue_job.Job_status

type t = Job_status.job_done Ivar.t

let run_job t job =
  Async_cmd.get_output ~text:"" ~prog:job.Queue_job.payload ~args:[] >>> function
    | Result.Ok (_, _) ->
      Ivar.fill t Job_status.Completed
    | Result.Error _ ->
      Ivar.fill t Job_status.Failed

(*
 * **************************************************
 * API
 * **************************************************
 *)
let submit job =
  let t = Ivar.create () in
  run_job t job;
  Deferred.return (Some t)

let status t =
  if Ivar.is_full t then begin
    Ivar.read t >>= fun r ->
    Deferred.return (Some (Job_status.D r))
  end
  else begin
    Deferred.return (Some (Job_status.R Job_status.Running))
  end
