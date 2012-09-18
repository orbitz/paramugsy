open Core_extended.Std
open Async.Std

module Job_status = Queue_job.Job_status

type t = Job_status.job_done Ivar.t

let run_job ret job =
  Async_cmd.get_output ~text:"" ~prog:job ~args:[] >>= function
    | Result.Ok (_, _) ->
      Ivar.fill ret (Job_status.D Job_status.Completed)
    | Result.Error _ ->
      Ivar.fill ret (Job_status.D Job_status.Failed)


(*
 * **************************************************
 * API
 * **************************************************
 *)
let submit job =
  let ret = Ivar.create () in
  run_job ret job;
  Deferred.return ret

let status t =
  if Ivar.is_full t then begin
    Ivar.read t >>= fun r ->
    Deferred.return (Some r)
  end
  else
    Deferred.return (Some (Job_status.R Job_status.Running))
