open Core_extended.Std
open Async.Std
open Ort.Function

module Job_status = Queue_job.Job_status

type t = string

(*
 * The output of 'qacct' is key-value pairs,
 * this splits and stripps and returns a list
 * of typles
 *)
let split_lines s =
  let split_and_strip line =
    line
    |> String.split ~on:' '
    |> List.map ~f:String.strip
    |> List.filter ~f:((<>) "")
  and two_tuple  = function
    | [x1; x2] -> (x1, x2)
    | x1::xs   -> (x1, String.concat ~sep:" " xs)
    | _        -> raise (Failure "Unknown line")
  in
  s
  |> String.split ~on:'\n'
  |> List.filter ~f:(fun l -> not (String.is_empty l) && l.[0] <> '=')
  |> List.map ~f:split_and_strip
  |> List.map ~f:two_tuple

let get_done_job_state t =
  Async_cmd.get_output ~text:"" ~prog:"qacct" ~args:["-j"; t] >>= function
    | Result.Ok (stdout, _) -> begin
      let kv = split_lines stdout
      in
      match List.Assoc.find kv ~equal:(=) "exit_status" with
	| Some "0" -> Deferred.return (Some (Job_status.D Job_status.Completed))
	| Some _   -> Deferred.return (Some (Job_status.D Job_status.Failed))
	| None     -> Deferred.return (Some (Job_status.D Job_status.Failed))
    end
    | Result.Error _ ->
      Deferred.return (Some (Job_status.D Job_status.Failed))

let is_job_running t =
  Async_cmd.get_output ~text:"" ~prog:"qstat" ~args:["-j"; t] >>= function
    | Result.Ok _    -> Deferred.return true
    | Result.Error _ -> Deferred.return false


(*
 * **************************************************
 * API
 * **************************************************
 *)
let submit job =
  let args =
    [ "-o"; "/mnt/scratch"
    ; "-e"; "/mnt/scratch"
    ; "-S"; "/bin/sh"
    ; "-b"; "n"
    ; "-sync"; "n"
    ; "-q"; job.Queue_job.queue
    ; job.Queue_job.payload]
  in
  Async_cmd.get_output ~text:"" ~prog:"qsub" ~args:args >>= function
    | Result.Ok (stdout, _) -> begin
      match String.split_on_chars ~on:[' '] stdout with
	| "Your"::"job"::job_id::_ ->
	  Deferred.return (Some job_id)
	| _ ->
	  Deferred.return None
    end
    | Result.Error _ ->
      Deferred.return None

let status t =
  (*
   * In SGE, if a job is running or pending it will
   * show up in the 'qstat' command.  If it is not
   * there, we check qacct for its exist code.
   *
   * Note, this makes 'pending' a useless state, for now,
   * since we just set it when we submit and it will be
   * marked as 'running' as soon as we do the first update.
   *)
  is_job_running t >>= function
    | true -> Deferred.return (Some (Job_status.R Job_status.Running))
    | false -> get_done_job_state t
