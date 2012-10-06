open Async.Std

open Ort

open Script_task_server

module Make : functor (Sts : SCRIPT_TASK_SERVER) -> sig
  type t

  val start : int -> t
  val stop  : t -> unit Deferred.t

  val submit :
    p:int ->
    n:Queue_job.Name.t ->
    q:Queue_job.Queue.t ->
    Fileutils.file_path ->
    t ->
    Queue_job.Job_status.job_done Deferred.t
end

