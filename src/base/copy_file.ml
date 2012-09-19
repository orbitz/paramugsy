open Core.Std
open Async.Std

let copy_file in_file out_file =
  Async_cmd.get_output ~text:"" ~prog:"cp" ~args:[in_file; out_file] >>= (function
    | Result.Ok _      -> Deferred.return (Result.Ok out_file)
    | Result.Error err -> Deferred.return (Result.Error err))

let mkdir_p dir =
  Async_cmd.get_output ~text:"" ~prog:"mkdir" ~args:["-p"; dir] >>= (function
    | Result.Ok _      -> Deferred.return (Result.Ok dir)
    | Result.Error err -> Deferred.return (Result.Error err))
