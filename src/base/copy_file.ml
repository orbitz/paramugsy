open Core.Std
open Async.Std

let copy_file in_file out_file =
  Async_cmd.get_output ~text:"" ~prog:"cp" ~args:[in_file; out_file] >>= (function
    | Result.Ok _      -> Deferred.return (Result.Ok out_file)
    | Result.Error err -> Deferred.return (Result.Error err))
