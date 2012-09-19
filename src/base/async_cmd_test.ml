open Core.Std
open Async.Std

let never_returns = Core.Std.never_returns

let async_cmd_get_output_test () =
  Async_cmd.get_output ~text:"" ~prog:"echo" ~args:["foo"] >>= function
    | Result.Ok ("foo\n", "") -> begin
      Deferred.return 0
    end
    | Result.Ok (stdout, stderr) -> begin
      Printf.printf "Failed to run program, got output:\n";
      Printf.printf "Stdout: '%s' Stderr: '%s'\n" stdout stderr;
      Deferred.return 1;
    end
    | Result.Error (`Exited exit_code) -> begin
      Deferred.return 2
    end
    | Result.Error (`Signal s) -> begin
      Deferred.return 3
    end
    | Result.Error `Unknown -> begin
      Deferred.return 4
    end

let test () =
  upon
    (async_cmd_get_output_test ())
    (fun ret -> never_returns (Shutdown.shutdown_and_raise ret))


let () =
  test ();
  never_returns (Scheduler.go ())
