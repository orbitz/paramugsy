open Core_extended.Std
open Async.Std

let never_returns = Core.Std.never_returns

let async_cmd_get_output_test () =
  Printf.printf "THIS?\n";
  Async_cmd.get_output ~text:"" ~prog:"echo" ~args:["ls"; "foo"] >>= function
    | Result.Ok (stdout, stderr) -> begin
      Printf.printf "Stdout: %s\nStderr: %s\n" stdout stderr;
      Deferred.return ()
    end
    | Result.Error (`Exited exit_code) -> begin
      Printf.printf "Error - exit_code: %d\n" exit_code;
      Deferred.return ()
    end
    | Result.Error (`Signal s) -> begin
      Printf.printf "Error - signal: %d\n" s;
      Deferred.return ()
    end
    | Result.Error `Unknown -> begin
      Printf.printf "Error - unknown\n";
      Deferred.return ()
    end

let test () =
  upon
    (async_cmd_get_output_test ())
    (fun () -> never_returns (Shutdown.shutdown_and_raise 0))


let () =
  Printf.printf "What now?\n";
  test ();
  never_returns (Scheduler.go ())
