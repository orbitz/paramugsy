open Core.Std
open Async.Std

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

(* let async_cmd_get_output_test () = *)
(*   Async_cmd.get_output ~text:"foo\n" ~prog:"cat" ~args:[] >>= function *)
(*     | Result.Ok ("foo\n", "") -> *)
(*       Deferred.return 0 *)
(*     | Result.Ok (stdout, stderr) -> begin *)
(*       Printf.printf "Failed to run program, got output:\n"; *)
(*       Printf.printf "Stdout: '%s' Stderr: '%s'\n" stdout stderr; *)
(*       Deferred.return 1; *)
(*     end *)
(*     | Result.Error (`Exited exit_code) -> *)
(*       Deferred.return 2 *)
(*     | Result.Error (`Signal s) -> *)
(*       Deferred.return 3 *)
(*     | Result.Error `Unknown -> *)
(*       Deferred.return 4 *)

let test () =
  upon
    (async_cmd_get_output_test ())
    (fun ret -> never_returns (Shutdown.shutdown_and_raise ret))


let () =
  test ();
  never_returns (Scheduler.go ())
