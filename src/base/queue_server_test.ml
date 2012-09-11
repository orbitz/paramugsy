open Core_extended.Std
open Async.Std

module Test_server = struct
  type name        = string
  type queue       = string

  type run_success = unit
  type run_error   = Queue_error
  type job_running = Pending | Running
  type job_done    = Completed | Failed
  type job_status  = R of job_running | D of job_done

  module Job_set = Set.Make(String)

  type t = Job_set.t ref

  let start () = ref Job_set.empty
  let stop ts  = Deferred.return ()

  let run ~n ~q script ts =
    ts := Job_set.add n !ts;
    Deferred.return (Result.Ok ())

  let status n ts =
    if Job_set.mem n !ts then
      Deferred.return (Some (D Completed))
    else
      Deferred.return None

  let wait n ts =
    if Job_set.mem n !ts then
      Deferred.return (Some Completed)
    else
      Deferred.return None

  let ack n ts =
    ts := Job_set.remove n !ts
end

module Test_queue_server = Queue_server.Make(Test_server)

let never_returns = Core.Std.never_returns

let test_server_test () =
  let tqs = Test_queue_server.start ()
  in
  let job = { Test_queue_server.name          = "test"
	    ; Test_queue_server.verbose       = false
	    ; Test_queue_server.template_file = ""
	    ; Test_queue_server.script_dir    = ""
	    ; Test_queue_server.exec_queue    = ""
	    ; Test_queue_server.data_queue    = ""
	    ; Test_queue_server.pre           = []
	    ; Test_queue_server.post          = []
	    ; Test_queue_server.body          = []
	    ; Test_queue_server.in_files      = []
	    ; Test_queue_server.out_files     = []
	    }
  in
  Test_queue_server.submit job tqs >>= function
    | Test_server.Completed -> Deferred.return 0
    | Test_server.Failed    -> Deferred.return 1

let test () =
  test_server_test () >>> fun ret ->
  never_returns (Shutdown.shutdown_and_raise ret)


let () =
  test ();
  never_returns (Scheduler.go ())
