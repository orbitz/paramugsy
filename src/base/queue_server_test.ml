open Core.Std
open Async.Std

module Job_status = Queue_job.Job_status

module Test_server : Queue_server.QUEUE_SERVER = struct
  module Job_set = Set.Make(Queue_job.Name)

  type t = Job_set.t ref

  let start () = ref Job_set.empty
  let stop ts  = Deferred.return ()

  let run ~n ~q payload ts =
    ts := Job_set.add n !ts;
    Deferred.return true

  let status n ts =
    if Job_set.mem n !ts then
      Deferred.return (Some (Job_status.D Job_status.Completed))
    else
      Deferred.return None

  let wait n ts =
    if Job_set.mem n !ts then
      Deferred.return (Some Job_status.Completed)
    else
      Deferred.return None

  let ack n ts =
    ts := Job_set.remove n !ts
end

module Test_queue_server = Queue_server.Make(Test_server)

let never_returns = Core.Std.never_returns

let test_server_test temp_dir =
  let tqs           = Test_queue_server.start () in
  let template_file = Filename.temp_file ~in_dir:temp_dir "foo" "bar" in
  let job = { Queue_server.name          = "test"
	    ;              verbose       = false
	    ;              template_file = template_file
	    ;              script_dir    = temp_dir
	    ;              exec_queue    = ""
	    ;              data_queue    = ""
	    ;              pre           = []
	    ;              post          = []
	    ;              body          = []
	    ;              in_files      = []
	    ;              out_files     = []
	    }
  in
  Test_queue_server.submit job tqs >>= function
    | Job_status.Completed -> Deferred.return 0
    | Job_status.Failed    -> Deferred.return 1

let test () =
  let temp_dir = Filename.temp_dir ~in_dir:"/tmp" "foo" "bar" in
  test_server_test temp_dir >>> fun ret ->
  Shell.rm ~r:() ~f:() temp_dir;
  never_returns (Shutdown.shutdown_and_raise ret)


let () =
  test ();
  never_returns (Scheduler.go ())
