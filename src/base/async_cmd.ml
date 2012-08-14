module OUnix = Unix
open Core_extended.Std
open Async.Std

type pid = int

type t = { pid    : pid
	 ; stdin  : Writer.t
	 ; stdout : Reader.t
	 ; stderr : Reader.t
	 }

type cmd_exit = [ `Exited of int | `Signal of int | `Unknown ]

let read_all r =
  let rec read_all' s =
    let str = String.create 4096
    in
    Reader.read r str >>= (function
      | `Ok n -> read_all' (s ^ str)
      | `Eof  -> begin let _ = Reader.close r in Deferred.return s end)
  in
  read_all' ""

let wait pi =
  In_thread.run
    (fun () ->
      match OUnix.waitpid [] pi.pid with
	| (_, OUnix.WEXITED exit_code) ->
	  `Exited exit_code
	| (_, OUnix.WSIGNALED signal) ->
	  `Signal signal
	| _ ->
	  `Unknown)

let run ~prog ~args =
  let (c_stdin, m_stdin)   = OUnix.pipe ()
  and (m_stdout, c_stdout) = OUnix.pipe ()
  and (m_stderr, c_stderr) = OUnix.pipe ()
  in
  let pid =
    OUnix.create_process
      prog
      (Array.of_list args)
      c_stdin
      c_stdout
      c_stderr
  in
  let module K = Async.Std.Fd.Kind
  in
  Deferred.return
    { pid = pid
    ; stdin  = Writer.create (Fd.create K.File m_stdin ~name:"stdin")
    ; stdout = Reader.create (Fd.create K.File m_stdout ~name:"stdout")
    ; stderr = Reader.create (Fd.create K.File m_stderr ~name:"stderr")
    }

let get_output ~text ~prog ~args =
  Printf.printf "this?\n";
  run ~prog:prog ~args:args >>= fun pi ->
  Printf.printf "bar\n";
  (* Writer.write pi.stdin text; *)
  let _ = Writer.close pi.stdin
  in
  Printf.printf "foo\n";
  wait pi >>= (function
    | `Exited 0 -> begin
      Printf.printf "Exited ok\n";
      (Deferred.both (read_all pi.stdout) (read_all pi.stderr)) >>= (fun
	(stdout, stderr) ->
	  Deferred.return (Result.Ok (stdout, stderr)))
    end
    | err ->
      Deferred.return (Result.Error err))
