module OUnix = Unix
open Core.Std
open Async.Std

type pid = int

type t = { pid    : pid
	 ; stdin  : Writer.t
	 ; stdout : Reader.t
	 ; stderr : Reader.t
	 }

type cmd_exit = [ `Exited of int | `Signal of int | `Unknown ]

let wait pi =
  Unix.wait (`Pid (Core.Std.Pid.of_int pi.pid)) >>= function
    | (_, Result.Ok ()) ->
      Deferred.return (`Exited 0)
    | (_, Result.Error (`Exit_non_zero n)) ->
      Deferred.return (`Exited n)
    | (_, _) ->
      Deferred.return `Unknown

let run ~prog ~args =
  let (c_stdin, m_stdin)   = OUnix.pipe ()
  and (m_stdout, c_stdout) = OUnix.pipe ()
  and (m_stderr, c_stderr) = OUnix.pipe ()
  in
  let pid =
    OUnix.create_process
      prog
      (Array.of_list (prog::args))
      c_stdin
      c_stdout
      c_stderr
  in
  OUnix.close c_stdin;
  OUnix.close c_stdout;
  OUnix.close c_stderr;
  let module K = Async.Std.Fd.Kind
  in
  Deferred.return
    { pid = pid
    ; stdin  = Writer.create (Fd.create K.File m_stdin ~name:"stdin")
    ; stdout = Reader.create (Fd.create K.File m_stdout ~name:"stdout")
    ; stderr = Reader.create (Fd.create K.File m_stderr ~name:"stderr")
    }

let get_output ~text ~prog ~args =
  run ~prog:prog ~args:args >>= fun pi ->
  Writer.write pi.stdin text;
  Writer.close pi.stdin >>= fun () ->
  let stdout = Reader.contents pi.stdout in
  let stderr = Reader.contents pi.stderr in
  wait pi >>= function
    | `Exited 0 ->
      Deferred.both stdout stderr >>= fun (stdout, stderr) ->
      Deferred.return (Result.Ok (stdout, stderr))
    | err ->
      Deferred.both stdout stderr >>= fun (stdout, stderr) ->
      Deferred.return (Result.Error (err, (stdout, stderr)))
