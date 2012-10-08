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

let read_all r =
  let b = Buffer.create 1024
  in
  let rec read_all' () =
    let str = String.create 4096
    in
    Reader.read r str >>= (function
      | `Ok n -> begin
	Buffer.add_substring b str 0 n;
	read_all' ()
      end
      | `Eof  -> begin
	let _ = Reader.close r in
	Deferred.return (Buffer.contents b)
      end)
  in
  read_all' ()

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
  (* Writer.write pi.stdin text; *)
  let _ = Writer.close pi.stdin
  in
  wait pi >>= function
    | `Exited 0 ->
      Deferred.both (read_all pi.stdout) (read_all pi.stderr) >>= fun (stdout, stderr) ->
      Deferred.return (Result.Ok (stdout, stderr))
    | err ->
      Deferred.both (read_all pi.stdout) (read_all pi.stderr) >>= fun (stdout, stderr) ->
      Deferred.return (Result.Error (err ,(stdout, stderr)))
