open Core_extended.Std
open Async.Std

type t = { pid    : Core.Pid.t
	 ; stdin  : Writer.t
	 ; stdout : Reader.t
	 ; stderr : Reader.t
	 }

let read_all r =
  let rec read_all' s =
    let str = String.create 4096
    in
    Deferred.bind
      (Reader.read r str)
      (function
	| `Ok n -> read_all' (s ^ str)
	| `Eof  -> begin let _ = Reader.close r in Deferred.return s end)
  in
  read_all' ""

let wait pi =
  In_thread.run
    (fun () -> Core.Std.Unix.wait (`Pid pi.pid))

let run ~prog ~args =
  In_thread.run
    (fun () ->
      let program_info = Core.Std.Unix.create_process ~prog:prog ~args:args
      in
      let module P = Core.Std.Unix.Process_info
      in
      let module K = Async.Std.Fd.Kind
      in
      { pid    = program_info.P.pid
      ; stdin  = Writer.create (Fd.create K.File program_info.P.stdin ~name:"stdin")
      ; stdout = Reader.create (Fd.create K.File program_info.P.stdout ~name:"stdout")
      ; stderr = Reader.create (Fd.create K.File program_info.P.stderr ~name:"stderr")
      })

let get_output ~text ~prog ~args =
  Deferred.bind
    (run ~prog:prog ~args:args)
    (fun pi ->
      Writer.write pi.stdin text;
      let _ = Writer.close pi.stdin
      in
      wait pi >>= (function
	| (_, Result.Ok ()) ->
	  (Deferred.both (read_all pi.stdout) (read_all pi.stderr)) >>= (fun
	    (stdout, stderr) ->
	      Deferred.return (Result.Ok (stdout, stderr)))
	  | (_, Result.Error err) ->
	    Deferred.return (Result.Error err)))
