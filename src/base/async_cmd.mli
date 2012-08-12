open Core_extended.Std
open Async.Std

type t = { pid    : Core.Pid.t
	 ; stdin  : Writer.t
	 ; stdout : Reader.t
	 ; stderr : Reader.t
	 }

val wait       : t -> (Core.Pid.t * Core.Std.Unix.Exit_or_signal.t) Deferred.t
val run        : prog:string -> args:string list -> t Deferred.t
val get_output :
  text:string ->
  prog:string ->
  args:string list ->
  ((string * string), Core.Std.Unix.Exit_or_signal.error) Result.t Deferred.t
