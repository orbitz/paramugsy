open Core.Std
open Async.Std

type pid = int

type t = { pid    : pid
	 ; stdin  : Writer.t
	 ; stdout : Reader.t
	 ; stderr : Reader.t
	 }

type cmd_exit = [ `Exited of int | `Signal of int | `Unknown ]

val wait       : t -> cmd_exit Deferred.t
val run        : prog:string -> args:string list -> t Deferred.t
val get_output :
  text:string ->
  prog:string ->
  args:string list ->
  ((string * string), cmd_exit) Result.t Deferred.t
