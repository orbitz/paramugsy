open Core.Std

type t

val create      : In_channel.t -> t
val next_header : t -> string option
val next_seq    : t -> buf:string -> pos:int -> len:int -> int
