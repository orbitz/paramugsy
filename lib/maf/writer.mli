open Core.Std

type t

val create : Out_channel.t -> t

val write : t -> Alignment.t -> unit
