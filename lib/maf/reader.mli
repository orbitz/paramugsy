open Core.Std

type t

val create : In_channel.t -> t

val read_next : t -> Alignment.t option
