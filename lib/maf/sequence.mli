open Core.Std

type t

module Direction : sig
  type t = Forward | Reverse

  val to_string : t -> string
end

val make :
  name:string ->
  start:Int64.t ->
  size:Int64.t ->
  d:Direction.t ->
  total:Int64.t ->
  t

val reverse   : t -> t

val name      : t -> string
val start     : t -> Int64.t
val size      : t -> Int64.t
val direction : t -> Direction.t
val total     : t -> Int64.t
