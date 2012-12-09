open Core.Std

type t

module Direction : sig
  type t = Forward | Reverse

  val to_string : t -> string
end

module Range : sig
  (*
   * A Range represents a slice of a gnome.  A Range
   * is always of the from [start, end], where both
   * sides are inclusive.  Start is always less than
   * end, which means Reverse (0, 10) represents the
   * reverse compliment of that part of the genome
   *)

  type t =
    | Forward of (Int64.t * Int64.t)
    | Reverse of (Int64.t * Int64.t)

  (*
   * Always compares the ranges by the start of the
   * range, regardless of direction
   *)
  val compare  : t -> t -> int

  val length   : t -> Int64.t
  val of_coord :
    start:Int64.t ->
    size:Int64.t ->
    total:Int64.t ->
    d:Direction.t ->
    t
end

val make :
  name:string ->
  start:Int64.t ->
  size:Int64.t ->
  d:Direction.t ->
  total:Int64.t ->
  seq:string ->
  t

val reverse   : t -> t

val name      : t -> string
val start     : t -> Int64.t
val size      : t -> Int64.t
val direction : t -> Direction.t
val total     : t -> Int64.t
val sequence  : t -> string
val range     : t -> Range.t
