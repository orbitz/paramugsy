type reference

val make_count : unit -> string
val make_ref   : unit -> reference
val equal_ref  : reference -> reference -> bool


module Ref_compare : sig
  type t = reference
  val compare : reference -> reference -> int
end
