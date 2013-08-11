module Fault : sig
  type range = (Int64.t * Int64.t)

  type t =
    | Gap     of (range * range)
    | Overlap of (range * range)
end

val find : Reader.t -> (string * Fault.t list) list
