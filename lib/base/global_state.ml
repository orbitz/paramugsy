type reference = string

module Ref_compare = struct
  type t = reference
  let compare = compare
end


let count = ref 0
let make_count () = count := !count + 1; string_of_int !count

let make_ref = make_count

let equal_ref = (==)
