open Core.Std

module Direction = struct
  type t = Forward | Reverse

  let to_string = function
    | Forward -> "+"
    | Reverse -> "-"
end

type t = { name  : string
	 ; start : Int64.t
	 ; size  : Int64.t
	 ; d     : Direction.t
	 ; total : Int64.t
	 }


let make ~name ~start ~size ~d ~total =
  { name; start; size; d; total }

let reverse _ = failwith "Not implemented yet"

let name t = t.name

let start t = t.start

let size t = t.size

let direction t = t.d

let total t = t.total
