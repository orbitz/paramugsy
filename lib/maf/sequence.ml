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
	 ; seq   : string
	 }


let make ~name ~start ~size ~d ~total ~seq =
  { name; start; size; d; total; seq }

let reverse _ = failwith "Not implemented yet"

let name t = t.name

let start t = t.start

let size t = t.size

let direction t = t.d

let total t = t.total

let sequence t = t.seq
