open Core.Std

module Direction = struct
  type t = Forward | Reverse

  let to_string = function
    | Forward -> "+"
    | Reverse -> "-"
end

module Range = struct
  type t =
    | Forward of (Int64.t * Int64.t)
    | Reverse of (Int64.t * Int64.t)

  let compare l r =
    let get_start = function
      | Forward (s, _) -> s
      | Reverse (s, _) -> s
    in
    Int64.compare (get_start l) (get_start r)

  let length t =
    let open Int64 in
    match t with
      | Forward (s, e) -> e - s + one
      | Reverse (s, e) -> e - s + one

  let of_coord ~start ~size ~total ~d =
    let open Int64 in
    match d with
      | Direction.Forward ->
	Forward (start, start + pred size)
      | Direction.Reverse ->
	Reverse (total - start - pred size, total - start)

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

let range t =
  Range.of_coord ~start:t.start ~size:t.size ~total:t.total ~d:t.d
