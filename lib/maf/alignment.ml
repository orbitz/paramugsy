open Core.Std

type t = Sequence.t list

let make t = t

let sequences t = t

let sequence_names =
  List.map ~f:(fun s -> Sequence.name s)

let rec sequence name = function
  | [] -> None
  | s::_ when Sequence.name s = name -> Some s
  | _::ss -> sequence name ss


