open Core.Std

module Score = struct
  type t = string
end

type t = { score : Score.t
	 ; seqs  : Sequence.t list
	 }

let make score seqs = {score; seqs}

let score t = t.score

let sequences t = t.seqs

let sequence_names t =
  List.map ~f:(fun s -> Sequence.name s) t.seqs

let sequence name t =
  let rec find = function
    | [] -> None
    | s::_ when Sequence.name s = name -> Some s
    | _::ss -> find ss
  in
  find t.seqs


