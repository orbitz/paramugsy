type t

module Score : sig
  type t = string
end

val make : Score.t -> Sequence.t list -> t

val score : t -> Score.t

val sequences : t -> Sequence.t list

val sequence_names : t -> string list

val sequence : string -> t -> Sequence.t option

