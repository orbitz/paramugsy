type t

val make : Sequence.t list -> t

val sequences : t -> Sequence.t list

val sequence_names : t -> string list

val sequence : string -> t -> Sequence.t option

