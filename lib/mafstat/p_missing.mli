type t

val create   : unit -> t
val add      : Maf.Alignment.t -> t -> t
val finalize : string -> t -> unit
