open Core.Std

type t

type string_map_key = String.Map.Key.t Map.key

val build : Anchor.t Array.t -> t

(* Accessors *)
val genome_exn       : string -> t -> int
val genome_keys      : t -> string list
val accession_exn    : string -> t -> int
val accession_keys   : t -> string list
val accession_fold   : f:(key:string_map_key -> data:int -> 'a -> 'a) -> init:'a -> t -> 'a
val seq_anchors_exn  : string -> t -> int list
val seq_anchors_keys : t -> string list
val seq_anchors_fold : f:(key:string_map_key -> data:int list -> 'a -> 'a) -> init:'a -> t -> 'a
val anchors_exn      : int -> t -> Anchor.t
