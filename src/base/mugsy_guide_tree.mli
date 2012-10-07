open Async.Std

type 'a phylogenetic_binary_tree =
  | Taxonomic_unit of 'a
  | H_taxonomic_unit of ('a phylogenetic_binary_tree * 'a phylogenetic_binary_tree)

type mugsy_tree = string phylogenetic_binary_tree

val guide_tree_of_sequences : string list -> mugsy_tree Deferred.t
val list_of_guide_tree      : mugsy_tree -> string list
