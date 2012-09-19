(*pp camlp4o *)
open Core_extended
open Core.Std
open Ort
open Ort.Function
open Bio

type name = string

type 'a phylogenetic_binary_tree =
  | Taxonomic_unit of 'a
  | H_taxonomic_unit of ('a phylogenetic_binary_tree * 'a phylogenetic_binary_tree)

type mugsy_tree = string phylogenetic_binary_tree

let rec mugsy_tree_of_newick_tree = function
  | Newick.Leaf (name, _distance) ->
    Taxonomic_unit name
  | Newick.Tree ([t1; t2], _d) ->
    H_taxonomic_unit (mugsy_tree_of_newick_tree t1, mugsy_tree_of_newick_tree t2)
  | Newick.Tree _ ->
    raise (Failure "Not a binary tree!")
  | Newick.Newick [t1; t2] ->
    H_taxonomic_unit (mugsy_tree_of_newick_tree t1, mugsy_tree_of_newick_tree t2)
  | Newick.Newick _ ->
    raise (Failure "Not a binary Newick tree!")

let rec tree_depth = function
  | Taxonomic_unit _ ->
    0
  | H_taxonomic_unit (t1, t2) ->
    max (tree_depth t1 + 1) (tree_depth t2 + 1)

let branch_depths = function
  | Taxonomic_unit _ ->
    (0, 0)
  | H_taxonomic_unit (t1, t2) ->
    (tree_depth t1, tree_depth t2)


let load_guide_tree sin =
  let rec char_stream_of_string_stream stream =
    match Seq.next stream with
      | Some s ->
	[< Seq.of_string s; char_stream_of_string_stream stream >]
      | None ->
	[< >]
  in
  sin
  |> char_stream_of_string_stream
  |> Newick.parse
  |> mugsy_tree_of_newick_tree


let rec list_of_guide_tree = function
  | Taxonomic_unit tu ->
    [tu]
  | H_taxonomic_unit (l, r) ->
    List.append (list_of_guide_tree l) (list_of_guide_tree r)

let rec map ~f = function
  | H_taxonomic_unit (l, r) ->
    H_taxonomic_unit (map ~f:f l, map ~f:f r)
  | Taxonomic_unit tu ->
    Taxonomic_unit (f tu)

(*
 * Create a guide tree from a list of sequence files.  The entry names in the
 * guide tree will correspond to the names of the files
 *)
let guide_tree_of_sequences sequences =
  let seqs = String.concat ~sep:"\n" sequences ^ "\n"
  in
  let path_map =
    List.map
      ~f:(fun s -> (Fileutils.basename s, s))
      sequences
  in
  let tree =
    Shell.sh_lines
      ~echo:true
      ~input:seqs
      "strip_sequences.sh | muscle -clusteronly -tree1 - -maxmb 99999999"
  in
  map
    ~f:(List.Assoc.find_exn ~equal:(=) path_map)
    (load_guide_tree (Seq.of_list tree))
