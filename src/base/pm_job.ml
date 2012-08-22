(*
 * We want the ability to have multiple backends to processing input data
 * but we don't want them to have to worry about how to construct the work.
 * This module takes the input and constructs three pieces of information:
 *
 * 1) A tree describing how the input should be processed.  This tree
 *    consists of three types of work:
 *    Mugsy_profile - A profile run between two previous processed inputs
 *    Mugsy         - A mugsy run against some some genomes
 *    Fake_mugsy    - If there is only 1 genome this fakes a Mugsy run
 *
 * 2) List of nucmers to run, in the order that they will be needed if the
 *    above tree is executed depth-first
 *
 * 3) The above two structures all reference genomes by their a name, a mapping
 *    of genome name to the filename is also provided.
 *)
open Core_extended.Std
open Ort

module Genome_map = Map.Make(String)

type genome = string

type pairwise = genome * genome

type job_tree =
  | Nil
  | Mugsy_profile of (job_tree * job_tree)
  | Mugsy of pairwise list
  | Fake_mugsy of genome

type t = { job_tree   : job_tree
	 ; pairwise   : pairwise list
	 ; genome_map : string Genome_map.t
	 }

let cross left_seqs right_seqs =
  let inner_fold acc e =
    List.fold_left
      ~f:(fun acc' e' ->
	(e, e')::acc')
      ~init:acc
      right_seqs
  in
  List.fold_left
    ~f:inner_fold
    ~init:[]
    left_seqs

let not_equal (x, y) = x <> y

let uniq = List.filter ~f:not_equal

let split len l =
  (List.take l (len/2), List.drop l (len/2))

let mk_job max_seqs guide_tree =
  let rec mk_job_from_list l =
    match List.length l with
      | 1 ->
	Fake_mugsy (List.hd_exn l)
      | len when len <= max_seqs ->
	Mugsy (uniq (cross l l))
      | len ->
	let (left, right) = split len l
	in
	Mugsy_profile (mk_job_from_list left, mk_job_from_list right)
  in
  let sequences = Mugsy_guide_tree.list_of_guide_tree guide_tree
  in
  mk_job_from_list sequences

let mk_nucmer max_seqs guide_tree =
  let rec mk_nucmer_from_list l =
    match List.length l with
      | 1 ->
	[]
      | len when len <= max_seqs ->
	(mk_nucmer_from_list l) @ (uniq (cross l l))
      | len ->
	let (left, right) = split len l
	in
	(mk_nucmer_from_list l) @ (cross left right)
  in
  let sequences = Mugsy_guide_tree.list_of_guide_tree guide_tree
  in
  mk_nucmer_from_list sequences

let create_genome_map sequences _ =
  List.fold_left
    ~f:(fun acc s -> Genome_map.add (Fileutils.basename s) s acc)
    ~init:Genome_map.empty
    sequences

let make_job max_seqs sequences =
  let guide_tree = Mugsy_guide_tree.guide_tree_of_sequences sequences
  in
  { job_tree   = mk_job max_seqs guide_tree
  ; pairwise   = mk_nucmer max_seqs guide_tree
  ; genome_map = create_genome_map sequences guide_tree
  }
