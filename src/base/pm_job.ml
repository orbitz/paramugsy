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
open Core.Std
open Async.Std

open Ort

module Genome_name : Identifier = String

module Genome_map = Genome_name.Map

type t =
  | Nil
  | Mugsy_profile of (t * t)
  | Mugsy of Fileutils.file_path list
  | Fake_mugsy of Fileutils.file_path

let rec to_list = function
  | Nil ->
    []
  | Mugsy_profile (left, right) ->
    to_list left @ to_list right
  | Mugsy genomes ->
    genomes
  | Fake_mugsy genome ->
    [genome]

let searches genomes =
  let rec s accum = function
    | [] ->
      accum
    | x::xs ->
      let pairwise = List.map ~f:(fun g -> (x, g)) xs in
      s (accum @ pairwise) xs
  in
  s [] genomes

let cross left right =
  List.concat
    (List.map
       ~f:(fun g1 -> List.map ~f:(fun g2 -> (g1, g2)) right)
       left)

let split len l =
  (List.take l (len/2), List.drop l (len/2))

let mk_job max_seqs guide_tree =
  let rec mk_job_from_list l =
    match List.length l with
      | 1 ->
	Fake_mugsy (List.hd_exn l)
      | len when len <= max_seqs ->
	Mugsy l
      | len ->
	let (left, right) = split len l
	in
	Mugsy_profile (mk_job_from_list left, mk_job_from_list right)
  in
  let sequences =
    Mugsy_guide_tree.list_of_guide_tree guide_tree
  in
  mk_job_from_list sequences

let make_job max_seqs sequences =
  Mugsy_guide_tree.guide_tree_of_sequences sequences >>= fun guide_tree ->
  Deferred.return (mk_job max_seqs guide_tree)

let pairwise = function
  | Nil ->
    []
  | Mugsy_profile (left, right) ->
    cross (to_list left) (to_list right)
  | Mugsy genomes ->
    searches genomes
  | Fake_mugsy _ ->
    []

let pp_job_tree w job_tree =
  let rec pp_job_tree' depth tree =
    w (Printf.sprintf "Depth: %d" depth);
    match tree with
      | Nil ->
	()
      | Mugsy_profile (left, right) -> begin
	pp_job_tree' (depth + 1) left;
	pp_job_tree' (depth + 1) right
      end
      | Mugsy genomes -> begin
	List.iter
	  ~f:w
	  genomes;
	w "\n"
      end
      | Fake_mugsy genome ->
	w (Printf.sprintf "Fake mugsy: %s" genome)
  in
  pp_job_tree' 0 job_tree

let pp w job_tree =
  w "Job tree:";
  pp_job_tree w job_tree

