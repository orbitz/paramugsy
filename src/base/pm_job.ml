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
open Ort

module Genome_name : Identifier = String

module Genome_map = Genome_name.Map

module Job_tree = struct
  type t =
    | Nil
    | Mugsy_profile of (t * t)
    | Mugsy of Genome_name.t list
    | Fake_mugsy of Genome_name.t
end

type t = { job_tree   : Job_tree.t
	 ; genome_map : Fileutils.file_path Genome_map.t
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

let split len l =
  (List.take l (len/2), List.drop l (len/2))

let genome_from_file = Core.Fn.compose Genome_name.of_string Fileutils.basename

let mk_job max_seqs guide_tree =
  let rec mk_job_from_list l =
    match List.length l with
      | 1 ->
	Job_tree.Fake_mugsy (List.hd_exn l)
      | len when len <= max_seqs ->
	Job_tree.Mugsy l
      | len ->
	let (left, right) = split len l
	in
	Job_tree.Mugsy_profile (mk_job_from_list left, mk_job_from_list right)
  in
  let sequences =
    List.map
      ~f:genome_from_file
      (Mugsy_guide_tree.list_of_guide_tree guide_tree)
  in
  mk_job_from_list sequences

let create_genome_map sequences _ =
  List.fold_left
    ~f:(fun acc s -> Genome_map.add ~key:(genome_from_file s) ~data:s acc)
    ~init:Genome_map.empty
    sequences

let make_job max_seqs sequences =
  let guide_tree = Mugsy_guide_tree.guide_tree_of_sequences sequences
  in
  { job_tree   = mk_job max_seqs guide_tree
  ; genome_map = create_genome_map sequences guide_tree
  }


let pp_job_tree fout job_tree =
  let rec pp_job_tree' depth tree =
    Printf.fprintf fout "Depth: %d\n" depth;
    match tree with
      | Job_tree.Nil ->
	()
      | Job_tree.Mugsy_profile (left, right) -> begin
	pp_job_tree' (depth + 1) left;
	pp_job_tree' (depth + 1) right
      end
      | Job_tree.Mugsy genomes ->
	List.iter
	  ~f:(Core.Fn.compose
		(Printf.fprintf fout "%s ")
		Genome_name.to_string)
	  genomes
      | Job_tree.Fake_mugsy genome ->
	Printf.fprintf fout "Fake mugsy: %s\n" (Genome_name.to_string genome)
  in
  pp_job_tree' 0 job_tree

let pp fout jt =
  output_string fout "Job tree:\n";
  pp_job_tree fout jt.job_tree
