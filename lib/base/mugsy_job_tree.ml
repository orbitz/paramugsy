open Core_extended
open Core.Std

type 'a job_tree =
  | Job of 'a
  | Serial of 'a job_tree list
  | Parallel of 'a job_tree list

(*
 * All of the parameters that will be used in turning a guide tree into a job tree
 *)
type job_parameters = { max_seqs_per_nucmer : int (* Maximum number of seqs to calculate per nucmer run *)
		      ; max_jobs_per_nucmer : int (* Maximum number of parallel jobs to make per nucmer run *)
		      }

(* Fairly arbitrary values *)
let default_job_parameters = { max_seqs_per_nucmer = 10
			     ; max_jobs_per_nucmer = 10
			     }

(*
 * This turns a guide tree into a job tree based on the job parameters given.
 * The result is a tree where each parent depends on the work of its child.
 * The algorithm to create the job tree looks like so:
 *
 * 1) For each branch, descend until the bottom is hit
 * 2) Collect sequences until max_seqs_per_nucmer is reached
 * 3) Construct a parallel job containing N nucmers based on max_jobs_per_nucmer
 *)
let job_tree_of_guide_tree params t = ()
