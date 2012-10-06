open Core.Std
open Ort

module Genome_name : sig
  type t
  include Identifier with type t := t
end

module Job_tree : sig
  type t =
    | Nil
    | Mugsy_profile of (t * t)
    | Mugsy of Genome_name.t list
    | Fake_mugsy of Genome_name.t
end

type t = { job_tree   : Job_tree.t
	 ; genome_map : Fileutils.file_path Genome_name.Map.t
	 }

val make_job  : int -> Fileutils.file_path list -> t
val pairwise  : Job_tree.t -> (Genome_name.t * Genome_name.t) list
val pp        : out_channel -> t -> unit
val pp_stdout : t -> unit
