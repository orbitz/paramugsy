open Core.Std
open Async.Std

open Ort

module Genome_name : sig
  type t
  include Identifier with type t := t
end

type t =
  | Nil
  | Mugsy_profile of (t * t)
  | Mugsy of Fileutils.file_path list
  | Fake_mugsy of Fileutils.file_path

val make_job  : int -> Fileutils.file_path list -> t Deferred.t
val pairwise  : t -> (Fileutils.file_path * Fileutils.file_path) list
val pp        : (string -> unit) -> t -> unit
