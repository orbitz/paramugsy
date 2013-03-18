open Core.Std

type t

type error = [ `Bad_format ]

(*
 * This reads the fasta file upto the first header
 *)
val create : In_channel.t -> (t, [> error ]) Result.t

(*
 * Read upto the next header and return it.  This function is
 * idempotent.
 *)
val read_next_header : t -> string option

(*
 * Read a sequence upto a header
 * Return 0 when there is no more of the sequence to read.
 * otherwise return the amount that has been read.
 * When a sequence has been finished, consecutive calls to
 * this will return 0 until [read_next_header] has been
 * called
 *)
val read_next_seq : t -> buf:string -> pos:int -> len:int -> int
