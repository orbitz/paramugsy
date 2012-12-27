open Core.Std

type seq = { accession_idx : int
	   ; range         : Maf.Sequence.Range.t
	   }

type t = seq list

val read : In_channel.t -> (t Array.t, [> `Bad_chain_file of string ]) Result.t
