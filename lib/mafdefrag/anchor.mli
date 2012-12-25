open Core.Std

type seq = { genome    : string
	   ; accession : string
	   ; range     : Maf.Sequence.Range.t
	   }
type t = { pos  : Int64.t
	 ; seqs : seq list
	 }

val get_accession_exn : string -> t -> seq

val anchors_of_maf : In_channel.t -> (t Array.t,  [> `Bad_accession of string ]) Result.t
