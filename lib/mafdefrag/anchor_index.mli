open Core.Std

type t = { genome    : int String.Map.t
	 ; accession : int String.Map.t
	 ; anchors   : int list String.Map.t
	 }

val build : Anchor.t Array.t -> t

