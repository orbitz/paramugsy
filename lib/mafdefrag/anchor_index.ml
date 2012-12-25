open Core.Std

type t = { genome    : int String.Map.t
	 ; accession : int String.Map.t
	 ; anchors   : int list String.Map.t
	 }

let add_index s idx =
  match Map.find idx s with
    | Some _ -> idx
    | None   -> Map.add ~key:s ~data:(Map.length idx) idx

let add_pos accession pos idx =
  match Map.find idx accession with
    | Some v -> Map.add ~key:accession ~data:(pos::v) idx
    | None   -> Map.add ~key:accession ~data:[pos] idx

let build =
  Array.foldi
    ~f:(fun pos acc anchor ->
      List.fold_left
	~f:(fun acc seq ->
	  { genome    = add_index seq.Anchor.genome        acc.genome
	  ; accession = add_index seq.Anchor.accession     acc.accession
	  ; anchors   = add_pos   seq.Anchor.accession pos acc.anchors
	  })
	~init:acc
	anchor.Anchor.seqs)
    ~init:{ genome    = String.Map.empty
	  ; accession = String.Map.empty
	  ; anchors   = String.Map.empty
	  }
