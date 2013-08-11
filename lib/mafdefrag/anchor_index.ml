open Core.Std

type t = { genome      : int String.Map.t
	 ; accession   : int String.Map.t
	 ; seq_anchors : int list String.Map.t
	 ; anchors     : Anchor.t Array.t
	 }

type string_map_key = String.Map.Key.t

let add_index s idx =
  match Map.find idx s with
    | Some _ -> idx
    | None   -> Map.add ~key:s ~data:(Map.length idx) idx

let add_pos accession pos idx =
  match Map.find idx accession with
    | Some v -> Map.add ~key:accession ~data:(pos::v) idx
    | None   -> Map.add ~key:accession ~data:[pos] idx

let build anchors =
  Array.foldi
    ~f:(fun pos acc anchor ->
      List.fold_left
	~f:(fun acc seq ->
	  { acc with
            genome      = add_index seq.Anchor.genome        acc.genome
	  ; accession   = add_index seq.Anchor.accession     acc.accession
	  ; seq_anchors = add_pos   seq.Anchor.accession pos acc.seq_anchors
	  })
	~init:acc
	anchor.Anchor.seqs)
    ~init:{ genome      = String.Map.empty
	  ; accession   = String.Map.empty
	  ; seq_anchors = String.Map.empty
	  ; anchors     = anchors
	  }
    anchors

let genome_exn g t =
  Map.find_exn t.genome g

let genome_keys t =
  Map.keys t.genome

let accession_exn acc t =
  Map.find_exn t.accession acc

let accession_keys t =
  Map.keys t.accession

let accession_fold ~f ~init t =
  Map.fold ~f ~init t.accession

let seq_anchors_exn acc t =
  Map.find_exn t.seq_anchors acc

let seq_anchors_keys t =
  Map.keys t.seq_anchors

let seq_anchors_fold ~f ~init t =
  Map.fold ~f ~init t.seq_anchors

let anchors_exn idx t =
  t.anchors.(idx)
