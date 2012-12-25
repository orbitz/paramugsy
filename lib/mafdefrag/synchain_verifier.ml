open Core.Std

let create_chain_index chained =
  let idx =
    Array.fold
      ~f:(fun m c ->
	List.fold_left
	  ~f:(fun m s ->
	    match Map.find m s.Synchain.accession_idx with
	      | Some l ->
		Map.add m ~key:s.Synchain.accession_idx ~data:(s.Synchain.range::l)
	      | None ->
		Map.add m ~key:s.Synchain.accession_idx ~data:[s.Synchain.range])
	  ~init:m
	  c)
      ~init:Int.Map.empty
      chained
  in
  Map.fold
    ~f:(fun ~key ~data acc ->
      Map.add acc ~key ~data:(List.sort ~cmp:Maf.Sequence.Range.compare data))
    ~init:Int.Map.empty
    idx

let create_anchors_index anchors indicies =
  Map.fold
    ~f:(fun ~key ~data m ->
      let ranges =
	List.map
	  ~f:(fun idx ->
	    let seq = Anchor.get_accession_exn key anchors.(idx) in
	    seq.Anchor.range)
	  data
      in
      let accession_idx = Map.find_exn indicies.Anchor_index.accession key in
      Map.add m ~key:accession_idx ~data:(List.sort ~cmp:Maf.Sequence.Range.compare ranges))
    ~init:Int.Map.empty
    indicies.Anchor_index.anchors

let rec is_contigious = function
  | []  ->
    true
  | [_] ->
    true
  | x1::x2::xs ->
    let open Int64 in
    if (Util.end_of_range x1 - Util.start_of_range x2) = one then
      is_contigious (x2::xs)
    else
      false

let verify anchors indicies chained =
  let chained_idx = create_chain_index chained in
  ignore (create_anchors_index anchors indicies);
  Map.fold
    ~f:(fun ~key ~data acc ->
      if is_contigious data then
	acc
      else
	key::acc)
    ~init:[]
    chained_idx
