open Core.Std

let create_chain_index chained =
  let idx =
    Array.fold
      ~f:(fun m c ->
	List.fold_left
	  ~f:(fun m s ->
	    let range = s.Synchain.range in
	    match Map.find m s.Synchain.accession_idx with
	      | Some l ->
		Map.add m ~key:s.Synchain.accession_idx ~data:(range::l)
	      | None ->
		Map.add m ~key:s.Synchain.accession_idx ~data:[range])
	  ~init:m
	  c)
      ~init:Int.Map.empty
      chained
  in
  Map.fold
    ~f:(fun ~key ~data acc ->
      let sorted = List.sort ~cmp:Maf.Sequence.Range.compare data in
      Map.add acc ~key ~data:sorted)
    ~init:Int.Map.empty
    idx

let create_anchors_index indicies =
  Anchor_index.seq_anchors_fold
    ~f:(fun ~key ~data m ->
      let ranges =
	List.map
	  ~f:(fun idx ->
	    let seq = Anchor.get_accession_exn key (Anchor_index.anchors_exn idx indicies) in
	    seq.Anchor.range)
	  data
      in
      let accession_idx = Anchor_index.accession_exn key indicies in
      Map.add m ~key:accession_idx ~data:(List.sort ~cmp:Maf.Sequence.Range.compare ranges))
    ~init:Int.Map.empty
    indicies

let rec is_contigious acc = function
  | []  ->
    acc
  | [_] ->
    acc
  | x1::x2::xs -> begin
    let open Int64 in
    if (Util.start_of_range x2 - Util.end_of_range x1) = one then
      is_contigious acc (x2::xs)
    else
      is_contigious ((x1, x2)::acc) (x2::xs)
  end

let verify indicies chained =
  let chained_idx = create_chain_index chained in
  ignore (create_anchors_index indicies);
  Map.fold
    ~f:(fun ~key ~data acc ->
      match is_contigious [] data with
	| [] ->
	  acc
	| l ->
	  (List.map ~f:(fun (x1, x2) -> (key, x1, x2)) l) @ acc)
    ~init:[]
    chained_idx
