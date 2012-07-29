open Core_extended.Std

type genome = string

type pairwise = genome * genome

type job_tree =
  | Nil
  | Mugsy_profile of (t * t)
  | Mugsy of pairwise list
  | Fake_mugsy of genome

type t = { job_tree : job_tree
	 ; pairwise : pairwise list
	 }

let cross left_seqs right_seqs =
  let inner_fold acc e =
    List.fold_left
      ~f:(fun acc' e' ->
	(e, e')::acc')
      ~init:acc
      right_seqs
  in
  List.fold_left
    ~f:inner_fold
    ~init:[]
    left_seqs

let not_equal (x, y) = x <> y

let uniq = List.filter not_equal

let split len l =
  (List.take l (len/2), List.drop l (len/2))

let mk_job max_seqs guide_tree =
  let rec mk_job_from_list l =
    match List.length l with
      | 1 ->
	Fake_mugsy l
      | len when len <= max_seqs ->
	Mugsy (uniq (cross l l))
      | len ->
	let (left, right) = split len l
	in
	Mugsy_profile (mk_job_from_list left, mk_job_from_list right)
  in
  let sequences = Mugsy_guide_tree.list_of_tree guide_tree
  in
  mk_job_from_list sequences

let mk_nucmer max_seqs guide_tree =
  let rec mk_nucmer_from_list l =
    match List.length l with
      | 1 ->
	[]
      | len when len <= max_seqs ->
	(mk_nucmer_from_list l) @ (uniq (cross l l))
      | len ->
	let (left, right) = split len l
	in
	(mk_nucmer_from_list l) @ (cross left right)
  in
  let sequences = Mugsy_guide_tree.list_of_tree guide_tree
  in
  mk_nucmer_from_list sequences

let make_job max_seqs sequences =
  let guide_tree = Mugsy_guide_tree.gudie_tree_of_sequenes sequences
  in
  { job_tree = mk_job max_seqs guide_tree
  ; pairwise = mk_nucmer max_seqs guide_tree
  }
