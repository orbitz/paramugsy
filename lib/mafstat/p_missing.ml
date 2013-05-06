open Core.Std

type t = { counted       : Int64.t
	 ; genome_ranges : (Int64.t * Int64.t) list String.Map.t
	 ; genomes       : Int64.t String.Map.t
	 }

let map_cons key data m =
  match Map.find m key with
    | Some v -> Map.add ~key ~data:(data::v) m
    | None   -> Map.add ~key ~data:[data] m

let create () =
  { counted       = Int64.zero
  ; genome_ranges = String.Map.empty
  ; genomes       = String.Map.empty
  }

let add al t =
  let seqs = Maf.Alignment.sequences al in
  let genomes =
    List.fold_left
      ~f:(fun m s ->
	Map.add
	  ~key:(Maf.Sequence.name s)
	  ~data:(Maf.Sequence.total s)
	  m)
      ~init:t.genomes
      seqs
  in
  let genome_ranges =
    List.fold_left
      ~f:(fun m s ->
	let r =
	  match Maf.Sequence.range s with
	    | Maf.Sequence.Range.Forward (s, e) -> (s, e)
	    | Maf.Sequence.Range.Reverse (s, e) -> (s, e)
	in
	map_cons (Maf.Sequence.name s) r m)
      ~init:t.genome_ranges
      seqs
  in
  let counted =
    List.fold_left
      ~f:(fun acc s ->
	Int64.(acc + Maf.Sequence.size s))
      ~init:t.counted
      seqs
  in
  { counted; genome_ranges; genomes }

let count_unique genome_ranges genome_len =
  let sorted =
    List.sort
      ~cmp:(fun x y -> Int64.compare (fst x) (fst y))
      genome_ranges
  in
  let rec find_missing acc = function
    | [] ->
      failwith "Impossible"
    | [(_, e)] when Int64.(e = genome_len) ->
      acc
    | [(_, e)] ->
      Int64.(acc + (genome_len - e - one))
    | x1::x2::xs -> begin
      match Int64.(fst x2 - snd x1) with
	| d when Int64.(d = one) ->
	  find_missing acc (x2::xs)
	| d when Int64.(d > one) ->
	  find_missing Int64.(acc + d - one) (x2::xs)
	| _ ->
	  find_missing acc (x2::xs)
    end
  in
  Int64.(genome_len - find_missing Int64.zero sorted)

let finalize dir t =
  let total =
    List.fold_left
      ~f:(Int64.(+))
      ~init:Int64.zero
      (Map.data t.genomes)
  in
  let unique =
    Map.fold
      ~f:(fun ~key ~data acc ->
	let genome_len = Map.find_exn t.genomes key in
	let unique = count_unique data genome_len in
	Int64.(acc + unique))
      ~init:Int64.zero
      t.genome_ranges
  in
  Out_channel.with_file
    (Filename.concat dir "missing")
    ~f:(fun w ->
      Out_channel.output_lines
	w
	[ String.concat ~sep:"\t" ["counted"; Int64.to_string t.counted]
	; String.concat ~sep:"\t" ["unique"; Int64.to_string unique]
	; String.concat ~sep:"\t" ["total"; Int64.to_string total]
	])
