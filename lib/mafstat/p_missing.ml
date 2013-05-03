open Core.Std

type t = { counted : Int64.t
	 ; genomes : Int64.t String.Map.t
	 }

let create () =
  { counted = Int64.zero; genomes = String.Map.empty }

let add al t =
  let seqs = Maf.Alignment.sequences al in
  let genomes =
    List.fold_left
      ~f:(fun m s ->
	String.Map.add
	  ~key:(Maf.Sequence.name s)
	  ~data:(Maf.Sequence.total s)
	  t.genomes)
      ~init:t.genomes
      seqs
  in
  let counted =
    List.fold_left
      ~f:(fun acc s ->
	Int64.(acc + Maf.Sequence.size s))
      ~init:t.counted
      seqs
  in
  { counted; genomes }

let finalize dir t =
  let total =
    List.fold_left
      ~f:(Int64.(+))
      ~init:Int64.zero
      (String.Map.data t.genomes)
  in
  Out_channel.with_file
    (Filename.concat dir "missing")
    ~f:(fun w ->
      Out_channel.output_lines
	w
	[ String.concat ~sep:"\t" ["counted"; Int64.to_string t.counted]
	; String.concat ~sep:"\t" ["total"; Int64.to_string total]
	])
