open Core.Std

type t = { no_gaps : (int * Int64.t) list
	 ; genomes : Int64.t String.Map.t
	 }

let create () =
  { no_gaps = []
  ; genomes = String.Map.empty
  }

let has_gap s = String.contains s '-'

let rec any = function
  | []      -> false
  | true::_ -> true
  | _::xs   -> any xs

let add al t =
  let seqs = Maf.Alignment.sequences al in
  let with_gaps =
    List.map
      ~f:(fun s -> has_gap (Maf.Sequence.sequence s))
      seqs
  in
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
  if not (any with_gaps) then
    let num = List.length seqs in
    let bp  =
      List.fold_left
	~f:(fun acc s ->
	  Int64.(acc + Maf.Sequence.size s))
	~init:Int64.zero
	seqs
    in
    { no_gaps = (num, bp)::t.no_gaps
    ; genomes = genomes
    }
  else
    t

let finalize dir t =
  let total_genomes = Map.length t.genomes in
  let total_bp =
    List.fold_left
      ~f:Int64.(+)
      ~init:Int64.zero
      (Map.data t.genomes)
  in
  let core =
    List.filter
      ~f:(fun (n, _) -> n = total_genomes)
      t.no_gaps
  in
  let core_bp =
    List.fold_left
      ~f:(fun acc (_, bp) ->
	Int64.(acc + bp))
      ~init:Int64.zero
      core
  in
  Out_channel.with_file
    (Filename.concat dir "core_genome")
    ~f:(fun w ->
      Out_channel.output_lines
	w
	[ String.concat ~sep:"\t" ["num_genomes"; Int.to_string total_genomes]
	; String.concat ~sep:"\t" ["core"; Int64.to_string core_bp]
	; String.concat ~sep:"\t" ["total"; Int64.to_string total_bp]
	])
