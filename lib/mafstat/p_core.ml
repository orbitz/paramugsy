open Core.Std

type t = { no_gaps_columns : (int * int) list
	 ; genomes         : Int64.t String.Map.t
	 }

let create () =
  { no_gaps_columns = []
  ; genomes         = String.Map.empty
  }

let collapse_seq s1 seq =
  let s2 = Maf.Sequence.sequence seq in
  assert (String.length s1 = String.length s2);
  for i = 0 to String.length s1 - 1 do
    if s2.[i] = '-' then
      s1.[i] <- '-'
  done;
  s1

let add al t =
  let seqs = Maf.Alignment.sequences al in
  let hd   = List.hd_exn seqs in
  let collapsed =
    List.fold_left
      ~f:collapse_seq
      ~init:(Maf.Sequence.sequence hd)
      (List.tl_exn seqs)
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
  let num_gaps = String.count ~f:((=) '-') collapsed in
  let num_bp = String.length collapsed - num_gaps in
  if num_gaps > 0 then
    let num = List.length seqs in
    { no_gaps_columns = (num, num_bp)::t.no_gaps_columns
    ; genomes         = genomes
    }
  else
    { t with genomes = genomes }

let finalize dir t =
  let genomes =
    Map.fold
      ~f:(fun ~key ~data acc ->
	let (genome, _) = String.lsplit2_exn ~on:'.' key in
	Map.change
	  acc
	  genome
	  (function
	    | Some v -> Some Int64.(v + data)
	    | None   -> Some data))
      ~init:String.Map.empty
      t.genomes
  in
  let min_genome_size =
    List.hd_exn
      (List.sort ~cmp:Int64.compare (Map.data genomes))
  in
  let total_genomes = Map.length genomes in
  let core =
    List.filter
      ~f:(fun (n, _) -> n = total_genomes)
      t.no_gaps_columns
  in
  let core_cols =
    List.fold_left
      ~f:(fun acc (_, bp) ->
	Int64.(acc + of_int bp))
      ~init:Int64.zero
      core
  in
  Out_channel.with_file
    (Filename.concat dir "core_genome")
    ~f:(fun w ->
      Out_channel.output_lines
	w
	[ String.concat ~sep:"\t" ["num_genomes"; Int.to_string total_genomes]
	; String.concat ~sep:"\t" ["core"; Int64.to_string core_cols]
	; String.concat ~sep:"\t" ["shortest_genome_len"; Int64.to_string min_genome_size]
	])
