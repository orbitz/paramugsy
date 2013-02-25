open Core.Std

type genomes = { genomes       : Int64.t String.Map.t
	       ; genome_ranges : (Int64.t * Int64.t) list String.Map.t
	       }

module Fault = struct
  type range = (Int64.t * Int64.t)

  type t =
    | Gap     of (range * range)
    | Overlap of (range * range)
end

let map_cons key data m =
  match Map.find m key with
    | Some v -> Map.add ~key ~data:(data::v) m
    | None   -> Map.add ~key ~data:[data] m

let add_seqs genomes sequences =
  List.fold_left
    ~f:(fun genomes seq ->
      let range =
	match Sequence.range seq with
	  | Sequence.Range.Forward (s, e) -> (s, e)
	  | Sequence.Range.Reverse (s, e) -> (s, e)
      in
      let genome        = Sequence.name seq in
      let genome_ranges = map_cons genome range genomes.genome_ranges in
      let genomes       = { genomes with genome_ranges = genome_ranges } in
      match Map.find genomes.genomes genome with
	| Some _ ->
	  genomes
	| None   ->
	  let genomes' =
	    Map.add
	      ~key:genome
	      ~data:(Sequence.total seq)
	      genomes.genomes in
	  { genomes with genomes = genomes' })
    ~init:genomes
    sequences

let read_genomes r =
  let genomes = { genomes       = String.Map.empty
		; genome_ranges = String.Map.empty
		}
  in
  let rec read acc =
    match Reader.read_next r with
      | Some aln -> begin
	read (add_seqs acc (Alignment.sequences aln))
      end
      | None ->
	acc
  in
  read genomes

let find_missing genome genomes =
  let ranges = Map.find_exn genomes.genome_ranges genome in
  let ranges = List.sort ~cmp:(fun x y -> Int64.compare (fst x) (fst y)) ranges in
  let rec is_contigious acc = function
    | []  ->
      List.rev acc
    | [_] ->
      List.rev acc
    | x1::x2::xs -> begin
      match Int64.(fst x2 - snd x1) with
	| d when Int64.(d = one) ->
	  is_contigious acc (x2::xs)
	| d when Int64.(d < one) ->
	  is_contigious ((Fault.Overlap (x1, x2))::acc) (x2::xs)
	| _ ->
	  is_contigious ((Fault.Gap (x1, x2))::acc) (x2::xs)
    end
  in
  is_contigious [] ranges

let find r =
  let genomes       = read_genomes r in
  let genomes_names = Map.keys genomes.genomes in
  List.fold_left
    ~f:(fun missing genome ->
      match find_missing genome genomes with
	| [] ->
	  missing
	| m ->
	  (genome, m)::missing)
    ~init:[]
    genomes_names

