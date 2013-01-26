open Core.Std

type genomes = { genomes       : Int64.t String.Map.t
	       ; genome_ranges : (Int64.t * Int64.t) list String.Map.t
	       }

type range = (Int64.t * Int64.t)

type fault =
  | Gap of (range * range)
  | Overlap of (range * range)

let map_cons key data m =
  match Map.find m key with
    | Some v -> Map.add ~key ~data:(data::v) m
    | None   -> Map.add ~key ~data:[data] m

let add_seqs genomes sequences =
  List.fold_left
    ~f:(fun genomes seq ->
      let range =
	match Maf.Sequence.range seq with
	  | Maf.Sequence.Range.Forward (s, e) -> (s, e)
	  | Maf.Sequence.Range.Reverse (s, e) -> (s, e)
      in
      let genome        = Maf.Sequence.name seq in
      let genome_ranges = map_cons genome range genomes.genome_ranges in
      let genomes       = { genomes with genome_ranges = genome_ranges } in
      match Map.find genomes.genomes genome with
	| Some _ ->
	  genomes
	| None   ->
	  let genomes' =
	    Map.add
	      ~key:genome
	      ~data:(Maf.Sequence.total seq)
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
    match Maf.Reader.read_next r with
      | Some aln -> begin
	read (add_seqs acc (Maf.Alignment.sequences aln))
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
	  is_contigious ((Overlap (x1, x2))::acc) (x2::xs)
	| _ ->
	  is_contigious ((Gap (x1, x2))::acc) (x2::xs)
    end
  in
  is_contigious [] ranges

let print_missing =
  List.iter
    ~f:(function
      | Overlap ((s1, e1), (s2, e2)) ->
	Int64.(printf "Overlap (%s, %s) (%s, %s) %s\n"
		 (to_string s1)
		 (to_string e1)
		 (to_string s2)
		 (to_string e2)
		 (to_string (s2 - e1)))
      | Gap ((s1, e1), (s2, e2)) -> begin
	Int64.(printf "Gap (%s, %s) %s\n"
		 (to_string (succ e1))
		 (to_string (pred s2))
		 (to_string (s2 - e1)));
	(* Int64.(printf "(%s, %s) (%s, %s)\n" *)
	(* 	 (to_string s1) *)
	(* 	 (to_string e1) *)
	(* 	 (to_string s2) *)
	(* 	 (to_string e2)) *)
      end)

let main () =
  let genomes = read_genomes (Maf.Reader.create stdin) in
  let genomes_names = Map.keys genomes.genomes in
  List.iter
    ~f:(fun genome ->
      match find_missing genome genomes with
	| [] ->
	  ()
	| missing -> begin
	  printf "Genome: %s\n" genome;
	  printf "%s %d faults\n" genome (List.length missing);
	  print_missing missing;
	  print_newline ()
	end)
    genomes_names

let () = main ()
