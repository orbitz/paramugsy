open Core.Std


module Util = struct
  let split_accession acc =
    String.lsplit2_exn ~on:'.' acc

  let string_of_range = function
    | Maf.Sequence.Range.Forward (s, e) ->
      sprintf "+(%s, %s)" (Int64.to_string s) (Int64.to_string e)
    | Maf.Sequence.Range.Reverse (s, e) ->
      sprintf "-(%s, %s)" (Int64.to_string s) (Int64.to_string e)

  let string_of_direction = function
    | Maf.Sequence.Range.Forward _ -> "+"
    | Maf.Sequence.Range.Reverse _ -> "-"

  let start_of_range = function
    | Maf.Sequence.Range.Forward (s, _) -> s
    | Maf.Sequence.Range.Reverse (s, _) -> s

  let end_of_range = function
    | Maf.Sequence.Range.Forward (_, e) -> e
    | Maf.Sequence.Range.Reverse (_, e) -> e

  let extract_range_info = function
    | Maf.Sequence.Range.Forward (s, e) -> ("+", s, Int64.succ e)
    | Maf.Sequence.Range.Reverse (s, e) -> ("-", s, Int64.succ e)
end

module Anchor = struct
  type seq = { genome    : string
	     ; accession : string
	     ; range     : Maf.Sequence.Range.t
	     }
  type t = { pos  : Int64.t
	   ; seqs : seq list
	   }

  let get_accession_exn accession t =
    List.find_exn
      ~f:(fun seq -> seq.accession = accession)
      t.seqs

  let of_alignment pos aln =
    { pos
    ; seqs =
	List.map
	  ~f:(fun s ->
	    let (genome, _) = Util.split_accession (Maf.Sequence.name s) in
	    { genome
	    ; accession = Maf.Sequence.name s
	    ; range     = Maf.Sequence.range s
	    })
	  (Maf.Alignment.sequences aln)
    }
end

module Indicies = struct
  type t = { genome    : int String.Map.t
	   ; accession : int String.Map.t
	   ; anchors   : int list String.Map.t
	   }

  let add_index s idx =
    match Map.find idx s with
      | Some _ -> idx
      | None   -> Map.add ~key:s ~data:(Map.length idx) idx

  let add_pos accession pos idx =
    match Map.find idx accession with
      | Some v -> Map.add ~key:accession ~data:(pos::v) idx
      | None   -> Map.add ~key:accession ~data:[pos] idx

  let build =
    Array.foldi
      ~f:(fun pos acc anchor ->
	List.fold_left
	  ~f:(fun acc seq ->
	    { genome    = add_index seq.Anchor.genome        acc.genome
	    ; accession = add_index seq.Anchor.accession     acc.accession
	    ; anchors   = add_pos   seq.Anchor.accession pos acc.anchors
	    })
	  ~init:acc
	  anchor.Anchor.seqs)
      ~init:{ genome    = String.Map.empty
	    ; accession = String.Map.empty
	    ; anchors   = String.Map.empty
	    }
end

let anchors_of_maf fin =
  let rec read_anchors acc =
    let pos = In_channel.pos fin in
    match Maf.Reader.read_next_channel fin with
      | None ->
	Array.of_list (List.rev acc)
      | Some aln ->
	read_anchors ((Anchor.of_alignment pos aln)::acc)
  in
  read_anchors []

let build_indicies = Indicies.build

let output_synchain fout anchors indicies =
  let accessions = List.sort ~cmp:String.compare (Map.keys indicies.Indicies.anchors) in
  let cmp_start (_, l) (_, r) =
    Maf.Sequence.Range.compare l.Anchor.range r.Anchor.range
  in
  let rec iter_by_2 ~f = function
    | []         -> ()
    | [_]        -> ()
    | x1::x2::xs -> begin f (x1, x2); iter_by_2 ~f (x2::xs) end
  in
  let print_accession accession =
    let seq =
      List.map
	~f:(fun idx -> (idx, Anchor.get_accession_exn accession anchors.(idx)))
	(Map.find_exn indicies.Indicies.anchors accession)
    in
    let sorted_seq =
      List.sort ~cmp:cmp_start seq
    in
    let (genome, _) = Util.split_accession accession in
    let seqidx      = Map.find_exn indicies.Indicies.accession accession in
    let genomeidx   = Map.find_exn indicies.Indicies.genome genome in
    iter_by_2
      ~f:(fun ((idxl, s1), (idxr, s2)) ->
	let dist =
	  let open Int64 in
	  to_int_exn (Util.start_of_range s2.Anchor.range - Util.start_of_range s2.Anchor.range)
	in
	assert (dist = 0);
	let (s1_dir, s1_start, s1_end) = Util.extract_range_info s1.Anchor.range in
	assert (s1_start <> s1_end);
	let (s2_dir, s2_start, s2_end) = Util.extract_range_info s2.Anchor.range in
	assert (s2_start <> s2_end);
	Out_channel.output_string
	  fout
	  (String.concat
	     ~sep:" "
	     [ Int.to_string idxl
	     ; Int.to_string idxr
	     ; Int.to_string seqidx
	     ; Int.to_string dist
	     ; Int.to_string genomeidx
	     ; s1_dir
	     ; s2_dir
	     ; Int64.to_string s1_start
	     ; Int64.to_string s1_end
	     ; Int64.to_string s2_start
	     ; Int64.to_string s2_end
	     ]);
	Out_channel.output_char fout '\n')
      sorted_seq
  in
  List.iter ~f:print_accession accessions

let main () =
  In_channel.with_file
    Sys.argv.(1)
    ~f:(fun fin ->
      let anchors  = anchors_of_maf fin in
      let indicies = build_indicies anchors in
      output_synchain stdout anchors indicies)

let () = main ()
