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
end

module Anchor = struct
  type seq = { genome    : string
	     ; accession : string
	     ; range     : Maf.Sequence.Range.t
	     }
  type t = seq list

  let get_accession_exn accession t =
    List.find_exn
      ~f:(fun seq -> seq.accession = accession)
      t

  let of_alignment aln =
    List.map
      ~f:(fun s ->
	let (genome, _) = Util.split_accession (Maf.Sequence.name s) in
	{ genome
	; accession = Maf.Sequence.name s
	; range     = Maf.Sequence.range s
	})
      (Maf.Alignment.sequences aln)
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
	  anchor)
      ~init:{ genome    = String.Map.empty
	    ; accession = String.Map.empty
	    ; anchors   = String.Map.empty
	    }
end

let anchors_of_maf fin =
  let rec read_anchors acc r =
    match Maf.Reader.read_next r with
      | None ->
	Array.of_list acc
      | Some aln ->
	read_anchors ((Anchor.of_alignment aln)::acc) r
  in
  read_anchors [] (Maf.Reader.create fin)

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
	Out_channel.output_string
	  fout
	  (String.concat
	     ~sep:" "
	     [ Int.to_string idxl
	     ; Int.to_string idxr
	     ; Int.to_string seqidx
	     ; Int.to_string dist
	     ; Int.to_string genomeidx
	     ; Util.string_of_direction s1.Anchor.range
	     ; Util.string_of_direction s2.Anchor.range
	     ; Int64.to_string (Util.start_of_range s1.Anchor.range)
	     ; Int64.to_string (Util.start_of_range s2.Anchor.range)
	     ; Int64.to_string (Util.end_of_range s1.Anchor.range)
	     ; Int64.to_string (Util.end_of_range s2.Anchor.range)
	     ; "\n"
	     ]))
      sorted_seq
  in
  List.iter ~f:print_accession accessions

let main () =
  let anchors  = anchors_of_maf stdin in
  let indicies = build_indicies anchors in
  output_synchain stdout anchors indicies

let () = main ()
