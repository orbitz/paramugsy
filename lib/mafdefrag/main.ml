open Core.Std

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

let run_synchain ifname ofname = ()

let convert_to_maf fin fout anchors indicies =
  ignore (Synchain.read fin)


let main () =
  let anchors =
    In_channel.with_file
      Sys.argv.(1)
      ~f:(fun fin -> Anchor.anchors_of_maf fin)
  in
  let indicies = build_indicies anchors in
  Out_channel.with_file
    Sys.argv.(2)
    ~f:(fun fout -> output_synchain fout anchors indicies);
  run_synchain Sys.argv.(2) Sys.argv.(3);
  In_channel.with_file
    Sys.argv.(3)
    ~f:(fun fin -> convert_to_maf fin stdout anchors indicies)

let () = main ()
