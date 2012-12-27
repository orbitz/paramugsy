open Core.Std

let output_synchain fout anchors indicies =
  let split_accession_exn acc =
    match Util.split_accession acc with
      | Some x -> x
      | None -> failwith "Whoops"
  in
  let accessions = List.sort ~cmp:String.compare (Map.keys indicies.Anchor_index.anchors) in
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
	(Map.find_exn indicies.Anchor_index.anchors accession)
    in
    let sorted_seq =
      List.sort ~cmp:cmp_start seq
    in
    let (genome, _) = split_accession_exn accession in
    let seqidx      = Map.find_exn indicies.Anchor_index.accession accession in
    let genomeidx   = Map.find_exn indicies.Anchor_index.genome genome in
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

let build_anchors in_maf =
  In_channel.with_file
    in_maf
    ~f:Anchor.anchors_of_maf

let build_indicies anchors = Ok (Anchor_index.build anchors)

let write_synchain synchain_file anchors indicies =
  Out_channel.with_file
    synchain_file
    ~f:(fun fout -> Ok (output_synchain fout anchors indicies))

let run_synchain synchain_file chained_file = Ok ()

let read_chained chained_file =
  In_channel.with_file
    chained_file
    ~f:(fun fin -> Synchain.read fin)

let verify_chained anchors indicies chained =
  match Synchain_verifier.verify anchors indicies chained with
    | [] ->
      Ok ()
    | l ->
      Error (`Failed_verification l)

let write_maf chained out_maf = Ok ()

let run () =
  let in_maf        = Sys.argv.(1) in
  let synchain_file = Sys.argv.(2) in
  let chained_file  = Sys.argv.(3) in
  let out_maf       = Sys.argv.(4) in
  let open Result.Monad_infix      in
  printf "Reading anchors...\n%!";
  build_anchors in_maf
  >>= fun anchors ->
  printf "Building indicies...\n%!";
  build_indicies anchors
  >>= fun indicies ->
  printf "Writing synchain...\n%!";
  write_synchain synchain_file anchors indicies
  >>= fun () ->
  printf "Running synchain...\n%!";
  run_synchain synchain_file chained_file
  >>= fun () ->
  printf "Reading chained...\n%!";
  read_chained chained_file
  >>= fun chained ->
  printf "Verifying chained...\n%!";
  verify_chained anchors indicies chained
  >>= fun () ->
  printf "Writing maf...\n%!";
  write_maf chained out_maf

let main () =
  match run () with
    | Ok () ->
      0
    | Error (`Bad_chain_file line) -> begin
      fprintf stderr "Bad chain file: '%s'\n" line;
      1
    end
    | Error (`Failed_verification failed) -> begin
      fprintf stderr "Chained file faild verification\n";
      List.iter
	~f:(fun (acc_idx, r1, r2) ->
	  fprintf stderr "%d (%s, %s) (%s, %s) - %s\n"
	    acc_idx
	    (Int64.to_string (Util.start_of_range r1))
	    (Int64.to_string (Util.end_of_range r1))
	    (Int64.to_string (Util.start_of_range r2))
	    (Int64.to_string (Util.end_of_range r2))
	    (Int64.to_string (Int64.(-)
				(Util.start_of_range r2)
				(Util.end_of_range r1))))
	(List.rev failed);
      2
    end
    | Error (`Bad_accession acc) -> begin
      fprintf stderr "Bad accession: %s\n" acc;
      3
    end

let () = exit (main ())
