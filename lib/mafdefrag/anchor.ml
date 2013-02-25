open Core.Std

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
  let open Result.Monad_infix in
  Result.all
    (List.map
       ~f:(fun s ->
	 match Util.split_accession (Maf.Sequence.name s) with
	   | Some (genome, _) ->
	     Ok { genome
		; accession = Maf.Sequence.name s
		; range     = Maf.Sequence.range s
		}
	  | None ->
	    Error (`Bad_accession (Maf.Sequence.name s)))
       (Maf.Alignment.sequences aln))
  >>= fun seqs ->
  Ok { pos; seqs }

let anchors_of_maf fin =
  let rec read_anchors acc =
    let pos = In_channel.pos fin in
    match Maf.Reader.read_next_channel fin with
      | None ->
	Ok (List.rev acc)
      | Some aln ->
	let open Result.Monad_infix in
	of_alignment pos aln >>= fun anchor ->
	read_anchors (anchor::acc)
  in
  let open Result.Monad_infix in
  read_anchors [] >>= fun anchors ->
  Ok (Array.of_list anchors)
