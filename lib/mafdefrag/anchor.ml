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

let anchors_of_maf fin =
  let rec read_anchors acc =
    let pos = In_channel.pos fin in
    match Maf.Reader.read_next_channel fin with
      | None ->
	List.rev acc
      | Some aln ->
	read_anchors ((of_alignment pos aln)::acc)
  in
  Array.of_list (read_anchors [])
