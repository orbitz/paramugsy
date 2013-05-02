open Core.Std

module Fault = Maf.Missing_seqs.Fault

type error = [ `Overlap ]

type genome_faults = (string * Fault.t list) list
type gap_ranges = (string * (Fault.range * Fault.range) list) list

let no_overlaps =
  let rec no_overlaps' acc = function
    | [] ->
      Ok acc
    | (Fault.Overlap _)::_ ->
      Error `Overlap
    | (Fault.Gap r)::rs ->
      no_overlaps' (r::acc) rs
  in
  no_overlaps' []

let enforce_no_overlaps : genome_faults -> (gap_ranges, [> `Overlap ]) Result.t =
  let rec enforce_no_overlaps' acc = function
    | [] ->
      Ok acc
    | (genome, missing)::xs ->
      let open Result.Monad_infix in
      no_overlaps missing >>= fun gaps ->
      enforce_no_overlaps' ((genome, gaps)::acc) xs
  in
  enforce_no_overlaps' []

let make_genome_map =
  List.fold_left
    ~f:(fun m (genome, missing) ->
      String.Map.add ~key:genome ~data:missing m)
    ~init:String.Map.empty

let fill_from_fasta fasta genome_map =
  In_channel.with_file
    fasta
    ~f:(fun fin ->
      let r = Fasta.Reader.create fin in
      match Fasta.Reader.next_header r with
	| Some h -> printf "Header: %s\n" h
	| None   -> printf "No header %s\n" fasta)

let write_maf gaps fastas =
  let genome_map = make_genome_map gaps in
  List.iter
    ~f:(Fn.flip fill_from_fasta genome_map)
    fastas;
  Ok ()

let run () =
  let open Result.Monad_infix in
  let missing = Maf.Missing_seqs.find (Maf.Reader.create stdin) in
  let fastas  = List.tl_exn (Array.to_list Sys.argv) in
  enforce_no_overlaps missing >>= fun gaps ->
  write_maf gaps fastas

let main () =
  match run () with
    | Ok () ->
      ()
    | Error `Overlap ->
      exit 1

let () = main ()
