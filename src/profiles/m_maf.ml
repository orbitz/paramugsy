(*pp camlp4o *)
open Core_extended.Std
open Core_extended
open Ort
open Ort.Function
open Bio

type options = { in_fasta : Fileutils.file_path
	       ; out_maf : Fileutils.file_path
	       }


let maf_of_fasta in_fasta =
  (* We have an initial reader so we can calculate
   * sequence length, then a follower so we can write the MAF file out
   *)
  let remove_newlines s =
    s |> String.split_on_chars ~on:[' '; '\n'; '\r'; '\t'] |> String.concat ~sep:""
  in
  let next_seq_len sin =
    match Seq.next sin with
      | Some (Fasta.Header _) ->
	Some (sin
		 |> Seq.take_while ~f:(function | Fasta.Header _ -> false | Fasta.Sequence _ -> true)
		 |> Seq.fold ~f:(fun a -> function 
		     | Fasta.Sequence s -> a + String.length (remove_newlines s)
		     | _ -> raise (Failure "This should never happen")) ~init:0)
      | _ -> None
  in
  let sequence_name h =
    match String.split_on_chars ~on:[':'] h with
      | genome::sequence::_ ->
	genome ^ "." ^ sequence
      | _ ->
	h
  in
  let write_next_seq len sin =
    match Seq.next sin with
      | Some (Fasta.Header h) -> begin
	let seqs = (sin 
		       |> Seq.take_while ~f:(function | Fasta.Header _ -> false | Fasta.Sequence _ -> true)
		       |> Seq.map ~f:(function | Fasta.Sequence s -> (remove_newlines s) | _ -> raise (Failure "OMGBBQ"))
		       |> Seq.to_list)
	in
	let sequence = String.concat seqs in
	[< '"a score=0"
	;  'Printf.sprintf "s %s 0 %d + %d %s" (sequence_name (remove_newlines h)) len len sequence
	;  '""
	>]
      end
      | _ ->
	raise (Failure "Failed to parse fasta file")
  in
  let rec write_maf lead follow =
    match next_seq_len lead with
      | Some l -> begin
	[< write_next_seq l follow
	;  write_maf lead follow
	>]
      end
      | None ->
	[< >]
  in
  let lead = Fasta.read_file in_fasta in
  let follow = Fasta.read_file in_fasta in
  write_maf lead follow


let usage = ""

let parse_argv argv =
  let in_fasta = ref "" in
  let out_maf = ref "" in
  
  let params =
    Arg.align [ "-in_fasta", Arg.Set_string in_fasta, "Path Path to input fasta file" 
	      ; "-out_maf", Arg.Set_string out_maf, "Path Path to output MAF file"
	      ]
  in
  Arg.parse params (fun _ -> ()) usage;
  if !in_fasta = "" then
    raise (Failure "Must provide -in_fasta")
  else if !out_maf = "" then
    raise (Failure "Must provide -out_maf")
  else
    { in_fasta = !in_fasta
    ; out_maf = !out_maf
    }


let main argv =
  let options = parse_argv argv in
  Shell.mkdir_p (Fileutils.dirname options.out_maf);
  let fout = open_out options.out_maf in
  let rec print_lines sin =
    match Seq.next sin with
      | Some l -> begin
	output_string fout l;
	output_char fout '\n';
	print_lines sin
      end
      | None ->
	()
  in
  print_lines (maf_of_fasta options.in_fasta)
