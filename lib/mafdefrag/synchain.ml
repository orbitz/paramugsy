open Core.Std

type seq = { accession_idx : int
	   ; range         : Maf.Sequence.Range.t
	   }

type t = seq list

let read fin =
  let split_i l =
    String.split ~on:' ' l
  in
  let rec parse_seqs acc = function
    | [";"] ->
      Some (List.rev acc)
    | seq::d::start_stop::xs -> begin
      match String.lsplit2 ~on:'-' start_stop with
	| Some (start, stop) -> begin
	  let start = Int64.of_string start in
	  let stop  = Int64.of_string stop in
	  let seq   = Int.of_string seq in
	  assert (start < stop);
	  let range =
	    match d with
	      | "+" ->
		Maf.Sequence.Range.Forward (start, stop)
	      | "-" ->
		Maf.Sequence.Range.Reverse (start, stop)
	      | _ -> failwith "UGH"
	  in
	  parse_seqs ({accession_idx = seq;  range = range}::acc) xs
	end
	| None ->
	  None
    end
    | _ ->
      None
  in
  let parse_i = function
    | "I"::xs -> parse_seqs [] xs
    | _       -> None
  in
  let rec find_i acc =
    match In_channel.input_line fin with
      | Some l when String.is_prefix ~prefix:"I " (String.strip l) ->
	let open Option.Monad_infix in
	parse_i (split_i l) >>= fun i ->
	find_i (i::acc)
      | Some l when String.is_prefix ~prefix:"V " (String.strip l) ->
	find_i acc
      | Some _ ->
	None
      | None ->
	Some (List.rev acc)
  in
  let open Option.Monad_infix in
  find_i [] >>= fun acc ->
  Some (Array.of_list acc)
