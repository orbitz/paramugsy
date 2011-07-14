(*pp camlp4o *)
open Core_extended.Std
open Ort
open Ort.Function

let combinations l1 l2 =
  let rec inner o = function
    | [] ->
      [< >]
    | x::xs ->
      [< '(o, x)
      ;  inner o xs
      >]
  in
  let rec outter = function
    | [] ->
      [< >]
    | x::xs ->
      [< inner x l2
      ;  outter xs
      >]
  in
  outter l1

let xmfa_of_maf in_maf =
  let rec write_xmfa = function
    | (name, start, size, d, src_size, text)::xs ->
      [< 'Printf.sprintf ">%s %d %d %s %d" name start size (M_profile_stream.string_of_direction d) src_size
      ;  'text
      ;  write_xmfa xs
      >]
    | [] ->
      [< '"=" >]
  in
  let rec convert_to_xmfa fin =
    match Seq.next fin with
      | Some l when String.strip l = "" || l.[0] = '#' ->
	convert_to_xmfa fin
      | Some l when String.is_prefix ~prefix:"a " l -> begin
	let alignments = 
	  fin
            |> Seq.take_while ~f:(fun l -> String.strip l <> "")
	    |> Seq.filter ~f:(String.is_prefix ~prefix:"s ")
	    |> Seq.map ~f:M_profile_stream.split_maf
	    |> Seq.to_list
	in
	[< write_xmfa alignments
	;  convert_to_xmfa fin
	>]
      end
      | Some l ->
	raise (Failure ("Unknown MAF line: " ^ l))
      | None ->
	[< >]
  in
  convert_to_xmfa (Lazy_io.read_file_lines ~close:true (open_in in_maf))
    
let usage = ""

let parse_argv argv =
  let in_maf = ref "" in
  
  let params =
    Arg.align [ "-in_maf", Arg.Set_string in_maf, "Path Path to input maf file" ]
  in
  Arg.parse params (fun _ -> ()) usage;
  if !in_maf = "" then
    raise (Failure "Must provide -in_maf")
  else
    !in_maf

let main argv =
  let in_maf = parse_argv argv in
  let rec print_lines sin =
    match Seq.next sin with
      | Some l -> begin
	print_endline l;
	print_lines sin
      end
      | None ->
	()
  in
  print_lines (xmfa_of_maf in_maf)
