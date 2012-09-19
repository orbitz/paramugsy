(*pp camlp4o *)
open Core.Std
open Ort
open Ort.Function

let species_name fname = 
  let species_name = 
    match String.lsplit2 ~on:'.' (Fileutils.basename fname) with
      | Some (species, _) ->
	species
      | None ->
	Fileutils.basename fname
  in
  let species_name_clean =
    species_name 
    |> String.tr ~target:'-' ~replacement:'_' 
  in
  species_name_clean

let rec rewrite_headers_stream species sin =
  match Seq.next sin with
    | Some l when not (String.is_empty l) && l.[0] = '>' && String.is_prefix ~prefix:">gi|" l -> begin
      match String.split_on_chars ~on:['|'] l with
	| [">gi"; _; _; name; _] -> begin
	  match String.lsplit2 ~on:'.' name with
	    | Some (n, _) ->
	      let header_new = Printf.sprintf ">%s.%s" species n in
	      [< 'header_new
	      ;  rewrite_headers_stream species sin
	      >]
	    | None ->
	      let header_new = Printf.sprintf ">%s.%s" species name in
	      [< 'header_new
	      ;  rewrite_headers_stream species sin
	      >]
	end
	| _ ->
	  raise (Failure ("Unknown line: " ^ l))
    end
    | Some l when not (String.is_empty l) && l.[0] = '>' && String.contains l ':' -> begin
      match String.split_on_chars ~on:[':'] (String.drop_prefix l 1) with
	| species_name::header::_ ->
	  let header_new = Printf.sprintf ">%s.%s" species_name header in
	  [< 'header_new
	  ;  rewrite_headers_stream species sin
	  >]
	| _ ->
	  raise (Failure ("Unknown line: " ^ l))
    end
    | Some l when not (String.is_empty l) && l.[0] = '>' -> begin
      let cleaned_name = 
	(String.drop_prefix l 1) |>
	    String.tr ~target:'-' ~replacement:'_' |>
		String.tr ~target:'.' ~replacement:'_' |>
		    String.tr ~target:' ' ~replacement:'_'
      in
      let header_new = Printf.sprintf ">%s.%s" species cleaned_name in
      [< 'header_new
      ;  rewrite_headers_stream species sin
      >]
    end
    | Some l ->
      [< 'l
      ;  rewrite_headers_stream species sin
      >]
    | None ->
      [< >]


let rewrite_headers fname =
  let species_name = species_name fname in
  let sin = Lazy_io.read_file_lines ~close:true (open_in fname) in
  rewrite_headers_stream species_name sin
