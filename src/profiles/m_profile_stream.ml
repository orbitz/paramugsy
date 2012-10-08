(*pp camlp4o *)
open Core_extended.Std
open Ort

type t = M_profile.t Stream.t

let string_of_direction = function
  | `Forward -> "+"
  | `Reverse -> "-"

let direction_of_string = function
  | "+" -> `Forward
  | "-" -> `Reverse
  | s -> raise (Failure ("Invalid direction: " ^ s))

let split_maf s =
  match List.filter ~f:((<>) "") (String.split_on_chars ~on:[' '; '\t'] s) with
    | ["s"; name; start; size; d; src_size; text] ->
      (name, int_of_string start, int_of_string size, direction_of_string d, int_of_string src_size, text)
    | _ ->
      raise (Failure ("Unknown maf line: " ^ s))

let rec drop_until_score sin =
  match Seq.next sin with
    | Some s ->
      if String.is_prefix ~prefix:"a score=" s then
	  (* Modify this to actually parse score, could be useful in future *)
	Some s
      else
	drop_until_score sin
    | None ->
      None


let rec stream_profiles idx profile_basename sin =
  match Seq.next sin with
    | Some s when String.length s = 0 ->
      [< >]
    | Some s when String.is_prefix ~prefix:"s " s ->
      let profile_name = (profile_basename, string_of_int idx) in
      let (seq_name, start, size, d, src_size, text) = split_maf s in
      let range = M_range.of_maf ~start:start ~size:size ~src_size:src_size ~direction:d in
      [< 'M_profile.profile_of_maf_entry
	  ~name:profile_name
	  ~seq_name:seq_name
	  ~range:range
	  ~src_size:src_size
	  ~text:text
      ;  stream_profiles (idx + 1) profile_basename sin
      >]
    | Some s when s.[0] = '#' ->
      stream_profiles idx profile_basename sin
    | Some s ->
      raise (Failure "Unknown line")
    | None when idx = 0 ->
      raise (Failure "Expected alignment, did not get")
    | None ->
      [< >]

let rec profile_stream_of_maf_stream ?(start_count = 0) ~basename sin =
  match drop_until_score sin with
    | None ->
      [< >]
    | Some _ ->
      let profile_basename = Printf.sprintf "%s.%s_%04d" basename basename start_count in
      [< stream_profiles 0 profile_basename sin
      ;  profile_stream_of_maf_stream ~start_count:(start_count + 1) ~basename:basename sin
      >]


let profile_stream_of_maf ~basename fname =
  profile_stream_of_maf_stream 
    ~basename:basename 
    (Lazy_io.read_file_lines ~close:true (open_in fname))
