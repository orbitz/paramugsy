open Core.Std

type t = In_channel.t

let create t = t

let rec read_anchor t =
  match In_channel.input_line t with
    | Some l when String.is_prefix ~prefix:"a score=" (String.strip l) -> Some ()
    | Some _ -> read_anchor t
    | None -> None

let seq_of_string l =
  let s =
    List.filter
      ~f:(fun s -> s <> "")
      (String.split_on_chars
	 ~on:[' '; '\t']
	 l)
  in
  let d_of_string = function
    | "+" -> Sequence.Direction.Forward
    | "-" -> Sequence.Direction.Reverse
    | _   -> failwith "Bad direction"
  in
  Option.try_with
    (fun () ->
      match s with
	| [name; start; size; d; total] when d = "+" || d = "-" ->
	  Sequence.make
	    ~name
	    ~start:(Int64.of_string start)
	    ~size:(Int64.of_string size)
	    ~d:(d_of_string d)
	    ~total:(Int64.of_string total)
	| _ ->
	  failwith "Invalid line")

let read_alignment t =
  let open Option.Monad_infix in
  let rec read_aln acc =
    match In_channel.input_line t with
      | Some l when String.strip l = "" ->
	Some (List.rev acc)
      | Some l when l.[0] <> '#' ->
	seq_of_string l >>= fun seq ->
	read_aln (seq::acc)
      | Some _ -> None
      | None when acc = [] -> None
      | None -> Some (List.rev acc)
  in
  read_aln []


let read_next t =
  let open Option.Monad_infix in
  read_anchor t    >>= fun () ->
  read_alignment t >>= fun aln ->
  Some (Alignment.make aln)
