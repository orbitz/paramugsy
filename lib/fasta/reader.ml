open Core.Std

type buffer =
  | Header   of string
  | Seq      of string
  | Seq_done of string

type t = { mutable buf  : buffer
	 ;         chan : In_channel.t
	 }

let rec find_next_header chan =
  match In_channel.input_line chan with
    | None ->
      None
    | Some s when s.[0] = '>' ->
      Some s
    | Some _ ->
      find_first_header chan

let create chan =
  match find_next_header chan with
    | None ->
      Error `Bad_format
    | Some header ->
      Ok { buf  = ref (Header header)
	 ; chan = chan
	 }

let read_next_header = function
  | { buf = Header h } ->
    Some h
  | { buf = Seq_done h } as t -> begin
    t.buf <- Header h;
    Some h
  end
  | { buf = Seq _ } as t -> begin
    match find_next_header t.chan with
      | None ->
	None
      | Some header -> begin
	t.buf <- Header header;
	Some header
      end
  end


let read_next_seq_line t s =
  match In_channel.input_line t.chan with
    | None ->
      t.buf <- Seq_fin s
    | Some str when str.[0] = '<' ->
      t.buf <- Seq-fun
    | Some str ->
      t.buf <- Seq (s ^ str);

(*
 * This interface doesn't make much sense given the
 * implementation but it is this way so the
 * implementation can change to be more peformant
 * without changing the code of the callers
 *)
val rec read_next_seq t ~buf ~pos ~len =
  match t with
    | { buf = Header _ } as t -> begin
      match In_channel.input_line t.chan with
	| None ->
	  0
	| Some s ->
	  read_next_seq
	    { t with buf = Seq s }
	    ~buf
	    ~pos
	    ~len
    end
    | { buf = Seq s } as t when len <= String.length s -> begin
      String.blit
	~src:s
	~src_pos:0
	~dst:buf
	~dst_pos:pos
	~len:len;
      t.buf <- Seq (String.drop_prefix s len);
      len
    end
    | { buf = Seq s } as t -> begin
      match
    end
