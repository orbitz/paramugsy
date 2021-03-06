open Core.Std

type t = { in_chan     : In_channel.t
	 ; buf         : string
	 ; mutable len : int
	 }

let buf_size = 4096

let create in_chan = { in_chan; buf = String.create buf_size; len = 0 }

let rec read_max in_chan ~buf ~pos ~len =
  match In_channel.input in_chan ~buf ~pos ~len with
    | 0 -> 0
    | n when n = len -> n
    | n -> n + read_max in_chan ~buf ~pos:(pos + n) ~len:(len - n)

let find_char s ~pos ~len ~c =
  match String.index_from s pos c with
    | Some idx when idx < (pos + len) -> Some idx
    | _                               -> None

let find_begin_header s ~pos ~len =
  find_char s ~pos ~len ~c:'>'

let find_endline s ~pos ~len =
  find_char s ~pos ~len ~c:'\n'

let move_buf_left t ~pos =
  String.blit
    ~src:t.buf
    ~src_pos:pos
    ~dst:t.buf
    ~dst_pos:0
    ~len:(t.len - pos);
  t.len <- t.len - pos

let read_buf_max t =
  if t.len < buf_size then begin
    match
      read_max
	t.in_chan
	~buf:t.buf
	~pos:t.len
	~len:(buf_size - t.len)
    with
      | 0 -> 0
      | n -> begin t.len <- t.len + n; n end
  end
  else
    0

let rec next_header t =
  match find_begin_header t.buf ~pos:0 ~len:t.len with
    | None -> begin
      (*
       * If there is no sign of a a header, throw everything
       * away and read the next chunk in
       *)
      t.len <- 0;
      match read_buf_max t with
	| 0 -> None
	| _ -> next_header t
    end
    | Some hstart -> begin
      match find_endline t.buf ~pos:hstart ~len:(t.len - hstart) with
	| Some hend -> begin
	  let header = String.sub t.buf ~pos:(hstart + 1) ~len:(hend - hstart - 1) in
	  move_buf_left t ~pos:(hend + 1);
	  Some header
	end
	| None -> begin
	  move_buf_left t ~pos:hstart;
	  match read_buf_max t with
	    | 0 -> begin
	      let header = String.sub t.buf ~pos:(hstart + 1) ~len:(t.len - hstart - 1) in
	      t.len <- 0;
	      Some header
	    end
	    | _ ->
	      next_header t
	end
    end

let strip_bad_chars buf ~pos ~len =
  let pos_end = pos + len in
  let rec sbc' i dropped =
    if i < pos_end then begin
      if Char.is_whitespace buf.[i] then
	sbc' (i + 1) (dropped + 1)
      else if dropped > 0 then begin
	buf.[i - dropped] <- buf.[i];
	sbc' (i + 1) dropped
      end
      else
	sbc' (i + 1) dropped
    end
    else
      len - dropped
  in
  sbc' pos 0

let rec next_seq t ~buf ~pos ~len =
  let min_len = min len t.len in
  match find_begin_header t.buf ~pos:0 ~len:min_len with
    | Some idx -> begin
      String.blit
	~src:t.buf
	~src_pos:0
	~dst:buf
	~dst_pos:pos
	~len:idx;
      move_buf_left t ~pos:idx;
      strip_bad_chars buf ~pos ~len:idx
    end
    | None when t.len > 0 && len > 0 -> begin
      String.blit
	~src:t.buf
	~src_pos:0
	~dst:buf
	~dst_pos:pos
	~len:min_len;
      move_buf_left t ~pos:min_len;
      let stripped_len = strip_bad_chars buf ~pos ~len:min_len in
      stripped_len + next_seq t ~buf ~pos:(pos + stripped_len) ~len:(len - stripped_len)
    end
    | None when len > 0 -> begin
      match read_buf_max t with
	| 0 -> 0
	| _ -> next_seq t ~buf ~pos ~len
    end
    | None ->
      0
