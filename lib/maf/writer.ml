open Core.Std

type t = Out_channel.t

let create t = t

let write aln t =
  let score =
    Printf.sprintf
      "a %s\n"
      (Alignment.score aln)
  in
  let seqs =
    List.map
      ~f:(fun s ->
	String.concat
	  ~sep:"\t"
	  [ "s"
	  ; Sequence.name s
	  ; Int64.to_string (Sequence.start s)
	  ; Int64.to_string (Sequence.size s)
	  ; Sequence.Direction.to_string (Sequence.direction s)
	  ; Int64.to_string (Sequence.total s)
	  ])
      (Alignment.sequences aln)
  in
  Out_channel.output_string t score;
  Out_channel.output_lines t seqs;
  Out_channel.output_char t '\n';

