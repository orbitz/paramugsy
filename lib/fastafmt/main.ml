open Core.Std

let rec read_sequence r buf =
  match Fasta.Reader.next_seq r ~buf ~pos:0 ~len:(String.length buf) with
    | 0 ->
      print_newline ()
    | n -> begin
      Out_channel.output Out_channel.stdout ~buf ~pos:0 ~len:n;
      Out_channel.newline Out_channel.stdout;
      read_sequence r buf
    end

let rec read_fasta r buf =
  match Fasta.Reader.next_header r with
    | Some h -> begin
      printf ">%s\n" h;
      read_sequence r buf;
      read_fasta r buf
    end
    | None ->
      ()

let format_fasta r width =
  let buf = String.create width in
  read_fasta r buf

let main () =
  let width = Int.of_string Sys.argv.(1) in
  format_fasta (Fasta.Reader.create In_channel.stdin) width

let () = main ()
