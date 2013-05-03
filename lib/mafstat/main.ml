open Core.Std

let rec read_alns r p =
  match Maf.Reader.read_next r with
    | Some aln ->
      read_alns r (P_missing.add aln p)
    | None ->
      p

let main () =
  let r = Maf.Reader.create stdin in
  let p = read_alns r (P_missing.create ()) in
  P_missing.finalize "." p


let () = main ()

