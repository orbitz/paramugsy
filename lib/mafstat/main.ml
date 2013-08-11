open Core.Std

type t = { missing : P_missing.t
	 ; core    : P_core.t
	 }

let rec read_alns r p =
  match Maf.Reader.read_next r with
    | Some aln ->
      read_alns
	r
	{ missing = P_missing.add aln p.missing
	; core    = P_core.add aln p.core
	}
    | None ->
      p

let create_plugins () =
  { missing = P_missing.create ()
  ; core    = P_core.create ()
  }

let finalize_plugins d p =
  P_missing.finalize d p.missing;
  P_core.finalize d p.core

let main () =
  let p = create_plugins () in
  let r = Maf.Reader.create stdin in
  let p = read_alns r p in
  finalize_plugins "." p

let () = main ()

