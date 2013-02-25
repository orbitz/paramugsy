open Core.Std

let print_missing =
  List.iter
    ~f:(function
      | Maf.Missing_seqs.Fault.Overlap ((s1, e1), (s2, e2)) ->
	Int64.(printf "Overlap (%s, %s) (%s, %s) %s\n"
		 (to_string s1)
		 (to_string e1)
		 (to_string s2)
		 (to_string e2)
		 (to_string (s2 - e1)))
      | Maf.Missing_seqs.Fault.Gap ((s1, e1), (s2, e2)) -> begin
	Int64.(printf "Gap (%s, %s) %s\n"
		 (to_string (succ e1))
		 (to_string (pred s2))
		 (to_string (s2 - e1)))
      end)

let main () =
  let missing = Maf.Missing_seqs.find (Maf.Reader.create stdin) in
  let some_missing =
    List.fold_left
      ~f:(fun some_missing (genome, missing) ->
	match missing with
	  | [] ->
	    some_missing
	  | missing -> begin
	    printf "Genome: %s\n" genome;
	    printf "%s %d faults\n" genome (List.length missing);
	    print_missing missing;
	    print_newline ();
	    true
	  end)
      ~init:false
      missing
  in
  match some_missing with
    | true  -> exit 1
    | false -> exit 0

let () = main ()
