open Core.Std

module Fault = Maf.Missing_seqs.Fault

let rec no_overlaps = function
  | [] ->
    true
  | (Fault.Overlap _)::_ ->
    false
  | _::xs ->
    no_overlaps xs

let rec enforce_no_overlaps = function
  | [] ->
    Ok ()
  | (_, missing)::xs when no_overlaps missing ->
    enforce_no_overlaps xs
  | _ ->
    Error `Overlap

let write_maf missing fastas = Ok ()

let run () =
  let open Result.Monad_infix in
  let missing = Maf.Missing_seqs.find (Maf.Reader.create stdin) in
  let fastas  = List.tl_exn (Array.to_list Sys.argv) in
  enforce_no_overlaps missing >>= fun () ->
  write_maf missing fastas

let main () =
  match run () with
    | Ok () ->
      ()
    | Error _ ->
      exit 1

let () = main ()
