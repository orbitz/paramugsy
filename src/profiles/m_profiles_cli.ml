open Core_extended.Std
open Ort.Function

let usage = ""

let run_mode_argv argv =
  match argv.(0) with
    | "make" ->
      M_make.main argv
    | "translate" ->
      M_translate.main argv
    | "maf_to_xmfa" ->
      M_xmfa.main argv
    | "untranslate" ->
      M_untranslate.main argv
    | "fasta_to_maf" ->
      M_maf.main argv
    | _ -> begin
      print_endline usage;
      exit 1
    end

let main () =
  run_mode_argv
    (Sys.argv |> Array.to_list |> List.tl_exn |> Array.of_list)


let () = main ()
