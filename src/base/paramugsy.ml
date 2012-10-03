open Core_extended
open Core.Std
open Ort
open Ort.Function

type file_path = string

module Local_processor =
  Job_processor.Make(Queue_server.Make(Local_interface))

let usage = ""

let run_mode_argv argv =
  match argv.(0) with
    | "sge" ->
      raise (Failure "Not implemented yet")
    | "local" ->
      raise (Failure "Not implemented yet")
    | _ -> begin
      print_endline usage;
      exit 1
    end


let main () =
  run_mode_argv
    (Sys.argv |> Array.to_list |> List.tl_exn |> Array.of_list)


let () = main ()

