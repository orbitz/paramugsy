open Core_extended
open Core.Std
open Ort
open Ort.Function

type file_path = string

type sge_mode = { sge_out_dir : file_path
		; sge_sequences : file_path list
		; exec_q : string
		; staging_q : string
		; rsync_options : string
		; data_host : string
		}

let usage = ""

let run_mode_argv argv =
  match argv.(0) with
    | "sge" ->
      Pm_sge_main.run argv
    | "ergatis" ->
      raise (Failure "Not implemented yet")
    | _ -> begin
      print_endline usage;
      exit 1
    end


let main () =
  run_mode_argv
    (Sys.argv |> Array.to_list |> List.tl_exn |> Array.of_list)


let () = main ()

