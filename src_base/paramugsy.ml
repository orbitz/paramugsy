open Core_extended
open Core_extended.Std
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

type ergatis_mode = unit

type parallel_mode =
  | Sge of sge_mode
  | Ergatis of ergatis_mode


let lsplit_once ~on s =
  match String.lsplit2 ~on:on s with
    | Some (v, _) -> v
    | None -> s

let usage = ""

let run_mode_argv argv = 
  match argv.(0) with
    | "sge" ->
      Pm_sge.run_sge argv
    | "ergatis" ->
      Pm_ergatis.run_ergatis argv
    | _ -> begin
      print_endline usage;
      exit 1
    end


let main () = 
  run_mode_argv 
    (Sys.argv |> Array.to_list |> List.tl_exn |> Array.of_list)


let () = main ()

