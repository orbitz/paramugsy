open Core.Std
open Async.Std

module Command = Core_extended.Command

module Local_processor =
  Job_processor.Make(Queue_server.Make(Local_interface))

module Sge_processor =
  Job_processor.Make(Queue_server.Make(Sge_interface))

let seq_list = ref ""
let seq_list_f =
  Command.Flag.set_string "-seq-list" seq_list
    ~doc:" File containing a list of sequences"

let out_maf = ref ""
let out_maf_f =
  Command.Flag.set_string "-out-maf" out_maf
    ~doc:" Output MAF location"

let tmp_dir = ref ""
let tmp_dir_f =
  Command.Flag.set_string "-tmp-dir" tmp_dir
    ~doc:" Directory to do temporary work in"

let seqs_per_mugsy = ref 10
let seqs_per_mugsy_f =
  Command.Flag.set_int "-seqs-per-mugsy" seqs_per_mugsy
    ~doc:" Number of sequences to use per Mugsy run"

let nucmer_chunk_size = ref 10
let nucmer_chunk_size_f =
  Command.Flag.set_int "-nucmer-chunk-size" nucmer_chunk_size
    ~doc:" Number of nucmers to do per job"

let template_file = ref ""
let template_file_f =
  Command.Flag.set_string "-template-file" template_file
    ~doc:" Path to template file"

let local_cmd_flags =
  [ seq_list_f
  ; out_maf_f
  ; tmp_dir_f
  ; seqs_per_mugsy_f
  ; nucmer_chunk_size_f
  ; template_file_f
  ]

let local_cmd = Command.create_no_accum
  ~summary:"Run locally"
  ~usage_arg:""
  ~flags:local_cmd_flags
  ~final:(function
    | [] ->
      failwith "Not implemented yet"
    | _ -> failwith "What you doin dawg?")
  (fun args -> ())

let sge_cmd = Command.create_no_accum
  ~summary:"Run on SGE"
  ~usage_arg:""
  ~flags:local_cmd_flags
  ~final:(function
    | [] ->
      failwith "Not implemented yet"
    | _ -> failwith "What you doin dawg?")
  (fun args -> ())

let main () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run ~version:"0.1" ~build_info:"N/A"
      (Command.group ~summary:"Run alignments"
	 [ ("local", local_cmd)
	 ; ("sge"  , sge_cmd)
	 ]))

let () = main ()

