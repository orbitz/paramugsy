open Core.Std

open Ort

module Command = Core_extended.Command

module Local_processor =
  Job_processor.Make(Queue_server.Make(Local_interface))

module Sge_processor =
  Job_processor.Make(Queue_server.Make(Sge_interface))

(* For both local and sge *)
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

let minlength = ref 30
let minlength_f =
  Command.Flag.set_int "-minlength" minlength
    ~doc:" Minlength to give Mugsy"

let distance = ref 100
let distance_f =
  Command.Flag.set_int "-distance" distance
    ~doc:" Distance to give Mugsy"

let run_size = ref 1
let local_run_size_f =
  Command.Flag.set_int "-cores" run_size
    ~doc:" Number of cores on the machine"

let sge_run_size_f =
  Command.Flag.set_int "-run-size" run_size
    ~doc:" Number of jobs to queue"

(* SGE only *)
let exec_q = ref ""
let exec_q_f =
  Command.Flag.set_string "-exec-q" exec_q
    ~doc:" Exec queue to use"

let data_q = ref ""
let data_q_f =
  Command.Flag.set_string "-data-q" data_q
    ~doc:" Data queue to use (optional)"

let local_cmd_flags =
  [ seq_list_f
  ; out_maf_f
  ; tmp_dir_f
  ; seqs_per_mugsy_f
  ; nucmer_chunk_size_f
  ; template_file_f
  ; minlength_f
  ; distance_f
  ; local_run_size_f
  ]

let sge_cmd_flags =
  [ seq_list_f
  ; out_maf_f
  ; tmp_dir_f
  ; seqs_per_mugsy_f
  ; nucmer_chunk_size_f
  ; template_file_f
  ; minlength_f
  ; distance_f
  ; exec_q_f
  ; data_q_f
  ; sge_run_size_f
  ]

let local_flags () =
  if !seq_list = "" then
    failwith "-seq-list must be set";

  if !run_size < 1 then
    failwith "-cores must be greater than 0";

  if !tmp_dir = "" then
    failwith "-tmp-dir must be set";

  if !distance < 1 then
    failwith "-distance must be greater than 0";

  if !minlength < 1 then
    failwith "-minlength must be greater than 0";

  if !template_file = "" then
    failwith "-template-file must be set";

  if !seqs_per_mugsy < 1 then
    failwith "-seqs-per-mugsy must be greater than 0";

  if !nucmer_chunk_size < 1 then
    failwith "-nucmer-chunk-size must be greater than 0";

  if !out_maf = "" then
    failwith "-out-maf must be set";

  let seqs =
    Seq.to_list (Lazy_io.read_file_lines ~close:true (open_in !seq_list))
  in
  let module J = Job_processor in
  { J.seq_list       = seqs
  ;   run_size       = !run_size
  ;   exec_q         = Queue_job.Queue.of_string ""
  ;   data_q         = None
  ;   tmp_dir        = !tmp_dir
  ;   distance       = !distance
  ;   minlength      = !minlength
  ;   template_file  = !template_file
  ;   seqs_per_mugsy = !seqs_per_mugsy
  ;   nucmer_chunk   = !nucmer_chunk_size
  ;   out_maf        = !out_maf
  ;   log            = Logger.log
  }


let sge_flags () =
  if !seq_list = "" then
    failwith "-seq-list must be set";

  if !run_size < 1 then
    failwith "-cores must be greater than 0";

  if !exec_q = "" then
    failwith "-exec-q must be set";

  if !tmp_dir = "" then
    failwith "-tmp-dir must be set";

  if !distance < 1 then
    failwith "-distance must be greater than 0";

  if !minlength < 1 then
    failwith "-minlength must be greater than 0";

  if !template_file = "" then
    failwith "-template-file must be set";

  if !seqs_per_mugsy < 1 then
    failwith "-seqs-per-mugsy must be greater than 0";

  if !nucmer_chunk_size < 1 then
    failwith "-nucmer-chunk-size must be greater than 0";

  if !out_maf = "" then
    failwith "-out-maf must be set";

  let seqs =
    Seq.to_list (Lazy_io.read_file_lines ~close:true (open_in !seq_list))
  in
  let module J = Job_processor in
  { J.seq_list       = seqs
  ;   run_size       = !run_size
  ;   exec_q         = Queue_job.Queue.of_string !exec_q
  ;   data_q         = if !data_q = "" then None else Some (Queue_job.Queue.of_string !data_q)
  ;   tmp_dir        = !tmp_dir
  ;   distance       = !distance
  ;   minlength      = !minlength
  ;   template_file  = !template_file
  ;   seqs_per_mugsy = !seqs_per_mugsy
  ;   nucmer_chunk   = !nucmer_chunk_size
  ;   out_maf        = !out_maf
  ;   log            = Logger.log
  }

let rec print_lines fout sin =
  match Seq.next sin with
    | Some l -> begin
      output_string fout l;
      output_char fout '\n';
      print_lines fout sin
    end
    | None ->
      ()

let rewrite_sequences sequences tmp_dir =
  let rewrite_sequence seq =
    let species_name = M_rewrite_fasta.species_name seq in
    let fout_name    = Fileutils.join [tmp_dir; species_name] in
    let fout         = open_out fout_name in
    print_lines fout (M_rewrite_fasta.rewrite_headers seq);
    close_out fout;
    fout_name
  in
  Core_extended.Shell.mkdir ~p:() tmp_dir;
  List.map ~f:rewrite_sequence sequences

let run runner flags =
  Core_extended.Shell.mkdir ~p:() flags.Job_processor.tmp_dir;
  let flags =
    {flags with Job_processor.seq_list =
	rewrite_sequences
	  flags.Job_processor.seq_list
	  flags.Job_processor.tmp_dir
    }
  in
  let open Async.Std in
  ignore (after (sec 0.) >>= fun () ->
	  runner flags);
  never_returns (Scheduler.go ())

let local_cmd = Command.create_no_accum
  ~summary:"Run locally"
  ~usage_arg:""
  ~flags:local_cmd_flags
  ~final:(function
    | [] -> ()
    | _  -> failwith "What you doin dawg?")
  (fun () -> run Local_processor.run (local_flags ()))

let sge_cmd = Command.create_no_accum
  ~summary:"Run on SGE"
  ~usage_arg:""
  ~flags:sge_cmd_flags
  ~final:(function
    | [] -> ()
    | _  -> failwith "What you doin dawg?")
  (fun () -> run Sge_processor.run (sge_flags ()))

let main () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run ~version:"0.1" ~build_info:"N/A"
      (Command.group ~summary:"Run alignments"
	 [ ("local", local_cmd)
	 ; ("sge"  , sge_cmd)
	 ]))

let () = main ()

