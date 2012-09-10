open Core_extended.Std
open Ort
open Ort.Function

module Shell = Core_extended.Shell

type sge_mode = { out_dir : Fileutils.file_path
		; sequences : Fileutils.file_path list
		; exec_q : string
		; stage_data : bool
		; staging_q : string
		; tmp_dir : Fileutils.file_path
		; sequence_dir : Fileutils.file_path
		; distance : int
		; minlength : int
		; template_file : Fileutils.file_path
		; seqs_per_mugsy : int
		; nucmer_chunk_size : int
		; out_maf : Fileutils.file_path
		}

let usage = ""

let parse_argv argv =
  let seq_list = ref "" in
  let out_dir = ref "" in
  let exec_q = ref "exec.q" in
  let stage_data = ref false in
  let staging_q = ref "staging.q" in
  let tmp_dir = ref "/tmp" in
  let distance = ref 1000 in
  let minlength = ref 30 in
  let template_file = ref "/usr/local/etc/pm_qsub_template.sh" in
  let seqs_per_mugsy = ref 2 in
  let nucmer_chunk_size = ref 1 in
  let out_maf = ref "" in

  let params =
    Arg.align [ ( "-seq_list"
		, Arg.Set_string seq_list
		, "Path A file containing a list of paths to sequences.")

	      ; ( "-out_dir"
		, Arg.Set_string out_dir
		, "Path Location to output files.")

	      ; ( "-exec_q"
		, Arg.Set_string exec_q
		, "String Queue name to run exec jobs on, defaults to exec.q.")

	      ; ( "-stage_data"
		, Arg.Set stage_data
		, " Handle staging data out to nodes, off by default.")

	      ; ( "-staging_q"
		, Arg.Set_string staging_q
		, "String Queue to run data staging jobs in, defaults to staging.q (if -stage_data).")

	      ; ( "-tmp_dir"
		, Arg.Set_string tmp_dir
		, "String Directory to put temporary files in, defaults to /tmp")

	      ; ( "-distance"
		, Arg.Set_int distance
		, "Int Distance option for Mugsy (default 1000)")

	      ; ( "-minlength"
		, Arg.Set_int minlength
		, "Int Minlength for Mugsy (default 30)")

	      ; ( "-template_file"
		, Arg.Set_string template_file
		, "Path File to use for qsub templates")

	      ; ( "-seqs_per_mugsy"
		, Arg.Set_int seqs_per_mugsy
		, "Int Maximum number of sequences to have per Mugsy run (default 2)")

	      ; ( "-nucmer_chunk_size"
		, Arg.Set_int nucmer_chunk_size
		, "Int Maximum number of sequences to have per Nucmer run (default 1)")

	      ; ( "-out_maf"
		, Arg.Set_string out_maf
		, "Path File to output the final MAF")
	      ]
  in
  Arg.parse params (fun _ -> ()) usage;
  if !seq_list = "" then
    raise (Failure "Must set -seq_list")
  else if !out_dir = "" then
    raise (Failure "Must set -out_dir")
  else if !out_maf = "" then
    raise (Failure "Must set -out_maf")
  else if !seqs_per_mugsy < 2 then
    raise (Failure "-seqs_per_mugsy must be >= 2")
  else
    { out_dir = !out_dir
    ; sequences = Lazy_io.read_file_lines ~close:true (open_in !seq_list) |> Seq.to_list
    ; exec_q = !exec_q
    ; stage_data = !stage_data
    ; staging_q = !staging_q
    ; tmp_dir = !tmp_dir
    ; sequence_dir = Fileutils.join [!tmp_dir; "sequences"]
    ; distance = !distance
    ; minlength = !minlength
    ; template_file = !template_file
    ; seqs_per_mugsy = !seqs_per_mugsy
    ; nucmer_chunk_size = !nucmer_chunk_size
    ; out_maf = !out_maf
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
    let fout_name =  Fileutils.join [tmp_dir; species_name] in
    let fout = open_out fout_name in
    print_lines fout (M_rewrite_fasta.rewrite_headers seq);
    close_out fout;
    fout_name
  in
  Shell.mkdir ~p:() tmp_dir;
  List.map ~f:rewrite_sequence sequences


let run argv =
  let options = parse_argv argv
  in
  Shell.mkdir ~p:() options.out_dir;
  Shell.mkdir ~p:() options.tmp_dir;
  Shell.mkdir ~p:() options.sequence_dir;
  let sequences = rewrite_sequences options.sequences options.sequence_dir
  in
  let job = Pm_job.make_job options.seqs_per_mugsy sequences
  in
  Pm_job.pp stdout job
