(*pp camlp4o `ocamlfind query -i-format lwt` `ocamlfind query -predicates syntax,preprocessor -a-format -r lwt.syntax` *)
open Core_extended
open Ort
open Ort.Function
open Lwt

type sge_options = { priority : int
		   ; template_file : Fileutils.file_path
		   ; tmp_dir : Fileutils.file_path
		   ; script_dir : Fileutils.file_path
		   ; exec_queue : string
		   ; data_queue : string
		   ; out_dir : Fileutils.file_path
		   }

type copy_files = { file_list : Fileutils.file_path
		  ; src_path : Fileutils.file_path
		  ; dst_path : Fileutils.file_path
		  }

type job_id = string

type rsync_opts = { rsync_opts : string
		  ; rsync_user : string
		  ; rsync_host : string
		  }

exception Qsub_cmd_failure of string

let write_qsub_template template fout =
  let fin = open_in template in
  try
    while true do
      let line = input_line fin in
      output_string fout (line ^ "\n")
    done
  with
    | End_of_file ->
      close_in fin
      

(* Want to have a uniq identifier for qsubs *)
let qsub_count = ref 0
let qsub_commands verbose priority template_file script_dir queue cmds =
  let qsub_dir = Fileutils.join [script_dir; "qsub"] in
  Shell.mkdir_p qsub_dir;
  let qsub_script = Fileutils.join [qsub_dir; Printf.sprintf "q%05d.sh" !qsub_count] in
  qsub_count := !qsub_count + 1;
  let fout = open_out qsub_script in
  write_qsub_template template_file fout;
  output_string fout (Std.String.concat ~sep:"\n" cmds);
  output_char fout '\n';
  close_out fout;
  Std.Unix.chmod qsub_script ~perm:0o555;
  let command = 
    Printf.sprintf 
      "qsub -p %d -o /mnt/scratch -e /mnt/scratch -S /bin/sh -b n -sync n -q %s %s"
      priority
      queue
      qsub_script
  in
  if verbose then begin
    print_endline (Std.String.concat ~sep:"\n" (Std.List.map ~f:(fun s -> "Qsub: " ^ s) cmds));
    print_newline ();
  end
  else
    ();
  lwt qsub_outp = Lwt_process.pread_line (Lwt_process.shell command)
  in
  let job_id =
    match Std.String.split_on_chars ~on:[' '] qsub_outp with
      | "Your"::"job"::job_id::_ ->
	job_id
      | _ -> raise (Failure ("Unknown qsub output: " ^ qsub_outp))
  in
  Lwt.return job_id



let rec wait_on_job_id job_id =
  let check_job_failure () =
    lwt output = Lwt_process.pread ~stderr:`Dev_null (Lwt_process.shell ("qacct -j " ^ job_id)) in
    let tuple_of = function
      | [x1; x2] ->
	(x1, x2)
      | x1::xs -> 
	(x1, Std.String.concat ~sep:" " xs)
      | _ -> raise (Failure "Unknown line")
    in
    let lines = output 
		  |> Std.String.split ~on:'\n'
		  |> Std.List.filter ~f:(fun l -> not (Std.String.is_empty l) && l.[0] <> '=')
		  |> Std.List.map ~f:(fun l -> l 
					 |> Std.String.split ~on:' '
					 |> Std.List.map ~f:(Std.String.strip)
					 |> Std.List.filter ~f:(fun x -> x <> ""))
		  |> Std.List.map ~f:tuple_of
		  
    in
    try
      if List.assoc "exit_status" lines <> "0" then
	raise (Qsub_cmd_failure job_id)
      else
	Lwt.return job_id
    with
      | Not_found ->
	raise (Qsub_cmd_failure job_id)
  in
  lwt t = 
    Lwt_process.exec ~stdin:`Close ~stdout:`Dev_null ~stderr:`Dev_null 
      (Lwt_process.shell ("qstat -j " ^ job_id))
  in
  match t with
    | Unix.WEXITED 1 -> begin
      try_lwt
	check_job_failure ()
      with
        | _ ->
	  Lwt_unix.sleep 30.0 >>= check_job_failure
    end
    | Unix.WEXITED _ ->
      Lwt_unix.sleep 30.0 >>= fun () -> wait_on_job_id job_id
    | _ ->
      raise (Failure "Invalid return signal")
  

let qsub ?(wait = true) ?(verbose = true) ?(priority = 0) ~template_file ~script_dir ~queue cmds =
  lwt job_id = qsub_commands verbose priority template_file script_dir queue cmds in
  if wait then
    wait_on_job_id job_id
  else
    Lwt.return job_id

(*
 * Qsub some commands but perform datasyncing before and after, copying files to and from
 * this uses rsync so rsync_options are required.
 * A datasync queue is used for the transfers. -- Currently this is ignored
 *)
let qsub_with_datasync ?(wait = true) ?(verbose = true) ?(pre = []) ?(post = []) ~options ~in_files ~out_files cmds =
  let rsync_of_in_file copy_file = 
    Printf.sprintf 
      "sync_to.sh %s %d %s %s %s" 
      options.data_queue 
      options.priority 
      copy_file.file_list
      copy_file.src_path
      copy_file.dst_path
  in
  let rsync_of_out_file copy_file = 
    Printf.sprintf 
      "sync_from.sh %s %d %s %s %s" 
      options.data_queue 
      options.priority
      copy_file.file_list
      copy_file.src_path
      copy_file.dst_path
  in
  let in_file_cmds = Std.List.fold_left ~f:(fun a e -> (rsync_of_in_file e)::a) ~init:[] in_files in
  let out_file_cmds = Std.List.fold_left ~f:(fun a e -> (rsync_of_out_file e)::a) ~init:[] out_files in
  let cmds = pre @ in_file_cmds @ cmds @ out_file_cmds @ post in
  qsub 
    ~wait:wait 
    ~verbose:verbose 
    ~priority:options.priority 
    ~template_file:options.template_file 
    ~script_dir:options.script_dir 
    ~queue:options.exec_queue 
    cmds

