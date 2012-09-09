open Core_extended.Std
open Async.Std
open Ort

module Shell = Core_extended.Shell

type run_success = Pm_sge_server.run_success
type run_error   = Pm_sge_server.run_error

type copy_file = { file_list : Fileutils.file_path
		 ; src_path  : Fileutils.file_path
		 ; dst_path  : Fileutils.file_path
		 }

type command = string

type t  = { name          : Pm_sge_server.name
	  ; verbose       : bool
	  ; template_file : Fileutils.file_path
          ; script_dir    : Fileutils.file_path
          ; exec_queue    : Pm_sge_server.queue
          ; data_queue    : Pm_sge_server.queue
	  ; pre           : command list
	  ; post          : command list
	  ; body          : command list
	  ; in_files      : copy_file list
	  ; out_files     : copy_file list
          }


module Template = struct
  type template_vars = { name : Pm_sge_server.name
		       ; pre  : string
		       ; post : string
		       ; body : string
		       }
end

let replace_template_vars tv s =
  let module T = Template
  in
  let replace =
    [ (Str.regexp "%(NAME)", tv.T.name)
    ; (Str.regexp "%(PRE)",  tv.T.pre)
    ; (Str.regexp "%(BODY)", tv.T.body)
    ; (Str.regexp "%(POST)", tv.T.post)
    ]
  in
  List.fold_left
    ~f:(fun s (re, v) -> Str.global_replace re v s)
    ~init:s
    replace

let instantiate_template qsub_run =
  let j = String.concat ~sep:"\n"
  in
  let module T = Template
  in
  let template_vars = { T.name = qsub_run.name
		      ; T.pre  = j qsub_run.pre
		      ; T.post = j qsub_run.post
		      ; T.body = j qsub_run.body
		      }
  in
  replace_template_vars
    template_vars
    (Pm_file.read_file qsub_run.template_file)

let sync_cmd script queue copy_file =
  Printf.sprintf
    "%s %s %s %s %s"
    script
    queue
    copy_file.file_list
    copy_file.src_path
    copy_file.dst_path


(* We want a little counter for these qsubs *)
let submit_count = ref 0

let submit qsub_run sge =
  let copy_to   = List.map ~f:(sync_cmd "sync_to.sh" qsub_run.data_queue) qsub_run.in_files
  and copy_from = List.map ~f:(sync_cmd "sync_from.sh" qsub_run.data_queue) qsub_run.out_files
  in
  let qsub_run =
    { qsub_run with
      pre  = qsub_run.pre @ copy_to;
      post = copy_from @ qsub_run.post
    }
  in
  let qsub_cmds = instantiate_template qsub_run
  in
  let qsub_dir = Fileutils.join [qsub_run.script_dir; "qsub"]
  in
  let qsub_script = Fileutils.join [qsub_dir; Printf.sprintf "q%05d.sh" !submit_count]
  in
  submit_count := !submit_count + 1;
  Writer.with_file
    qsub_script
    ~f:(fun w -> Deferred.return (Writer.write w qsub_cmds)) >>= fun () ->
  let _ = Unix.chmod ~perm:0o555 qsub_script
  in
  Pm_sge_server.run qsub_run.name qsub_run.exec_queue qsub_script sge

