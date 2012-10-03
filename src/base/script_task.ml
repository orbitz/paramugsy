open Core.Std

open Ort

module Command = struct
  type t = string
end

module Copy_file = struct
  type t = { file_list : Fileutils.file_path
	   ; src_path  : Fileutils.file_path
	   ; dst_path  : Fileutils.file_path
	   }
end

module Template = struct
  type t = { name : Queue_job.Name.t
	   ; pre  : string
	   ; post : string
	   ; body : string
	   }
end

type t = { name      : Queue_job.Name.t
	 ; pre       : Command.t list
	 ; post      : Command.t list
	 ; body      : Command.t list
	 ; in_files  : Copy_file.t list
	 ; out_files : Copy_file.t list
	 }

let replace_template_vars tv s =
  let module T = Template
  in
  let replace =
    [ (Str.regexp "%(NAME)", Queue_job.Name.to_string tv.T.name)
    ; (Str.regexp "%(PRE)",  tv.T.pre)
    ; (Str.regexp "%(BODY)", tv.T.body)
    ; (Str.regexp "%(POST)", tv.T.post)
    ]
  in
  List.fold_left
    ~f:(fun s (re, v) -> Str.global_replace re v s)
    ~init:s
    replace

let instantiate_template template t =
  let j = String.concat ~sep:"\n"
  in
  let module T = Template
  in
  let template_vars = { T.name =   t.name
		      ; T.pre  = j t.pre
		      ; T.post = j t.post
		      ; T.body = j t.body
		      }
  in
  replace_template_vars
    template_vars
    template

let sync_cmd script queue copy_file =
  Printf.sprintf
    "%s %s %s %s %s"
    script
    queue
    copy_file.Copy_file.file_list
    copy_file.Copy_file.src_path
    copy_file.Copy_file.dst_path

let to_string ?(data_queue = None) template t =
  let t =
    match data_queue with
      | Some data_queue ->
	let data_queue' = Queue_job.Queue.to_string data_queue in
	let copy_to =
	  List.map
	    ~f:(sync_cmd "sync_to.sh" data_queue')
	    t.in_files
	and copy_from =
	  List.map
	    ~f:(sync_cmd "sync_from.sh" data_queue')
	    t.out_files
	in
	{ t with
	  pre  = t.pre @ copy_to;
	  post = copy_from @ t.post
	}
      | None ->
	t
  in
  instantiate_template template t
