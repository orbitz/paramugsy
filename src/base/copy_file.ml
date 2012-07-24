open Lwt

let copy_file in_file out_file = 
  Lwt_process.exec 
    ~stdin:`Close 
    ~stdout:`Dev_null 
    (Lwt_process.shell 
       (Printf.sprintf "cp %s %s" in_file out_file)) >>= fun _ -> Lwt.return out_file

let mkdir_p dir =
  Lwt_process.exec
    ~stdin:`Close
    ~stdout:`Close
    (Lwt_process.shell
       (Printf.sprintf "mkdir -p %s" dir)) >>= fun _ -> Lwt.return dir
