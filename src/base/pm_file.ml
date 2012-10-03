open Core.Std
open Async.Std

let read_file fname =
  let b = Buffer.create 1024
  and fin = open_in fname
  in
  try
    while true; do
      Buffer.add_string b (input_line fin)
    done; ""
  with End_of_file ->
    close_in fin;
    Buffer.contents b


let write_lines fname lines =
  Writer.with_file
    fname
    ~f:(fun w -> Deferred.return (List.iter ~f:(Writer.write w) lines))
