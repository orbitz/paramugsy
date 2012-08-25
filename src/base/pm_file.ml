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
