open Core.Std

let log =
  let module A = Async.Std in
  let w = A.Writer.create (A.Fd.stdout ())
  in
  fun s ->
    let time = Time.format (Time.now ()) "%Y-%m-%d %H:%M:%S" in
    A.Writer.write w (time ^ " - " ^ s ^ "\n")
