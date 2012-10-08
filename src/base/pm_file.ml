open Core.Std
open Async.Std

let read_file fname =
  Reader.with_file
    fname
    ~f:Reader.contents

let write_lines fname lines =
  let lines_str = String.concat ~sep:"\n" lines in
  Writer.with_file
    fname
    ~f:(fun w -> Deferred.return (Writer.write w lines_str))
