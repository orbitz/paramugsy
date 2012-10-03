open Core.Std
open Async.Std

let read_file fname =
  Reader.with_file
    fname
    ~f:Reader.contents

let write_lines fname lines =
  Writer.with_file
    fname
    ~f:(fun w -> Deferred.return (List.iter ~f:(Writer.write w) lines))
