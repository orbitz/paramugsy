open Core.Std

let rec clean r w =
  match Maf.Reader.read_next r with
    | Some aln -> begin
      Maf.Writer.write w aln;
      clean r w
    end
    | None ->
      ()

let () =
  clean
    (Maf.Reader.create stdin)
    (Maf.Writer.create stdout)
