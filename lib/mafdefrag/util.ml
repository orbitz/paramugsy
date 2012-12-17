open Core.Std

let split_accession acc =
  String.lsplit2_exn ~on:'.' acc

let string_of_range = function
  | Maf.Sequence.Range.Forward (s, e) ->
    sprintf "+(%s, %s)" (Int64.to_string s) (Int64.to_string e)
  | Maf.Sequence.Range.Reverse (s, e) ->
    sprintf "-(%s, %s)" (Int64.to_string s) (Int64.to_string e)

let string_of_direction = function
  | Maf.Sequence.Range.Forward _ -> "+"
  | Maf.Sequence.Range.Reverse _ -> "-"

let start_of_range = function
  | Maf.Sequence.Range.Forward (s, _) -> s
  | Maf.Sequence.Range.Reverse (s, _) -> s

let end_of_range = function
  | Maf.Sequence.Range.Forward (_, e) -> e
  | Maf.Sequence.Range.Reverse (_, e) -> e

let extract_range_info = function
  | Maf.Sequence.Range.Forward (s, e) -> ("+", s, Int64.succ e)
  | Maf.Sequence.Range.Reverse (s, e) -> ("-", s, Int64.succ e)
