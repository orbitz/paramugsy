open Core.Std

val split_accession     : string -> (string * string) option
val string_of_range     : Maf.Sequence.Range.t -> string
val string_of_direction : Maf.Sequence.Range.t -> string
val start_of_range      : Maf.Sequence.Range.t -> Int64.t
val end_of_range        : Maf.Sequence.Range.t -> Int64.t
val extract_range_info  : Maf.Sequence.Range.t -> (string * Int64.t * Int64.t)
