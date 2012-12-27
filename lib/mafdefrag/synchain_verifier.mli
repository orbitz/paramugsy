open Core.Std

val verify :
  Anchor.t Array.t ->
  Anchor_index.t ->
  Synchain.t Array.t ->
  (int * Maf.Sequence.Range.t * Maf.Sequence.Range.t) list
