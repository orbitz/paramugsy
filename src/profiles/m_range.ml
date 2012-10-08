(*
 * A range is a 1-indexed and inclusive range on a genome.  For example the range (6, 6)
 * would represent the element at the 6th point in the genome.  (3, 5) would represent
 * elements at points 3, 4, and 5 in the genome.
 *)
type t = (int * int)

let swap_if_needed (s, e) = if s < e then (s, e) else (e, s)

(*
 * Determines if r1 and r2 overlap and if so returns the overlapping
 * region.
 * (s, e) is the maximum starting point and the minimum ending
 * point.  That means if e is less than s, there is no overlapping
 * region because you have something like:
 * s1 <------> e1
 *                s2 <---------> e2
 * rather than
 * s1 <--------> e1
 *        s2 <------> e2
 * 
 *)
let overlap r1 r2 =
  let (s1, e1) = swap_if_needed r1 in
  let (s2, e2) = swap_if_needed r2 in
  let (s, e) = (max s1 s2, min e1 e2) in
  if e - s >= 0 then
    Some (s, e)
  else
    None


let reverse (s, e) = (e, s)

let get_start = fst
let get_end = snd

let get_direction (s, e) =
  if s <= e then
    `Forward
  else
    `Reverse

let contains r v =
  let (s, e) = swap_if_needed r in
  s <= v && v <= e

let length (s, e) = abs (s - e) + 1 

let lift ~f r = f r

let of_tuple tup = tup

let to_tuple r = r

(*
 * MAF is 0 indexed and has directions.  This takes that information
 * and converts it to a range
 *)
let of_maf ~start ~size ~src_size ~direction = 
  match direction with
    | `Forward ->
      (start + 1, start + size)
    | `Reverse ->
      (src_size - start, src_size - start - (size - 1))
