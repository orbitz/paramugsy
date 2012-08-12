let ref_count = ref 0
let make_ref () = ref_count := !ref_count + 1; string_of_int !ref_count
