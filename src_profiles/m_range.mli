type t = private (int * int)
val overlap : t -> t -> t option
val reverse : t -> t
val get_start : t -> int
val get_end : t -> int
val get_direction : t -> [> `Forward | `Reverse]
val contains : t -> int -> bool
val length : t -> int
val lift : f:((int * int) -> (int * int)) -> t -> t
val of_tuple : (int * int) -> t
val to_tuple : t -> (int * int)
val of_maf : start:int -> size:int -> src_size:int -> direction:[< `Forward | `Reverse ] -> t
