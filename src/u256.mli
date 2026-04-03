type t

val zero : t
val one : t
val of_int : int -> t
val of_hex : string -> t
val to_hex : t -> string
val of_bytes_be : bytes -> t
val to_bytes_be : t -> bytes
val compare : t -> t -> int
val equal : t -> t -> bool
val is_zero : t -> bool
val get_bit : t -> int -> bool
val add_mod : t -> t -> t -> t
val sub_mod : t -> t -> t -> t
val mul_mod : t -> t -> t -> t
val square_mod : t -> t -> t
val pow_mod : t -> t -> t -> t
val sub_small : t -> int -> t
val add_raw : t -> t -> t * bool
