type t

val empty : t
val init : unit -> t
val update_subbytes : t -> bytes -> off:int -> len:int -> t
val update_bytes : t -> bytes -> t
val update_string : t -> string -> t
val finalize : t -> string
val finalize_hex : t -> string

val digest_string : string -> string
val digest_bytes : bytes -> string
val digest_hex : string -> string
