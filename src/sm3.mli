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

(** HMAC using SM3 as the underlying hash function.
    @param key secret key
    @param data message to authenticate
    @return HMAC tag as raw bytes (string) *)
val hmac : key:string -> string -> string

(** HMAC using SM3, returning the tag as a hexadecimal string. *)
val hmac_hex : key:string -> string -> string

(** KDF (Key Derivation Function) based on SM3, as specified in GM/T 0003.4-2012.
    @param z shared secret
    @param klen desired length of the derived key in bytes
    @return derived key of length [klen] *)
val kdf : z:string -> klen:int -> string
