type t

val init : key:bytes -> iv:bytes -> t
val copy : t -> t
val next_word : t -> int32
val keystream_words : t -> int -> int32 array
val crypt : t -> bytes -> bytes
val crypt_with_key : key:bytes -> iv:bytes -> bytes -> bytes
