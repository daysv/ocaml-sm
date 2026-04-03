type round_keys

val key_schedule : bytes -> round_keys
val encrypt_block : round_keys -> bytes -> bytes
val decrypt_block : round_keys -> bytes -> bytes

val encrypt_block_with_key : bytes -> bytes -> bytes
val decrypt_block_with_key : bytes -> bytes -> bytes

module Cbc : sig
  val encrypt_no_pad : key:bytes -> iv:bytes -> bytes -> bytes
  val decrypt_no_pad : key:bytes -> iv:bytes -> bytes -> bytes
  val encrypt_pkcs7 : key:bytes -> iv:bytes -> bytes -> bytes
  val decrypt_pkcs7 : key:bytes -> iv:bytes -> bytes -> bytes option
end

module Ctr : sig
  val crypt : key:bytes -> iv:bytes -> bytes -> bytes
end

module Gcm : sig
  type encrypted = {
    ciphertext : bytes;
    tag : bytes;
  }

  val encrypt : key:bytes -> iv:bytes -> ?aad:bytes -> bytes -> encrypted
  val decrypt : key:bytes -> iv:bytes -> ?aad:bytes -> tag:bytes -> bytes -> bytes option
end

module Stream : sig
  module Cbc_encrypt : sig
    type padding = [ `No_padding | `Pkcs7 ]
    type t
    val init : key:bytes -> iv:bytes -> padding:padding -> t
    val update : t -> bytes -> unit
    val finalize : t -> bytes option
  end

  module Cbc_decrypt : sig
    type padding = [ `No_padding | `Pkcs7 ]
    type t
    val init : key:bytes -> iv:bytes -> padding:padding -> t
    val update : t -> bytes -> unit
    val finalize : t -> bytes option
  end

  module Ctr : sig
    type t
    val init : key:bytes -> iv:bytes -> t
    val update : t -> bytes -> bytes
  end

  module Gcm_encrypt : sig
    type t
    val init : key:bytes -> iv:bytes -> t
    val add_aad : t -> bytes -> unit
    val update : t -> bytes -> unit
    val finalize : t -> Gcm.encrypted
  end

  module Gcm_decrypt : sig
    type t
    val init : key:bytes -> iv:bytes -> t
    val add_aad : t -> bytes -> unit
    val update : t -> bytes -> unit
    val finalize : t -> tag:bytes -> bytes option
  end
end
