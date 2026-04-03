type scalar
type point
type private_key = {
  private_scalar : scalar;
  public_point : point;
}
type key_exchange_role = [ `Initiator | `Responder ]
type key_exchange_result = {
  shared_key : string;
  confirmation_in : string;
  confirmation_out : string;
}

val scalar_of_hex : string -> scalar
val scalar_to_hex : scalar -> string

val point_of_hex : x:string -> y:string -> point
val point_to_hex : point -> string * string

val derive_public_key : scalar -> point
val za : id:string -> point -> string
val digest_for_sign : id:string -> point -> string -> string

val sign_digest : k:scalar -> priv:scalar -> digest:string -> scalar * scalar
val verify_digest : pub:point -> digest:string -> signature:scalar * scalar -> bool

val encrypt : k:scalar -> pub:point -> string -> string
val decrypt : priv:scalar -> string -> string option

val private_key_of_scalar : scalar -> private_key
val ephemeral_public_key : scalar -> point

val key_exchange :
  role:key_exchange_role ->
  self_id:string ->
  self_static:private_key ->
  self_ephemeral:scalar ->
  peer_id:string ->
  peer_static:point ->
  peer_ephemeral:point ->
  key_length:int ->
  key_exchange_result option

val encode_signature_der : scalar * scalar -> string
val decode_signature_der : string -> (scalar * scalar) option

val encode_private_key_sec1_der : private_key -> string
val decode_private_key_sec1_der : string -> private_key option

val encode_private_key_pkcs8_der : private_key -> string
val decode_private_key_pkcs8_der : string -> private_key option

val encode_public_key_der : point -> string
val decode_public_key_der : string -> point option

val encode_private_key_pem : [ `Sec1 | `Pkcs8 ] -> private_key -> string
val decode_private_key_pem : string -> private_key option

val encode_private_key_encrypted_pem :
  password:string -> salt:bytes -> iv:bytes -> iterations:int -> private_key -> string

val decode_private_key_encrypted_pem : password:string -> string -> private_key option

val encode_public_key_pem : point -> string
val decode_public_key_pem : string -> point option
