type scalar = U256.t
type point = Infinity | Point of U256.t * U256.t
type private_key = { private_scalar : scalar; public_point : point }
type key_exchange_role = [ `Initiator | `Responder ]
type key_exchange_result = {
  shared_key : string;
  confirmation_in : string;
  confirmation_out : string;
}

type jacobian = { x : U256.t; y : U256.t; z : U256.t; inf : bool }

let p = U256.of_hex "FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFF"
let a = U256.of_hex "FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFC"
let b = U256.of_hex "28E9FA9E9D9F5E344D5A9E4BCF6509A7F39789F515AB8F92DDBCBD414D940E93"
let n = U256.of_hex "FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFF7203DF6B21C6052B53BBF40939D54123"
let gx = U256.of_hex "32C4AE2C1F1981195F9904466A39C9948FE30BBFF2660BE1715A4589334C74C7"
let gy = U256.of_hex "BC3736A2F4F6779C59BDCEE36B692153D0A9877CC62A474002DF32E52139F0A0"
let g = Point (gx, gy)
let a_bytes = U256.to_bytes_be a
let b_bytes = U256.to_bytes_be b
let gx_bytes = U256.to_bytes_be gx
let gy_bytes = U256.to_bytes_be gy
let p_minus_2 = U256.sub_small p 2
let n_minus_2 = U256.sub_small n 2
let three = U256.of_int 3

let scalar_of_hex = U256.of_hex
let scalar_to_hex = U256.to_hex

let point_of_hex ~x ~y = Point (U256.of_hex x, U256.of_hex y)

let point_to_hex = function
  | Infinity -> invalid_arg "Sm2.point_to_hex"
  | Point (x, y) -> (U256.to_hex x, U256.to_hex y)

let bytes_concat3 a b c = Bytes.unsafe_to_string (Bytes.concat Bytes.empty [Bytes.of_string a; Bytes.of_string b; Bytes.of_string c])

let mod_add = U256.add_mod
let mod_sub = U256.sub_mod
let mod_mul = U256.mul_mod
let mod_sqr = U256.square_mod
let mod_inv_p x = U256.pow_mod x p_minus_2 p
let mod_inv_n x = U256.pow_mod x n_minus_2 n
let double_mod = U256.double_mod

let triple_mod x modulus =
  let x2 = mod_add x x modulus in
  mod_add x2 x modulus

let point_at_infinity = { x = U256.zero; y = U256.one; z = U256.zero; inf = true }

let jacobian_of_point = function
  | Infinity -> point_at_infinity
  | Point (x, y) -> { x; y; z = U256.one; inf = false }

let point_of_jacobian j =
  if j.inf || U256.is_zero j.z then Infinity
  else
    let z_inv = mod_inv_p j.z in
    let z2 = mod_sqr z_inv p in
    let z3 = mod_mul z2 z_inv p in
    let x = mod_mul j.x z2 p in
    let y = mod_mul j.y z3 p in
    Point (x, y)

let point_double j =
  if j.inf || U256.is_zero j.y then point_at_infinity
  else
    let delta = mod_sqr j.z p in
    let gamma = mod_sqr j.y p in
    let beta = mod_mul j.x gamma p in
    let x_minus_delta = mod_sub j.x delta p in
    let x_plus_delta = mod_add j.x delta p in
    let alpha = mod_mul three (mod_mul x_minus_delta x_plus_delta p) p in
    let alpha2 = mod_sqr alpha p in
    let eight_beta =
      let two_beta = mod_add beta beta p in
      let four_beta = mod_add two_beta two_beta p in
      mod_add four_beta four_beta p
    in
    let x3 = mod_sub alpha2 eight_beta p in
    let y_plus_z = mod_add j.y j.z p in
    let z3 = mod_sub (mod_sub (mod_sqr y_plus_z p) gamma p) delta p in
    let gamma2 = mod_sqr gamma p in
    let eight_gamma2 =
      let two = mod_add gamma2 gamma2 p in
      let four = mod_add two two p in
      mod_add four four p
    in
    let four_beta_minus_x3 =
      let two = mod_add beta beta p in
      let four = mod_add two two p in
      mod_sub four x3 p
    in
    let y3 = mod_sub (mod_mul alpha four_beta_minus_x3 p) eight_gamma2 p in
    { x = x3; y = y3; z = z3; inf = false }

let point_add_jacobian p1 p2 =
  if p1.inf then p2
  else if p2.inf then p1
  else
    let z1z1 = mod_sqr p1.z p in
    let z2z2 = mod_sqr p2.z p in
    let u1 = mod_mul p1.x z2z2 p in
    let u2 = mod_mul p2.x z1z1 p in
    let z1_cubed = mod_mul p1.z z1z1 p in
    let z2_cubed = mod_mul p2.z z2z2 p in
    let s1 = mod_mul p1.y z2_cubed p in
    let s2 = mod_mul p2.y z1_cubed p in
    if U256.equal u1 u2 then
      if U256.equal s1 s2 then point_double p1 else point_at_infinity
    else
      let h = mod_sub u2 u1 p in
      let r = mod_sub s2 s1 p in
      let hh = mod_sqr h p in
      let hhh = mod_mul h hh p in
      let u1hh = mod_mul u1 hh p in
      let x3 = mod_sub (mod_sub (mod_sqr r p) hhh p) (mod_add u1hh u1hh p) p in
      let y3 = mod_sub (mod_mul r (mod_sub u1hh x3 p) p) (mod_mul s1 hhh p) p in
      let z1z2 = mod_mul p1.z p2.z p in
      let z3 = mod_mul h z1z2 p in
      { x = x3; y = y3; z = z3; inf = false }

let point_add_mixed j = function
  | Infinity -> j
  | Point (x2, y2) ->
      if j.inf then jacobian_of_point (Point (x2, y2))
      else
        let z1z1 = mod_sqr j.z p in
        let u2 = mod_mul x2 z1z1 p in
        let s2 = mod_mul y2 (mod_mul j.z z1z1 p) p in
        if U256.equal j.x u2 then
          if U256.equal j.y s2 then point_double j else point_at_infinity
        else
          let h = mod_sub u2 j.x p in
          let hh = mod_sqr h p in
          let i = mod_add hh hh p |> fun x -> mod_add x x p in
          let jv = mod_mul h i p in
          let r = mod_add (mod_sub s2 j.y p) (mod_sub s2 j.y p) p in
          let v = mod_mul j.x i p in
          let x3 = mod_sub (mod_sub (mod_sqr r p) jv p) (mod_add v v p) p in
          let y3 =
            mod_sub
              (mod_mul r (mod_sub v x3 p) p)
              (mod_mul (mod_add j.y j.y p) jv p)
              p
          in
          let z3 = mod_sub (mod_sub (mod_sqr (mod_add j.z h p) p) z1z1 p) hh p in
          { x = x3; y = y3; z = z3; inf = false }

let select_jacobian table idx =
  Array.unsafe_get table idx

let window_of_scalar k shift width =
  if width = 4 then U256.get_nibble k shift
  else
    let acc = ref 0 in
    for bit = 0 to width - 1 do
      let pos = shift + bit in
      if pos < 256 && U256.get_bit k pos then acc := !acc lor (1 lsl bit)
    done;
    !acc

let scalar_mult_with_table_jacobian ~width table k =
  let acc = ref point_at_infinity in
  let first = ref true in
  let windows = (256 + width - 1) / width in
  for window = windows - 1 downto 0 do
    if !first then first := false
    else (
      for _ = 1 to width do
        acc := point_double !acc
      done
    );
    let digit = window_of_scalar k (window * width) width in
    if digit <> 0 then acc := point_add_jacobian !acc (select_jacobian table digit)
  done;
  !acc

let scalar_mult_with_table ~width table k =
  point_of_jacobian (scalar_mult_with_table_jacobian ~width table k)

let precompute_window_width width point =
  let size = 1 lsl width in
  let table = Array.make size point_at_infinity in
  table.(1) <- jacobian_of_point point;
  for i = 2 to size - 1 do
    table.(i) <- point_add_jacobian table.(i - 1) table.(1)
  done;
  table

let g_table_affine =
  let table = precompute_window_width 5 g in
  Array.map point_of_jacobian table

let scalar_mult_g_jacobian k =
  let acc = ref point_at_infinity in
  let first = ref true in
  let width = 5 in
  let windows = (256 + width - 1) / width in
  for window = windows - 1 downto 0 do
    if !first then first := false
    else (
      for _ = 1 to width do
        acc := point_double !acc
      done
    );
    let digit = window_of_scalar k (window * width) width in
    if digit <> 0 then acc := point_add_mixed !acc (Array.unsafe_get g_table_affine digit)
  done;
  !acc

let scalar_mult_g k = point_of_jacobian (scalar_mult_g_jacobian k)

let scalar_mult_jacobian k point =
  scalar_mult_with_table_jacobian ~width:5 (precompute_window_width 5 point) k

let scalar_mult k point = point_of_jacobian (scalar_mult_jacobian k point)

let scalar_mult_simultaneous_jacobian s t pub =
  let p_table = precompute_window_width 4 pub in
  let acc = ref point_at_infinity in
  for i = 63 downto 0 do
    for _ = 1 to 4 do
      acc := point_double !acc
    done;
    let g_digit = window_of_scalar s (i * 4) 4 in
    if g_digit <> 0 then acc := point_add_mixed !acc (Array.unsafe_get g_table_affine g_digit);
    let p_digit = window_of_scalar t (i * 4) 4 in
    if p_digit <> 0 then acc := point_add_jacobian !acc (Array.unsafe_get p_table p_digit)
  done;
  !acc

let derive_public_key d = scalar_mult_g d
let private_key_of_scalar private_scalar = { private_scalar; public_point = derive_public_key private_scalar }
let ephemeral_public_key = derive_public_key

let point_add p1 p2 =
  match p1 with
  | Infinity -> p2
  | Point _ ->
      let j = jacobian_of_point p1 in
      point_of_jacobian (point_add_mixed j p2)

let point_add_j_affine j1 p2 =
  point_of_jacobian (point_add_mixed j1 p2)

let validate_public_key ?(check_order = false) point =
  match point with
  | Infinity -> false
  | Point (x, y) ->
      (* Check coordinates are in range [0, p-1] *)
      if U256.compare x p >= 0 || U256.compare y p >= 0 then false
      else
        (* Check curve equation: y^2 = x^3 + a*x + b (mod p) *)
        let x3 = mod_mul x (mod_sqr x p) p in
        let ax = mod_mul a x p in
        let left = mod_sqr y p in
        let right = mod_add (mod_add x3 ax p) b p in
        if not (U256.equal left right) then false
        else if not check_order then true
        else
          (* Check point order: [n]P = O *)
          let table = precompute_window_width 4 point in
          let result = scalar_mult_with_table_jacobian ~width:4 table n in
          result.inf

let point_to_bytes = function
  | Infinity -> invalid_arg "Sm2.point_to_bytes"
  | Point (x, y) -> (U256.to_bytes_be x, U256.to_bytes_be y)

let encode_point = function
  | Infinity -> invalid_arg "Sm2.encode_point"
  | Point (x, y) ->
      let xb = U256.to_bytes_be x in
      let yb = U256.to_bytes_be y in
      let out = Bytes.create 65 in
      Bytes.set out 0 '\x04';
      Bytes.blit xb 0 out 1 32;
      Bytes.blit yb 0 out 33 32;
      Bytes.unsafe_to_string out

let parse_point bytes off =
  if Bytes.length bytes - off < 65 then invalid_arg "Sm2.parse_point";
  if Bytes.get bytes off <> '\x04' then invalid_arg "Sm2.parse_point";
  let x = Bytes.sub bytes (off + 1) 32 |> U256.of_bytes_be in
  let y = Bytes.sub bytes (off + 33) 32 |> U256.of_bytes_be in
  Point (x, y)

let point_to_raw = function
  | Infinity -> invalid_arg "Sm2.point_to_raw"
  | Point (x, y) -> (Bytes.unsafe_to_string (U256.to_bytes_be x), Bytes.unsafe_to_string (U256.to_bytes_be y))

let sm3_bytes bytes = Sm3.digest_bytes bytes

let za ~id pub =
  let id_len = String.length id in
  let data = Bytes.create (2 + id_len + 192) in
  let entl = id_len * 8 in
  Bytes.set data 0 (Char.chr ((entl lsr 8) land 0xff));
  Bytes.set data 1 (Char.chr (entl land 0xff));
  Bytes.blit_string id 0 data 2 id_len;
  let off = 2 + id_len in
  Bytes.blit a_bytes 0 data off 32;
  Bytes.blit b_bytes 0 data (off + 32) 32;
  Bytes.blit gx_bytes 0 data (off + 64) 32;
  Bytes.blit gy_bytes 0 data (off + 96) 32;
  let px, py = point_to_bytes pub in
  Bytes.blit px 0 data (off + 128) 32;
  Bytes.blit py 0 data (off + 160) 32;
  sm3_bytes data

let digest_for_sign ~id pub msg =
  Sm3.digest_string (za ~id pub ^ msg)

let digest_to_scalar digest =
  if String.length digest <> 32 then invalid_arg "Sm2.digest";
  U256.of_bytes_be (Bytes.of_string digest)

let ensure_valid_nonce k =
  if U256.is_zero k || U256.compare k n >= 0 then invalid_arg "Sm2 nonce out of range"

let sign_digest ~k ~priv ~digest =
  ensure_valid_nonce k;
  let e = digest_to_scalar digest in
  let x1, _ =
    match scalar_mult_g_jacobian k |> point_of_jacobian with
    | Infinity -> invalid_arg "Sm2.sign_digest"
    | Point (x, y) -> (x, y)
  in
  let r = mod_add e x1 n in
  let rk, _ = U256.add_raw r k in
  if U256.is_zero r || U256.compare rk n = 0 then invalid_arg "Sm2.sign_digest invalid r";
  let one_plus_d = mod_add U256.one priv n in
  let inv = mod_inv_n one_plus_d in
  let rd = mod_mul r priv n in
  let kd = mod_sub k rd n in
  let s = mod_mul inv kd n in
  if U256.is_zero s then invalid_arg "Sm2.sign_digest invalid s";
  (r, s)

let verify_digest ~pub ~digest ~signature =
  let r, s = signature in
  if U256.is_zero r || U256.compare r n >= 0 || U256.is_zero s || U256.compare s n >= 0 then false
  else
    let e = digest_to_scalar digest in
    let t = mod_add r s n in
    if U256.is_zero t then false
    else
      match scalar_mult_simultaneous_jacobian s t pub |> point_of_jacobian with
      | Infinity -> false
      | Point (x1, _) ->
          let rr = mod_add e x1 n in
          U256.equal rr r

let kdf z klen =
  if klen < 0 then invalid_arg "Sm2.kdf: klen must be non-negative";
  (* GM/T 0003.4-2012: maximum derived key length is (2^32 - 1) * 32 bytes *)
  if klen > (Int64.to_int (Int64.sub (Int64.shift_left 1L 32) 1L)) * 32 then
    invalid_arg "Sm2.kdf: klen exceeds maximum allowed (2^32 - 1) * 32";
  let blocks = if klen = 0 then 0 else (klen + 31) / 32 in
  let out = Bytes.create klen in
  let z_len = String.length z in
  for ct = 1 to blocks do
    let block_bytes = Bytes.create (z_len + 4) in
    Bytes.blit_string z 0 block_bytes 0 z_len;
    Bytes.set block_bytes z_len (Char.chr ((ct lsr 24) land 0xff));
    Bytes.set block_bytes (z_len + 1) (Char.chr ((ct lsr 16) land 0xff));
    Bytes.set block_bytes (z_len + 2) (Char.chr ((ct lsr 8) land 0xff));
    Bytes.set block_bytes (z_len + 3) (Char.chr (ct land 0xff));
    let block = Sm3.digest_bytes block_bytes in
    let take = min 32 (klen - ((ct - 1) * 32)) in
    Bytes.blit_string block 0 out ((ct - 1) * 32) take
  done;
  Bytes.unsafe_to_string out

let all_zero s =
  let rec loop i =
    i = String.length s || (s.[i] = '\000' && loop (i + 1))
  in
  loop 0

let xor_strings a b =
  let len = String.length a in
  let out = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set out i (Char.chr (Char.code a.[i] lxor Char.code b.[i]))
  done;
  Bytes.unsafe_to_string out

let encrypt ~k ~pub msg =
  ensure_valid_nonce k;
  let c1 = scalar_mult_g k in
  let x2, y2 =
    match scalar_mult_jacobian k pub |> point_of_jacobian with
    | Infinity -> invalid_arg "Sm2.encrypt"
    | Point (x, y) -> (x, y)
  in
  let xb = U256.to_bytes_be x2 in
  let yb = U256.to_bytes_be y2 in
  let z = Bytes.unsafe_to_string (Bytes.cat xb yb) in
  let t = kdf z (String.length msg) in
  if all_zero t then invalid_arg "Sm2.encrypt kdf returned zero";
  let c2 = xor_strings msg t in
  let c3 = Sm3.digest_string (Bytes.unsafe_to_string xb ^ msg ^ Bytes.unsafe_to_string yb) in
  encode_point c1 ^ c3 ^ c2

let decrypt ~priv cipher =
  let bytes = Bytes.of_string cipher in
  if Bytes.length bytes < 97 then None
  else
    let c1 = parse_point bytes 0 in
    let c3 = Bytes.sub_string bytes 65 32 in
    let c2 = Bytes.sub_string bytes 97 (Bytes.length bytes - 97) in
    match scalar_mult_jacobian priv c1 |> point_of_jacobian with
    | Infinity -> None
    | Point (x2, y2) ->
        let xb = U256.to_bytes_be x2 in
        let yb = U256.to_bytes_be y2 in
        let z = Bytes.unsafe_to_string (Bytes.cat xb yb) in
        let t = kdf z (String.length c2) in
        if all_zero t then None
        else
          let msg = xor_strings c2 t in
          let u = Sm3.digest_string (Bytes.unsafe_to_string xb ^ msg ^ Bytes.unsafe_to_string yb) in
          if String.equal u c3 then Some msg else None

let x_bar x =
  let b = U256.to_bytes_be x in
  for i = 0 to 15 do
    Bytes.set b i '\000'
  done;
  Bytes.set b 16 (Char.chr ((Char.code (Bytes.get b 16) land 0x7f) lor 0x80));
  U256.of_bytes_be b

let key_exchange ~role ~self_id ~self_static ~self_ephemeral ~peer_id ~peer_static ~peer_ephemeral ~key_length =
  try
    let ra = derive_public_key self_ephemeral in
    let x1 =
      match ra with
      | Infinity -> raise Exit
      | Point (x, _) -> x
    in
    let x1b = x_bar x1 in
    let ta = mod_add self_static.private_scalar (mod_mul x1b self_ephemeral n) n in
    let x2b =
      match peer_ephemeral with
      | Infinity -> raise Exit
      | Point (x, _) -> x_bar x
    in
    let mixed =
      match point_add peer_static (scalar_mult x2b peer_ephemeral) with
      | Infinity -> raise Exit
      | p -> p
    in
    let u =
      match scalar_mult ta mixed with
      | Infinity -> raise Exit
      | p -> p
    in
    let xu, yu = point_to_raw u in
    let za_self = za ~id:self_id self_static.public_point in
    let za_peer = za ~id:peer_id peer_static in
    let za1, za2, ra_local, rb_peer =
      match role with
      | `Initiator -> (za_self, za_peer, ra, peer_ephemeral)
      | `Responder -> (za_peer, za_self, peer_ephemeral, ra)
    in
    let k = kdf (xu ^ yu ^ za1 ^ za2) key_length in
    let x1r, y1r = point_to_raw ra_local in
    let x2r, y2r = point_to_raw rb_peer in
    let inner = Sm3.digest_string (xu ^ za1 ^ za2 ^ x1r ^ y1r ^ x2r ^ y2r) in
    let s1 = Sm3.digest_string ("\x02" ^ yu ^ inner) in
    let s2 = Sm3.digest_string ("\x03" ^ yu ^ inner) in
    let confirmation_in, confirmation_out =
      match role with
      | `Initiator -> (s1, s2)
      | `Responder -> (s2, s1)
    in
    Some { shared_key = k; confirmation_in; confirmation_out }
  with Exit -> None

module Der = struct
  let oid_ec_public_key = "\x2a\x86\x48\xce\x3d\x02\x01"
  let oid_sm2 = "\x2a\x81\x1c\xcf\x55\x01\x82\x2d"
  let oid_pbes2 = "\x2a\x86\x48\x86\xf7\x0d\x01\x05\x0d"
  let oid_pbkdf2 = "\x2a\x86\x48\x86\xf7\x0d\x01\x05\x0c"
  let oid_hmac_sm3 = "\x2a\x81\x1c\xcf\x55\x01\x83\x12"
  let oid_sm4_cbc = "\x2a\x81\x1c\xcf\x55\x01\x68\x02"

  let length_bytes len =
    if len < 0x80 then String.make 1 (Char.chr len)
    else if len < 0x100 then String.init 2 (function 0 -> Char.chr 0x81 | _ -> Char.chr len)
    else String.init 3 (function 0 -> Char.chr 0x82 | 1 -> Char.chr (len lsr 8) | _ -> Char.chr (len land 0xff))

  let tlv tag content = String.make 1 (Char.chr tag) ^ length_bytes (String.length content) ^ content
  let integer_bytes bytes =
    if Bytes.length bytes = 0 then Bytes.of_string "\000"
    else
      let rec first_non_zero i =
        if i >= Bytes.length bytes - 1 then i
        else if Bytes.get bytes i <> '\000' then i
        else first_non_zero (i + 1)
      in
      let trimmed = Bytes.sub bytes (first_non_zero 0) (Bytes.length bytes - first_non_zero 0) in
      if Char.code (Bytes.get trimmed 0) land 0x80 <> 0 then Bytes.cat (Bytes.of_string "\000") trimmed else trimmed

  let integer_of_u256 x = tlv 0x02 (Bytes.unsafe_to_string (integer_bytes (U256.to_bytes_be x)))
  let octet_string s = tlv 0x04 s
  let bit_string s = tlv 0x03 ("\000" ^ s)
  let sequence xs = tlv 0x30 (String.concat "" xs)
  let oid s = tlv 0x06 s
  let context tag s = tlv (0xa0 + tag) s
  let null = tlv 0x05 ""
  let integer_int n =
    if n < 0 then invalid_arg "Der.integer_int";
    let rec bytes acc v =
      if v = 0 then acc else bytes (Char.chr (v land 0xff) :: acc) (v lsr 8)
    in
    let raw = if n = 0 then "\000" else String.init (List.length (bytes [] n)) (fun i -> List.nth (bytes [] n) i) in
    tlv 0x02 raw

  type reader = { s : string; mutable pos : int }

  let make_reader s = { s; pos = 0 }
  let remaining r = String.length r.s - r.pos

  let read_byte r =
    if remaining r <= 0 then raise Exit;
    let c = Char.code r.s.[r.pos] in
    r.pos <- r.pos + 1;
    c

  let read_length r =
    let b = read_byte r in
    if b land 0x80 = 0 then b
    else
      let n = b land 0x7f in
      let len = ref 0 in
      for _ = 1 to n do
        len := (!len lsl 8) lor read_byte r
      done;
      !len

  let read_tlv r =
    let tag = read_byte r in
    let len = read_length r in
    if remaining r < len then raise Exit;
    let content = String.sub r.s r.pos len in
    r.pos <- r.pos + len;
    (tag, content)

  let expect_tag tag (t, content) = if t <> tag then raise Exit else content

  let parse_integer_u256 content =
    let bytes = Bytes.of_string content in
    let bytes =
      if Bytes.length bytes > 0 && Bytes.get bytes 0 = '\000' then Bytes.sub bytes 1 (Bytes.length bytes - 1) else bytes
    in
    if Bytes.length bytes > 32 then raise Exit;
    U256.of_bytes_be bytes
end

module Pem = struct
  let encode_base64 s =
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
    let len = String.length s in
    let out = Buffer.create ((len + 2) / 3 * 4) in
    let rec loop i =
      if i >= len then ()
      else
        let b0 = Char.code s.[i] in
        let b1 = if i + 1 < len then Char.code s.[i + 1] else 0 in
        let b2 = if i + 2 < len then Char.code s.[i + 2] else 0 in
        Buffer.add_char out alphabet.[b0 lsr 2];
        Buffer.add_char out alphabet.[((b0 land 0x03) lsl 4) lor (b1 lsr 4)];
        Buffer.add_char out (if i + 1 < len then alphabet.[((b1 land 0x0f) lsl 2) lor (b2 lsr 6)] else '=');
        Buffer.add_char out (if i + 2 < len then alphabet.[b2 land 0x3f] else '=');
        loop (i + 3)
    in
    loop 0;
    Buffer.contents out

  let decode_base64 s =
    let decode = function
      | 'A' .. 'Z' as c -> Char.code c - Char.code 'A'
      | 'a' .. 'z' as c -> 26 + Char.code c - Char.code 'a'
      | '0' .. '9' as c -> 52 + Char.code c - Char.code '0'
      | '+' -> 62
      | '/' -> 63
      | '=' -> -1
      | _ -> -2
    in
    let filtered = Buffer.create (String.length s) in
    String.iter (fun c -> if c <> '\n' && c <> '\r' && c <> ' ' && c <> '\t' then Buffer.add_char filtered c) s;
    let t = Buffer.contents filtered in
    if String.length t mod 4 <> 0 then raise Exit;
    let out = Buffer.create (String.length t / 4 * 3) in
    let rec loop i =
      if i >= String.length t then ()
      else
        let a = decode t.[i] and b = decode t.[i + 1] and c = decode t.[i + 2] and d = decode t.[i + 3] in
        if a < 0 || b < 0 || c = -2 || d = -2 then raise Exit;
        Buffer.add_char out (Char.chr ((a lsl 2) lor (b lsr 4)));
        if c >= 0 then Buffer.add_char out (Char.chr (((b land 0x0f) lsl 4) lor (c lsr 2)));
        if d >= 0 then Buffer.add_char out (Char.chr (((c land 0x03) lsl 6) lor d));
        loop (i + 4)
    in
    loop 0;
    Buffer.contents out

  let wrap_lines s =
    let out = Buffer.create (String.length s + (String.length s / 64) + 8) in
    let rec loop i =
      if i >= String.length s then ()
      else
        let take = min 64 (String.length s - i) in
        Buffer.add_substring out s i take;
        Buffer.add_char out '\n';
        loop (i + take)
    in
    loop 0;
    Buffer.contents out

  let encode label der =
    "-----BEGIN " ^ label ^ "-----\n"
    ^ wrap_lines (encode_base64 der)
    ^ "-----END " ^ label ^ "-----\n"

  let decode label pem =
    let begin_marker = "-----BEGIN " ^ label ^ "-----" in
    let end_marker = "-----END " ^ label ^ "-----" in
    let find_sub s sub start =
      let rec loop i =
        if i + String.length sub > String.length s then None
        else if String.sub s i (String.length sub) = sub then Some i
        else loop (i + 1)
      in
      loop start
    in
    match find_sub pem begin_marker 0, find_sub pem end_marker 0 with
    | Some b, Some e ->
        let payload = String.sub pem (b + String.length begin_marker) (e - (b + String.length begin_marker)) in
        Some (decode_base64 payload)
    | _ -> None
end

let encode_signature_der (r, s) =
  Der.sequence [Der.integer_of_u256 r; Der.integer_of_u256 s]

let decode_signature_der der =
  try
    let r = Der.make_reader der in
    let seq = Der.expect_tag 0x30 (Der.read_tlv r) in
    if Der.remaining r <> 0 then raise Exit;
    let r2 = Der.make_reader seq in
    let rv = Der.parse_integer_u256 (Der.expect_tag 0x02 (Der.read_tlv r2)) in
    let sv = Der.parse_integer_u256 (Der.expect_tag 0x02 (Der.read_tlv r2)) in
    if Der.remaining r2 <> 0 then raise Exit;
    Some (rv, sv)
  with Exit -> None

let encode_public_key_der point =
  Der.sequence
    [
      Der.sequence [Der.oid Der.oid_ec_public_key; Der.oid Der.oid_sm2];
      Der.bit_string (encode_point point);
    ]

let decode_public_key_der der =
  try
    let r = Der.make_reader der in
    let seq = Der.expect_tag 0x30 (Der.read_tlv r) in
    if Der.remaining r <> 0 then raise Exit;
    let r2 = Der.make_reader seq in
    let alg = Der.expect_tag 0x30 (Der.read_tlv r2) in
    let ra = Der.make_reader alg in
    if Der.expect_tag 0x06 (Der.read_tlv ra) <> Der.oid_ec_public_key then raise Exit;
    if Der.expect_tag 0x06 (Der.read_tlv ra) <> Der.oid_sm2 then raise Exit;
    if Der.remaining ra <> 0 then raise Exit;
    let bits = Der.expect_tag 0x03 (Der.read_tlv r2) in
    if String.length bits < 1 || bits.[0] <> '\000' then raise Exit;
    if Der.remaining r2 <> 0 then raise Exit;
    Some (parse_point (Bytes.of_string bits) 1)
  with Exit -> None

let encode_ec_private_key_der ?(include_params = true) key =
  let fields =
    [
      Der.tlv 0x02 "\001";
      Der.octet_string (Bytes.unsafe_to_string (U256.to_bytes_be key.private_scalar));
    ]
  in
  let fields =
    if include_params then fields @ [Der.context 0 (Der.oid Der.oid_sm2)] else fields
  in
  let fields = fields @ [Der.context 1 (Der.bit_string (encode_point key.public_point))] in
  Der.sequence fields

let encode_private_key_sec1_der key =
  encode_ec_private_key_der ~include_params:true key

let decode_private_key_sec1_der der =
  try
    let r = Der.make_reader der in
    let seq = Der.expect_tag 0x30 (Der.read_tlv r) in
    if Der.remaining r <> 0 then raise Exit;
    let r2 = Der.make_reader seq in
    ignore (Der.expect_tag 0x02 (Der.read_tlv r2));
    let priv = U256.of_bytes_be (Bytes.of_string (Der.expect_tag 0x04 (Der.read_tlv r2))) in
    let pub = ref None in
    while Der.remaining r2 > 0 do
      let tag, content = Der.read_tlv r2 in
      match tag with
      | 0xa0 ->
          let rr = Der.make_reader content in
          if Der.expect_tag 0x06 (Der.read_tlv rr) <> Der.oid_sm2 then raise Exit
      | 0xa1 ->
          let rr = Der.make_reader content in
          let bits = Der.expect_tag 0x03 (Der.read_tlv rr) in
          if String.length bits < 1 || bits.[0] <> '\000' then raise Exit;
          pub := Some (parse_point (Bytes.of_string bits) 1)
      | _ -> raise Exit
    done;
    let public_point = match !pub with Some p -> p | None -> derive_public_key priv in
    Some { private_scalar = priv; public_point }
  with Exit -> None

let encode_private_key_pkcs8_der key =
  Der.sequence
    [
      Der.tlv 0x02 "\000";
      Der.sequence [Der.oid Der.oid_ec_public_key; Der.oid Der.oid_sm2];
      Der.octet_string (encode_ec_private_key_der ~include_params:false key);
    ]

let decode_private_key_pkcs8_der der =
  try
    let r = Der.make_reader der in
    let seq = Der.expect_tag 0x30 (Der.read_tlv r) in
    if Der.remaining r <> 0 then raise Exit;
    let r2 = Der.make_reader seq in
    ignore (Der.expect_tag 0x02 (Der.read_tlv r2));
    let alg = Der.expect_tag 0x30 (Der.read_tlv r2) in
    let ra = Der.make_reader alg in
    if Der.expect_tag 0x06 (Der.read_tlv ra) <> Der.oid_ec_public_key then raise Exit;
    if Der.expect_tag 0x06 (Der.read_tlv ra) <> Der.oid_sm2 then raise Exit;
    if Der.remaining ra <> 0 then raise Exit;
    let inner = Der.expect_tag 0x04 (Der.read_tlv r2) in
    if Der.remaining r2 <> 0 then raise Exit;
    decode_private_key_sec1_der inner
  with Exit -> None

let encode_private_key_pem kind key =
  match kind with
  | `Sec1 -> Pem.encode "EC PRIVATE KEY" (encode_private_key_sec1_der key)
  | `Pkcs8 -> Pem.encode "PRIVATE KEY" (encode_private_key_pkcs8_der key)

let decode_private_key_pem pem =
  match Pem.decode "PRIVATE KEY" pem with
  | Some der -> decode_private_key_pkcs8_der der
  | None ->
      (match Pem.decode "EC PRIVATE KEY" pem with
       | Some der -> decode_private_key_sec1_der der
       | None -> None)

let encode_public_key_pem point = Pem.encode "PUBLIC KEY" (encode_public_key_der point)

let decode_public_key_pem pem =
  match Pem.decode "PUBLIC KEY" pem with
  | Some der -> decode_public_key_der der
  | None -> None

let hmac_sm3 ~key data =
  let block_size = 64 in
  let key =
    if String.length key > block_size then Sm3.digest_string key else key
  in
  let key = key ^ String.make (block_size - String.length key) '\000' in
  let ipad = Bytes.make block_size '\x36' in
  let opad = Bytes.make block_size '\x5c' in
  for i = 0 to block_size - 1 do
    let k = Char.code key.[i] in
    Bytes.set ipad i (Char.chr (k lxor Char.code (Bytes.get ipad i)));
    Bytes.set opad i (Char.chr (k lxor Char.code (Bytes.get opad i)))
  done;
  let inner = Sm3.digest_string (Bytes.unsafe_to_string ipad ^ data) in
  Sm3.digest_string (Bytes.unsafe_to_string opad ^ inner)

let pbkdf2_sm3 ~password ~salt ~iterations ~dk_len =
  if iterations <= 0 || dk_len < 0 then invalid_arg "Sm2.pbkdf2_sm3";
  let hlen = 32 in
  let blocks = if dk_len = 0 then 0 else (dk_len + hlen - 1) / hlen in
  let out = Bytes.create dk_len in
  for i = 1 to blocks do
    let ctr = Bytes.make 4 '\000' in
    Bytes.set ctr 0 (Char.chr ((i lsr 24) land 0xff));
    Bytes.set ctr 1 (Char.chr ((i lsr 16) land 0xff));
    Bytes.set ctr 2 (Char.chr ((i lsr 8) land 0xff));
    Bytes.set ctr 3 (Char.chr (i land 0xff));
    let u = ref (hmac_sm3 ~key:password (Bytes.unsafe_to_string salt ^ Bytes.unsafe_to_string ctr)) in
    let t = Bytes.of_string !u in
    for _ = 2 to iterations do
      u := hmac_sm3 ~key:password !u;
      for j = 0 to hlen - 1 do
        Bytes.set t j (Char.chr (Char.code (Bytes.get t j) lxor Char.code (!u).[j]))
      done
    done;
    let take = min hlen (dk_len - ((i - 1) * hlen)) in
    Bytes.blit t 0 out ((i - 1) * hlen) take
  done;
  out

let encode_private_key_encrypted_pem ~password ~salt ~iv ~iterations key =
  if Bytes.length iv <> 16 then invalid_arg "Sm2.encode_private_key_encrypted_pem iv";
  let dek = pbkdf2_sm3 ~password ~salt ~iterations ~dk_len:16 in
  let inner = Bytes.of_string (encode_private_key_pkcs8_der key) in
  let encrypted = Sm4.Cbc.encrypt_pkcs7 ~key:dek ~iv inner in
  let pbkdf2_params =
    Der.sequence
      [
        Der.octet_string (Bytes.unsafe_to_string salt);
        Der.integer_int iterations;
        Der.integer_int 16;
        Der.sequence [Der.oid Der.oid_hmac_sm3; Der.null];
      ]
  in
  let pbes2_params =
    Der.sequence
      [
        Der.sequence [Der.oid Der.oid_pbkdf2; pbkdf2_params];
        Der.sequence [Der.oid Der.oid_sm4_cbc; Der.octet_string (Bytes.unsafe_to_string iv)];
      ]
  in
  let der =
    Der.sequence
      [
        Der.sequence [Der.oid Der.oid_pbes2; pbes2_params];
        Der.octet_string (Bytes.unsafe_to_string encrypted);
      ]
  in
  Pem.encode "ENCRYPTED PRIVATE KEY" der

let decode_private_key_encrypted_pem ~password pem =
  let open Der in
  try
    let der = match Pem.decode "ENCRYPTED PRIVATE KEY" pem with Some x -> x | None -> raise Exit in
    let r = make_reader der in
    let seq = expect_tag 0x30 (read_tlv r) in
    if remaining r <> 0 then raise Exit;
    let r2 = make_reader seq in
    let alg = expect_tag 0x30 (read_tlv r2) in
    let ra = make_reader alg in
    if expect_tag 0x06 (read_tlv ra) <> oid_pbes2 then raise Exit;
    let pbes2 = expect_tag 0x30 (read_tlv ra) in
    if remaining ra <> 0 then raise Exit;
    let rp = make_reader pbes2 in
    let kdf = expect_tag 0x30 (read_tlv rp) in
    let rk = make_reader kdf in
    if expect_tag 0x06 (read_tlv rk) <> oid_pbkdf2 then raise Exit;
    let kdf_params = expect_tag 0x30 (read_tlv rk) in
    if remaining rk <> 0 then raise Exit;
    let rkp = make_reader kdf_params in
    let salt = Bytes.of_string (expect_tag 0x04 (read_tlv rkp)) in
    let iterations = parse_integer_u256 (expect_tag 0x02 (read_tlv rkp)) |> U256.to_bytes_be in
    let int_of_bytes b =
      let v = ref 0 in
      Bytes.iter (fun c -> v := (!v lsl 8) lor Char.code c) b;
      !v
    in
    let iterations = int_of_bytes iterations in
    let key_len = parse_integer_u256 (expect_tag 0x02 (read_tlv rkp)) |> U256.to_bytes_be |> int_of_bytes in
    let prf = expect_tag 0x30 (read_tlv rkp) in
    let rr = make_reader prf in
    if expect_tag 0x06 (read_tlv rr) <> oid_hmac_sm3 then raise Exit;
    ignore (expect_tag 0x05 (read_tlv rr));
    if remaining rr <> 0 || remaining rkp <> 0 then raise Exit;
    let enc = expect_tag 0x30 (read_tlv rp) in
    let re = make_reader enc in
    if expect_tag 0x06 (read_tlv re) <> oid_sm4_cbc then raise Exit;
    let iv = Bytes.of_string (expect_tag 0x04 (read_tlv re)) in
    if remaining re <> 0 || remaining rp <> 0 then raise Exit;
    let encrypted = Bytes.of_string (expect_tag 0x04 (read_tlv r2)) in
    if remaining r2 <> 0 then raise Exit;
    let dek = pbkdf2_sm3 ~password ~salt ~iterations ~dk_len:key_len in
    match Sm4.Cbc.decrypt_pkcs7 ~key:dek ~iv encrypted with
    | None -> None
    | Some plain -> decode_private_key_pkcs8_der (Bytes.unsafe_to_string plain)
  with Exit -> None
