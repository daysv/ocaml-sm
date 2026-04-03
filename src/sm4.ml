type round_keys = int32 array

let sbox =
  [|
    0xd6; 0x90; 0xe9; 0xfe; 0xcc; 0xe1; 0x3d; 0xb7; 0x16; 0xb6; 0x14; 0xc2; 0x28; 0xfb; 0x2c; 0x05;
    0x2b; 0x67; 0x9a; 0x76; 0x2a; 0xbe; 0x04; 0xc3; 0xaa; 0x44; 0x13; 0x26; 0x49; 0x86; 0x06; 0x99;
    0x9c; 0x42; 0x50; 0xf4; 0x91; 0xef; 0x98; 0x7a; 0x33; 0x54; 0x0b; 0x43; 0xed; 0xcf; 0xac; 0x62;
    0xe4; 0xb3; 0x1c; 0xa9; 0xc9; 0x08; 0xe8; 0x95; 0x80; 0xdf; 0x94; 0xfa; 0x75; 0x8f; 0x3f; 0xa6;
    0x47; 0x07; 0xa7; 0xfc; 0xf3; 0x73; 0x17; 0xba; 0x83; 0x59; 0x3c; 0x19; 0xe6; 0x85; 0x4f; 0xa8;
    0x68; 0x6b; 0x81; 0xb2; 0x71; 0x64; 0xda; 0x8b; 0xf8; 0xeb; 0x0f; 0x4b; 0x70; 0x56; 0x9d; 0x35;
    0x1e; 0x24; 0x0e; 0x5e; 0x63; 0x58; 0xd1; 0xa2; 0x25; 0x22; 0x7c; 0x3b; 0x01; 0x21; 0x78; 0x87;
    0xd4; 0x00; 0x46; 0x57; 0x9f; 0xd3; 0x27; 0x52; 0x4c; 0x36; 0x02; 0xe7; 0xa0; 0xc4; 0xc8; 0x9e;
    0xea; 0xbf; 0x8a; 0xd2; 0x40; 0xc7; 0x38; 0xb5; 0xa3; 0xf7; 0xf2; 0xce; 0xf9; 0x61; 0x15; 0xa1;
    0xe0; 0xae; 0x5d; 0xa4; 0x9b; 0x34; 0x1a; 0x55; 0xad; 0x93; 0x32; 0x30; 0xf5; 0x8c; 0xb1; 0xe3;
    0x1d; 0xf6; 0xe2; 0x2e; 0x82; 0x66; 0xca; 0x60; 0xc0; 0x29; 0x23; 0xab; 0x0d; 0x53; 0x4e; 0x6f;
    0xd5; 0xdb; 0x37; 0x45; 0xde; 0xfd; 0x8e; 0x2f; 0x03; 0xff; 0x6a; 0x72; 0x6d; 0x6c; 0x5b; 0x51;
    0x8d; 0x1b; 0xaf; 0x92; 0xbb; 0xdd; 0xbc; 0x7f; 0x11; 0xd9; 0x5c; 0x41; 0x1f; 0x10; 0x5a; 0xd8;
    0x0a; 0xc1; 0x31; 0x88; 0xa5; 0xcd; 0x7b; 0xbd; 0x2d; 0x74; 0xd0; 0x12; 0xb8; 0xe5; 0xb4; 0xb0;
    0x89; 0x69; 0x97; 0x4a; 0x0c; 0x96; 0x77; 0x7e; 0x65; 0xb9; 0xf1; 0x09; 0xc5; 0x6e; 0xc6; 0x84;
    0x18; 0xf0; 0x7d; 0xec; 0x3a; 0xdc; 0x4d; 0x20; 0x79; 0xee; 0x5f; 0x3e; 0xd7; 0xcb; 0x39; 0x48;
  |]

let fk = [| 0xa3b1bac6l; 0x56aa3350l; 0x677d9197l; 0xb27022dcl |]

let ck =
  [|
    0x00070e15l; 0x1c232a31l; 0x383f464dl; 0x545b6269l; 0x70777e85l; 0x8c939aa1l; 0xa8afb6bdl; 0xc4cbd2d9l;
    0xe0e7eef5l; 0xfc030a11l; 0x181f262dl; 0x343b4249l; 0x50575e65l; 0x6c737a81l; 0x888f969dl; 0xa4abb2b9l;
    0xc0c7ced5l; 0xdce3eaf1l; 0xf8ff060dl; 0x141b2229l; 0x30373e45l; 0x4c535a61l; 0x686f767dl; 0x848b9299l;
    0xa0a7aeb5l; 0xbcc3cad1l; 0xd8dfe6edl; 0xf4fb0209l; 0x10171e25l; 0x2c333a41l; 0x484f565dl; 0x646b7279l;
  |]

let rotl x n =
  let open Int32 in
  logor (shift_left x n) (shift_right_logical x (32 - n))

let tau a =
  let open Int32 in
  let byte idx = Array.get sbox (to_int (logand (shift_right_logical a (idx * 8)) 0xffl)) in
  logor
    (shift_left (of_int (byte 3)) 24)
    (logor
       (shift_left (of_int (byte 2)) 16)
       (logor (shift_left (of_int (byte 1)) 8) (of_int (byte 0))))

let l x =
  Int32.logxor x
    (Int32.logxor (rotl x 2) (Int32.logxor (rotl x 10) (Int32.logxor (rotl x 18) (rotl x 24))))

let l' x = Int32.logxor x (Int32.logxor (rotl x 13) (rotl x 23))
let t x = l (tau x)
let t' x = l' (tau x)

let get_u32_be b off =
  let open Int32 in
  logor
    (shift_left (of_int (Char.code (Bytes.get b off))) 24)
    (logor
       (shift_left (of_int (Char.code (Bytes.get b (off + 1)))) 16)
       (logor
          (shift_left (of_int (Char.code (Bytes.get b (off + 2)))) 8)
          (of_int (Char.code (Bytes.get b (off + 3))))))

let set_u32_be b off x =
  let open Int32 in
  Bytes.set b off (Char.chr (to_int (shift_right_logical x 24) land 0xff));
  Bytes.set b (off + 1) (Char.chr (to_int (shift_right_logical x 16) land 0xff));
  Bytes.set b (off + 2) (Char.chr (to_int (shift_right_logical x 8) land 0xff));
  Bytes.set b (off + 3) (Char.chr (to_int x land 0xff))

let key_schedule key =
  if Bytes.length key <> 16 then invalid_arg "Sm4.key_schedule";
  let mk = Array.init 4 (fun i -> get_u32_be key (i * 4)) in
  let k = Array.make 36 0l in
  for i = 0 to 3 do
    k.(i) <- Int32.logxor mk.(i) fk.(i)
  done;
  let rk = Array.make 32 0l in
  for i = 0 to 31 do
    let x =
      Int32.logxor k.(i + 1)
        (Int32.logxor k.(i + 2) (Int32.logxor k.(i + 3) ck.(i)))
    in
    k.(i + 4) <- Int32.logxor k.(i) (t' x);
    rk.(i) <- k.(i + 4)
  done;
  rk

let crypt_block rks block decrypt =
  if Bytes.length block <> 16 then invalid_arg "Sm4.crypt_block";
  let x = Array.make 36 0l in
  for i = 0 to 3 do
    x.(i) <- get_u32_be block (i * 4)
  done;
  for i = 0 to 31 do
    let rk = if decrypt then rks.(31 - i) else rks.(i) in
    let v =
      Int32.logxor x.(i + 1)
        (Int32.logxor x.(i + 2) (Int32.logxor x.(i + 3) rk))
    in
    x.(i + 4) <- Int32.logxor x.(i) (t v)
  done;
  let out = Bytes.create 16 in
  set_u32_be out 0 x.(35);
  set_u32_be out 4 x.(34);
  set_u32_be out 8 x.(33);
  set_u32_be out 12 x.(32);
  out

let encrypt_block rks block = crypt_block rks block false
let decrypt_block rks block = crypt_block rks block true
let encrypt_block_with_key key block = encrypt_block (key_schedule key) block
let decrypt_block_with_key key block = decrypt_block (key_schedule key) block

let xor_block_into dst dst_off a a_off b b_off len =
  for i = 0 to len - 1 do
    Bytes.set dst (dst_off + i) (Char.chr (Char.code (Bytes.get a (a_off + i)) lxor Char.code (Bytes.get b (b_off + i))))
  done

let xor_bytes a b =
  let len = Bytes.length a in
  let out = Bytes.create len in
  xor_block_into out 0 a 0 b 0 len;
  out

let require_len who expected got =
  if got <> expected then invalid_arg who

module Cbc = struct
  let encrypt_no_pad ~key ~iv plaintext =
    require_len "Sm4.Cbc.encrypt_no_pad iv" 16 (Bytes.length iv);
    if Bytes.length plaintext mod 16 <> 0 then invalid_arg "Sm4.Cbc.encrypt_no_pad";
    let rks = key_schedule key in
    let prev = ref (Bytes.copy iv) in
    let out = Bytes.create (Bytes.length plaintext) in
    for off = 0 to (Bytes.length plaintext / 16) - 1 do
      let block = Bytes.sub plaintext (off * 16) 16 in
      let xored = xor_bytes block !prev in
      let cipher = encrypt_block rks xored in
      Bytes.blit cipher 0 out (off * 16) 16;
      prev := cipher
    done;
    out

  let decrypt_no_pad ~key ~iv ciphertext =
    require_len "Sm4.Cbc.decrypt_no_pad iv" 16 (Bytes.length iv);
    if Bytes.length ciphertext mod 16 <> 0 then invalid_arg "Sm4.Cbc.decrypt_no_pad";
    let rks = key_schedule key in
    let prev = ref (Bytes.copy iv) in
    let out = Bytes.create (Bytes.length ciphertext) in
    for off = 0 to (Bytes.length ciphertext / 16) - 1 do
      let block = Bytes.sub ciphertext (off * 16) 16 in
      let plain = xor_bytes (decrypt_block rks block) !prev in
      Bytes.blit plain 0 out (off * 16) 16;
      prev := block
    done;
    out

  let encrypt_pkcs7 ~key ~iv plaintext =
    let pad = 16 - (Bytes.length plaintext mod 16) in
    let padded = Bytes.make (Bytes.length plaintext + pad) (Char.chr pad) in
    Bytes.blit plaintext 0 padded 0 (Bytes.length plaintext);
    encrypt_no_pad ~key ~iv padded

  let decrypt_pkcs7 ~key ~iv ciphertext =
    let padded = decrypt_no_pad ~key ~iv ciphertext in
    if Bytes.length padded = 0 then None
    else
      let pad = Char.code (Bytes.get padded (Bytes.length padded - 1)) in
      if pad = 0 || pad > 16 || pad > Bytes.length padded then None
      else
        let ok = ref true in
        for i = Bytes.length padded - pad to Bytes.length padded - 1 do
          if Char.code (Bytes.get padded i) <> pad then ok := false
        done;
        if not !ok then None else Some (Bytes.sub padded 0 (Bytes.length padded - pad))
end

module Ctr = struct
  let incr_counter ctr =
    let rec loop i =
      if i < 0 then ()
      else
        let v = (Char.code (Bytes.get ctr i) + 1) land 0xff in
        Bytes.set ctr i (Char.chr v);
        if v = 0 then loop (i - 1)
    in
    loop 15

  let crypt ~key ~iv input =
    require_len "Sm4.Ctr.crypt iv" 16 (Bytes.length iv);
    let rks = key_schedule key in
    let ctr = Bytes.copy iv in
    let out = Bytes.create (Bytes.length input) in
    let blocks = (Bytes.length input + 15) / 16 in
    for i = 0 to blocks - 1 do
      let stream = encrypt_block rks ctr in
      let take = min 16 (Bytes.length input - (i * 16)) in
      xor_block_into out (i * 16) input (i * 16) stream 0 take;
      incr_counter ctr
    done;
    out
end

let ctr_crypt_full = Ctr.crypt

module Gcm = struct
  type encrypted = {
    ciphertext : bytes;
    tag : bytes;
  }

  let int64_xor = Int64.logxor
  let int64_and = Int64.logand
  let int64_or = Int64.logor

  let of_block b =
    let read off =
      let acc = ref 0L in
      for i = 0 to 7 do
        acc := int64_or (Int64.shift_left !acc 8) (Int64.of_int (Char.code (Bytes.get b (off + i))))
      done;
      !acc
    in
    (read 0, read 8)

  let to_block (hi, lo) =
    let out = Bytes.create 16 in
    let write off v =
      Bytes.set out off (Char.chr (Int64.to_int (Int64.shift_right_logical v 56) land 0xff));
      Bytes.set out (off + 1) (Char.chr (Int64.to_int (Int64.shift_right_logical v 48) land 0xff));
      Bytes.set out (off + 2) (Char.chr (Int64.to_int (Int64.shift_right_logical v 40) land 0xff));
      Bytes.set out (off + 3) (Char.chr (Int64.to_int (Int64.shift_right_logical v 32) land 0xff));
      Bytes.set out (off + 4) (Char.chr (Int64.to_int (Int64.shift_right_logical v 24) land 0xff));
      Bytes.set out (off + 5) (Char.chr (Int64.to_int (Int64.shift_right_logical v 16) land 0xff));
      Bytes.set out (off + 6) (Char.chr (Int64.to_int (Int64.shift_right_logical v 8) land 0xff));
      Bytes.set out (off + 7) (Char.chr (Int64.to_int v land 0xff))
    in
    write 0 hi;
    write 8 lo;
    out

  let xor128 (a1, a2) (b1, b2) = (int64_xor a1 b1, int64_xor a2 b2)

  let shift_right1 (hi, lo) =
    let carry = int64_and hi 1L in
    let hi' = Int64.shift_right_logical hi 1 in
    let lo' = int64_or (Int64.shift_right_logical lo 1) (Int64.shift_left carry 63) in
    (hi', lo')

  let gf_mul x y =
    let z = ref (0L, 0L) in
    let v = ref y in
    let r = of_block (Bytes.of_string "\xe1\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000") in
    for i = 0 to 127 do
      let xi =
        if i < 64 then Int64.(logand (shift_right_logical (fst x) (63 - i)) 1L = 1L)
        else Int64.(logand (shift_right_logical (snd x) (127 - i)) 1L = 1L)
      in
      if xi then z := xor128 !z !v;
      let lsb = Int64.(logand (snd !v) 1L = 1L) in
      v := shift_right1 !v;
      if lsb then v := xor128 !v r
    done;
    !z

  let ghash h aad ciphertext =
    let y = ref (0L, 0L) in
    let update_bytes bytes =
      let blocks = (Bytes.length bytes + 15) / 16 in
      for i = 0 to blocks - 1 do
        let block = Bytes.make 16 '\000' in
        let take = min 16 (Bytes.length bytes - (i * 16)) in
        Bytes.blit bytes (i * 16) block 0 take;
        y := gf_mul (xor128 !y (of_block block)) h
      done
    in
    update_bytes aad;
    update_bytes ciphertext;
    let lens = Bytes.make 16 '\000' in
    let set_u64 off v =
      Bytes.set lens off (Char.chr (Int64.to_int (Int64.shift_right_logical v 56) land 0xff));
      Bytes.set lens (off + 1) (Char.chr (Int64.to_int (Int64.shift_right_logical v 48) land 0xff));
      Bytes.set lens (off + 2) (Char.chr (Int64.to_int (Int64.shift_right_logical v 40) land 0xff));
      Bytes.set lens (off + 3) (Char.chr (Int64.to_int (Int64.shift_right_logical v 32) land 0xff));
      Bytes.set lens (off + 4) (Char.chr (Int64.to_int (Int64.shift_right_logical v 24) land 0xff));
      Bytes.set lens (off + 5) (Char.chr (Int64.to_int (Int64.shift_right_logical v 16) land 0xff));
      Bytes.set lens (off + 6) (Char.chr (Int64.to_int (Int64.shift_right_logical v 8) land 0xff));
      Bytes.set lens (off + 7) (Char.chr (Int64.to_int v land 0xff))
    in
    set_u64 0 Int64.(mul (of_int (Bytes.length aad)) 8L);
    set_u64 8 Int64.(mul (of_int (Bytes.length ciphertext)) 8L);
    gf_mul (xor128 !y (of_block lens)) h

  let j0 h iv =
    if Bytes.length iv = 12 then
      let j = Bytes.make 16 '\000' in
      Bytes.blit iv 0 j 0 12;
      Bytes.set j 15 '\001';
      j
    else
      let s = to_block (ghash h Bytes.empty iv) in
      s

  let incr32 ctr =
    let n =
      Int32.logor
        (Int32.shift_left (Int32.of_int (Char.code (Bytes.get ctr 12))) 24)
        (Int32.logor
           (Int32.shift_left (Int32.of_int (Char.code (Bytes.get ctr 13))) 16)
           (Int32.logor
              (Int32.shift_left (Int32.of_int (Char.code (Bytes.get ctr 14))) 8)
              (Int32.of_int (Char.code (Bytes.get ctr 15)))))
    in
    let n = Int32.add n 1l in
    Bytes.set ctr 12 (Char.chr (Int32.to_int (Int32.shift_right_logical n 24) land 0xff));
    Bytes.set ctr 13 (Char.chr (Int32.to_int (Int32.shift_right_logical n 16) land 0xff));
    Bytes.set ctr 14 (Char.chr (Int32.to_int (Int32.shift_right_logical n 8) land 0xff));
    Bytes.set ctr 15 (Char.chr (Int32.to_int n land 0xff))

  let crypt ~rks ~ctr input =
    let ctr = Bytes.copy ctr in
    incr32 ctr;
    let out = Bytes.create (Bytes.length input) in
    let blocks = (Bytes.length input + 15) / 16 in
    for i = 0 to blocks - 1 do
      let stream = encrypt_block rks ctr in
      let take = min 16 (Bytes.length input - (i * 16)) in
      xor_block_into out (i * 16) input (i * 16) stream 0 take;
      incr32 ctr
    done;
    out

  let encrypt ~key ~iv ?(aad = Bytes.empty) plaintext =
    let rks = key_schedule key in
    let h = of_block (encrypt_block rks (Bytes.make 16 '\000')) in
    let j0 = j0 h iv in
    let ciphertext = crypt ~rks ~ctr:j0 plaintext in
    let s = ghash h aad ciphertext |> to_block in
    let e_j0 = encrypt_block rks j0 in
    let tag = xor_bytes s e_j0 in
    { ciphertext; tag }

  let decrypt ~key ~iv ?aad ~tag ciphertext =
    let aad = match aad with Some x -> x | None -> Bytes.empty in
    if Bytes.length tag <> 16 then invalid_arg "Sm4.Gcm.decrypt tag";
    let rks = key_schedule key in
    let h = of_block (encrypt_block rks (Bytes.make 16 '\000')) in
    let j0 = j0 h iv in
    let s = ghash h aad ciphertext |> to_block in
    let expected_tag = xor_bytes s (encrypt_block rks j0) in
    if not (Bytes.equal expected_tag tag) then None
    else Some (crypt ~rks ~ctr:j0 ciphertext)
end

module Stream = struct
  module Cbc_encrypt = struct
    type padding = [ `No_padding | `Pkcs7 ]

    type t = {
      key : bytes;
      iv : bytes;
      padding : padding;
      buf : Buffer.t;
      mutable closed : bool;
    }

    let init ~key ~iv ~padding =
      require_len "Sm4.Stream.Cbc_encrypt.init iv" 16 (Bytes.length iv);
      { key = Bytes.copy key; iv = Bytes.copy iv; padding; buf = Buffer.create 128; closed = false }

    let update t bytes =
      if t.closed then invalid_arg "Sm4.Stream.Cbc_encrypt.update";
      Buffer.add_bytes t.buf bytes

    let finalize t =
      if t.closed then None
      else (
        t.closed <- true;
        let input = Bytes.of_string (Buffer.contents t.buf) in
        Some
          (match t.padding with
           | `No_padding -> Cbc.encrypt_no_pad ~key:t.key ~iv:t.iv input
           | `Pkcs7 -> Cbc.encrypt_pkcs7 ~key:t.key ~iv:t.iv input))
  end

  module Cbc_decrypt = struct
    type padding = [ `No_padding | `Pkcs7 ]

    type t = {
      key : bytes;
      iv : bytes;
      padding : padding;
      buf : Buffer.t;
      mutable closed : bool;
    }

    let init ~key ~iv ~padding =
      require_len "Sm4.Stream.Cbc_decrypt.init iv" 16 (Bytes.length iv);
      { key = Bytes.copy key; iv = Bytes.copy iv; padding; buf = Buffer.create 128; closed = false }

    let update t bytes =
      if t.closed then invalid_arg "Sm4.Stream.Cbc_decrypt.update";
      Buffer.add_bytes t.buf bytes

    let finalize t =
      if t.closed then None
      else (
        t.closed <- true;
        let input = Bytes.of_string (Buffer.contents t.buf) in
        match t.padding with
        | `No_padding -> Some (Cbc.decrypt_no_pad ~key:t.key ~iv:t.iv input)
        | `Pkcs7 -> Cbc.decrypt_pkcs7 ~key:t.key ~iv:t.iv input)
  end

  module Ctr = struct
    type t = {
      key : bytes;
      iv : bytes;
      input : Buffer.t;
      mutable produced : int;
    }

    let init ~key ~iv =
      require_len "Sm4.Stream.Ctr.init iv" 16 (Bytes.length iv);
      { key = Bytes.copy key; iv = Bytes.copy iv; input = Buffer.create 128; produced = 0 }

    let update t chunk =
      Buffer.add_bytes t.input chunk;
      let all_out = ctr_crypt_full ~key:t.key ~iv:t.iv (Bytes.of_string (Buffer.contents t.input)) in
      let delta = Bytes.sub all_out t.produced (Bytes.length all_out - t.produced) in
      t.produced <- Bytes.length all_out;
      delta
  end

  module Gcm_encrypt = struct
    type t = {
      key : bytes;
      iv : bytes;
      aad : Buffer.t;
      data : Buffer.t;
      mutable closed : bool;
    }

    let init ~key ~iv = { key = Bytes.copy key; iv = Bytes.copy iv; aad = Buffer.create 64; data = Buffer.create 128; closed = false }
    let add_aad t bytes = if t.closed then invalid_arg "Sm4.Stream.Gcm_encrypt.add_aad" else Buffer.add_bytes t.aad bytes
    let update t bytes = if t.closed then invalid_arg "Sm4.Stream.Gcm_encrypt.update" else Buffer.add_bytes t.data bytes
    let finalize t =
      if t.closed then invalid_arg "Sm4.Stream.Gcm_encrypt.finalize";
      t.closed <- true;
      Gcm.encrypt ~key:t.key ~iv:t.iv ~aad:(Bytes.of_string (Buffer.contents t.aad)) (Bytes.of_string (Buffer.contents t.data))
  end

  module Gcm_decrypt = struct
    type t = {
      key : bytes;
      iv : bytes;
      aad : Buffer.t;
      data : Buffer.t;
      mutable closed : bool;
    }

    let init ~key ~iv = { key = Bytes.copy key; iv = Bytes.copy iv; aad = Buffer.create 64; data = Buffer.create 128; closed = false }
    let add_aad t bytes = if t.closed then invalid_arg "Sm4.Stream.Gcm_decrypt.add_aad" else Buffer.add_bytes t.aad bytes
    let update t bytes = if t.closed then invalid_arg "Sm4.Stream.Gcm_decrypt.update" else Buffer.add_bytes t.data bytes
    let finalize t ~tag =
      if t.closed then None
      else (
        t.closed <- true;
        Gcm.decrypt ~key:t.key ~iv:t.iv ~aad:(Bytes.of_string (Buffer.contents t.aad)) ~tag (Bytes.of_string (Buffer.contents t.data)))
  end
end
