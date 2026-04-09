type round_keys = int array

let sbox = [|
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

let fk = [| 0xa3b1bac6; 0x56aa3350; 0x677d9197; 0xb27022dc |]
let ck = [|
  0x00070e15; 0x1c232a31; 0x383f464d; 0x545b6269; 0x70777e85; 0x8c939aa1; 0xa8afb6bd; 0xc4cbd2d9;
  0xe0e7eef5; 0xfc030a11; 0x181f262d; 0x343b4249; 0x50575e65; 0x6c737a81; 0x888f969d; 0xa4abb2b9;
  0xc0c7ced5; 0xdce3eaf1; 0xf8ff060d; 0x141b2229; 0x30373e45; 0x4c535a61; 0x686f767d; 0x848b9299;
  0xa0a7aeb5; 0xbcc3cad1; 0xd8dfe6ed; 0xf4fb0209; 0x10171e25; 0x2c333a41; 0x484f565d; 0x646b7279;
|]

let rotl32 x n = let x = x land 0xffffffff in ((x lsl n) land 0xffffffff) lor (x lsr (32 - n)) [@@inline always]

let t_table =
  let tbl = Array.make 1024 0 in
  for i = 0 to 255 do
    let s = sbox.(i) in
    let lt = s lxor (rotl32 s 2) lxor (rotl32 s 10) lxor (rotl32 s 18) lxor (rotl32 s 24) in
    tbl.(i) <- lt; tbl.(i + 256) <- rotl32 lt 8; tbl.(i + 512) <- rotl32 lt 16; tbl.(i + 768) <- rotl32 lt 24
  done; tbl

let get_u32_be b off =
  ((Char.code (Bytes.get b off)) lsl 24) lor ((Char.code (Bytes.get b (off + 1))) lsl 16) lor
  ((Char.code (Bytes.get b (off + 2))) lsl 8) lor (Char.code (Bytes.get b (off + 3)))

let set_u32_be b off v =
  let v = v land 0xffffffff in
  Bytes.set b off (Char.unsafe_chr (v lsr 24));
  Bytes.set b (off + 1) (Char.unsafe_chr ((v lsr 16) land 0xff));
  Bytes.set b (off + 2) (Char.unsafe_chr ((v lsr 8) land 0xff));
  Bytes.set b (off + 3) (Char.unsafe_chr (v land 0xff))

let key_schedule key =
  let k = Array.make 36 0 in
  for i = 0 to 3 do k.(i) <- (get_u32_be key (i * 4)) lxor fk.(i) done;
  let rk = Array.make 32 0 in
  for i = 0 to 31 do
    let x = k.(i+1) lxor k.(i+2) lxor k.(i+3) lxor ck.(i) in
    let s = (sbox.((x lsr 24) land 0xff) lsl 24) lor (sbox.((x lsr 16) land 0xff) lsl 16) lor
            (sbox.((x lsr 8) land 0xff) lsl 8) lor (sbox.(x land 0xff)) in
    let t_prime = s lxor (rotl32 s 13) lxor (rotl32 s 23) in
    k.(i+4) <- k.(i) lxor t_prime; rk.(i) <- k.(i+4)
  done; rk

let crypt_block_in_place rks src src_off dst dst_off is_decrypt =
  let x = ref (get_u32_be src src_off) in
  let y = ref (get_u32_be src (src_off + 4)) in
  let z = ref (get_u32_be src (src_off + 8)) in
  let w = ref (get_u32_be src (src_off + 12)) in
  for i = 0 to 31 do
    let rk = if is_decrypt then rks.(31 - i) else rks.(i) in
    let v = !y lxor !z lxor !w lxor rk in
    let lt = t_table.((v lsr 24) land 0xff + 768) lxor t_table.((v lsr 16) land 0xff + 512) lxor
             t_table.((v lsr 8) land 0xff + 256) lxor t_table.(v land 0xff) in
    let nx = !x lxor lt in x := !y; y := !z; z := !w; w := nx
  done;
  set_u32_be dst dst_off !w; set_u32_be dst (dst_off + 4) !z; set_u32_be dst (dst_off + 8) !y; set_u32_be dst (dst_off + 12) !x

let encrypt_block rks block = let out = Bytes.create 16 in crypt_block_in_place rks block 0 out 0 false; out
let decrypt_block rks block = let out = Bytes.create 16 in crypt_block_in_place rks block 0 out 0 true; out
let encrypt_block_with_key key block = encrypt_block (key_schedule key) block
let decrypt_block_with_key key block = decrypt_block (key_schedule key) block

let xor_block_into dst dst_off a a_off b b_off len =
  for i = 0 to len - 1 do
    Bytes.unsafe_set dst (dst_off + i) (Char.unsafe_chr ((Char.code (Bytes.unsafe_get a (a_off + i))) lxor (Char.code (Bytes.unsafe_get b (b_off + i)))))
  done

let incr_counter_be ctr =
  let rec loop i = if i >= 0 then (let v = (Char.code (Bytes.get ctr i) + 1) land 0xff in Bytes.set ctr i (Char.chr v); if v = 0 then loop (i - 1)) in loop 15

module Cbc = struct
  let encrypt_no_pad ~key ~iv plaintext =
    let rks = key_schedule key in let out = Bytes.create (Bytes.length plaintext) in let prev = ref (Bytes.copy iv) in
    for i = 0 to (Bytes.length plaintext / 16) - 1 do
      let off = i * 16 in xor_block_into out off plaintext off !prev 0 16;
      crypt_block_in_place rks out off out off false; prev := Bytes.sub out off 16
    done; out
  let decrypt_no_pad ~key ~iv ciphertext =
    let rks = key_schedule key in let out = Bytes.create (Bytes.length ciphertext) in let prev = ref (Bytes.copy iv) in
    for i = 0 to (Bytes.length ciphertext / 16) - 1 do
      let off = i * 16 in let block = Bytes.sub ciphertext off 16 in
      crypt_block_in_place rks block 0 out off true; xor_block_into out off out off !prev 0 16; prev := block
    done; out
  let encrypt_pkcs7 ~key ~iv plaintext =
    let pad = 16 - (Bytes.length plaintext mod 16) in let padded = Bytes.make (Bytes.length plaintext + pad) (Char.chr pad) in
    Bytes.blit plaintext 0 padded 0 (Bytes.length plaintext); encrypt_no_pad ~key ~iv padded
  let decrypt_pkcs7 ~key ~iv ciphertext =
    let p = decrypt_no_pad ~key ~iv ciphertext in let len = Bytes.length p in
    if len = 0 then None else let pad = Char.code (Bytes.get p (len - 1)) in
    if pad < 1 || pad > 16 || pad > len then None else Some (Bytes.sub p 0 (len - pad))
end

module Ctr = struct
  let crypt ~key ~iv input =
    let rks = key_schedule key in let ctr = Bytes.copy iv in let out = Bytes.create (Bytes.length input) in
    for i = 0 to (Bytes.length input + 15) / 16 - 1 do
      let off = i * 16 in let take = min 16 (Bytes.length input - off) in
      let stream = encrypt_block rks ctr in xor_block_into out off input off stream 0 take; incr_counter_be ctr
    done; out
end

module Gcm = struct
  type encrypted = { ciphertext : bytes; tag : bytes }
  let incr32 b = let n = (get_u32_be b 12 + 1) land 0xffffffff in set_u32_be b 12 n
  let gf_mul (x_hi, x_lo) (y_hi, y_lo) =
    let z_hi = ref 0L in let z_lo = ref 0L in let v_hi = ref y_hi in let v_lo = ref y_lo in
    for i = 0 to 127 do
      let bit = if i < 64 then Int64.(logand (shift_right_logical x_hi (63-i)) 1L) else Int64.(logand (shift_right_logical x_lo (127-i)) 1L) in
      if bit = 1L then (z_hi := Int64.logxor !z_hi !v_hi; z_lo := Int64.logxor !z_lo !v_lo);
      let lsb = Int64.logand !v_lo 1L = 1L in
      let carry = Int64.(shift_left (logand !v_hi 1L) 63) in
      v_hi := Int64.shift_right_logical !v_hi 1; v_lo := Int64.(logor (shift_right_logical !v_lo 1) carry);
      if lsb then v_hi := Int64.logxor !v_hi 0xE100000000000000L
    done; (!z_hi, !z_lo)
  let ghash h aad ciphertext =
    let y = ref (0L, 0L) in
    let update data =
      let n = Bytes.length data in let i = ref 0 in
      while !i < n do
        let b = Bytes.make 16 '\000' in let take = min 16 (n - !i) in Bytes.blit data !i b 0 take;
        let r off = let v = ref 0L in for j = 0 to 7 do v := Int64.(logor (shift_left !v 8) (of_int (Char.code (Bytes.get b (off + j))))) done; !v in
        let b_v = (r 0, r 8) in y := gf_mul (Int64.logxor (fst !y) (fst b_v), Int64.logxor (snd !y) (snd b_v)) h;
        i := !i + 16
      done in
    update aad; update ciphertext;
    let lens = Bytes.create 16 in
    let w off v = for i = 0 to 7 do Bytes.set lens (off+i) (Char.chr (Int64.(to_int (shift_right_logical v ((7-i)*8)) land 0xff))) done in
    w 0 Int64.(mul (of_int (Bytes.length aad)) 8L); w 8 Int64.(mul (of_int (Bytes.length ciphertext)) 8L);
    let r off = let v = ref 0L in for j = 0 to 7 do v := Int64.(logor (shift_left !v 8) (of_int (Char.code (Bytes.get lens (off + j))))) done; !v in
    let b_l = (r 0, r 8) in gf_mul (Int64.logxor (fst !y) (fst b_l), Int64.logxor (snd !y) (snd b_l)) h
  let gcm_crypt rks j0 input =
    let ctr = Bytes.copy j0 in let out = Bytes.create (Bytes.length input) in
    for i = 0 to (Bytes.length input + 15) / 16 - 1 do
      incr32 ctr; let off = i * 16 in let take = min 16 (Bytes.length input - off) in
      let s = encrypt_block rks ctr in xor_block_into out off input off s 0 take
    done; out
  let encrypt ~key ~iv ?(aad = Bytes.empty) plaintext =
    let rks = key_schedule key in let h_b = encrypt_block rks (Bytes.make 16 '\000') in
    let r b off = let v = ref 0L in for i = 0 to 7 do v := Int64.(logor (shift_left !v 8) (of_int (Char.code (Bytes.get b (off + i))))) done; !v in
    let h = (r h_b 0, r h_b 8) in
    let j0 = if Bytes.length iv = 12 then (let b = Bytes.make 16 '\000' in Bytes.blit iv 0 b 0 12; Bytes.set b 15 '\001'; b) 
             else let g = ghash h Bytes.empty iv in let b = Bytes.create 16 in 
             let w off v = for i = 0 to 7 do Bytes.set b (off+i) (Char.chr (Int64.(to_int (shift_right_logical v ((7-i)*8)) land 0xff))) done in
             w 0 (fst g); w 8 (snd g); b in
    let ciphertext = gcm_crypt rks j0 plaintext in
    let s = ghash h aad ciphertext in let ej0 = encrypt_block rks j0 in
    let tag = Bytes.create 16 in
    let w b off v = for i = 0 to 7 do Bytes.set b (off+i) (Char.chr (Int64.(to_int (shift_right_logical v ((7-i)*8)) land 0xff))) done in
    let s_b = Bytes.create 16 in w s_b 0 (fst s); w s_b 8 (snd s);
    xor_block_into tag 0 s_b 0 ej0 0 16; { ciphertext; tag }
  let decrypt ~key ~iv ?(aad = Bytes.empty) ~tag ciphertext =
    let rks = key_schedule key in let h_b = encrypt_block rks (Bytes.make 16 '\000') in
    let r b off = let v = ref 0L in for i = 0 to 7 do v := Int64.(logor (shift_left !v 8) (of_int (Char.code (Bytes.get b (off + i))))) done; !v in
    let h = (r h_b 0, r h_b 8) in
    let j0 = if Bytes.length iv = 12 then (let b = Bytes.make 16 '\000' in Bytes.blit iv 0 b 0 12; Bytes.set b 15 '\001'; b) 
             else let g = ghash h Bytes.empty iv in let b = Bytes.create 16 in 
             let w off v = for i = 0 to 7 do Bytes.set b (off+i) (Char.chr (Int64.(to_int (shift_right_logical v ((7-i)*8)) land 0xff))) done in
             w 0 (fst g); w 8 (snd g); b in
    let s = ghash h aad ciphertext in let ej0 = encrypt_block rks j0 in
    let etag = Bytes.create 16 in
    let w b off v = for i = 0 to 7 do Bytes.set b (off+i) (Char.chr (Int64.(to_int (shift_right_logical v ((7-i)*8)) land 0xff))) done in
    let s_b = Bytes.create 16 in w s_b 0 (fst s); w s_b 8 (snd s);
    xor_block_into etag 0 s_b 0 ej0 0 16;
    if Bytes.equal etag tag then Some (gcm_crypt rks j0 ciphertext) else None
end

module Stream = struct
  module Cbc_encrypt = struct
    type padding = [ `No_padding | `Pkcs7 ]
    type t = { key: bytes; iv: bytes; padding: padding; buf: Buffer.t; mutable closed: bool }
    let init ~key ~iv ~padding = { key; iv = Bytes.copy iv; padding; buf = Buffer.create 128; closed = false }
    let update t b = if not t.closed then Buffer.add_bytes t.buf b
    let finalize t = if t.closed then None else (t.closed <- true; Some (Cbc.encrypt_pkcs7 ~key:t.key ~iv:t.iv (Buffer.to_bytes t.buf)))
  end
  module Cbc_decrypt = struct
    type padding = [ `No_padding | `Pkcs7 ]
    type t = { key: bytes; iv: bytes; padding: padding; buf: Buffer.t; mutable closed: bool }
    let init ~key ~iv ~padding = { key; iv = Bytes.copy iv; padding; buf = Buffer.create 128; closed = false }
    let update t b = if not t.closed then Buffer.add_bytes t.buf b
    let finalize t = if t.closed then None else (t.closed <- true; Cbc.decrypt_pkcs7 ~key:t.key ~iv:t.iv (Buffer.to_bytes t.buf))
  end
  module Ctr = struct
    type t = { rks: round_keys; ctr: bytes; mutable ks: bytes; mutable ks_pos: int }
    let init ~key ~iv = { rks = key_schedule key; ctr = Bytes.copy iv; ks = Bytes.empty; ks_pos = 16 }
    let update t chunk =
      let len = Bytes.length chunk in let out = Bytes.create len in
      for i = 0 to len - 1 do
        if t.ks_pos = 16 then (t.ks <- encrypt_block t.rks t.ctr; t.ks_pos <- 0; incr_counter_be t.ctr);
        Bytes.set out i (Char.chr (Char.code (Bytes.get chunk i) lxor Char.code (Bytes.get t.ks t.ks_pos)));
        t.ks_pos <- t.ks_pos + 1
      done; out
  end
  module Gcm_encrypt = struct
    type t = { key: bytes; iv: bytes; aad: Buffer.t; data: Buffer.t }
    let init ~key ~iv = { key; iv; aad = Buffer.create 64; data = Buffer.create 128 }
    let add_aad t b = Buffer.add_bytes t.aad b
    let update t b = Buffer.add_bytes t.data b
    let finalize t = Gcm.encrypt ~key:t.key ~iv:t.iv ~aad:(Buffer.to_bytes t.aad) (Buffer.to_bytes t.data)
  end
  module Gcm_decrypt = struct
    type t = { key: bytes; iv: bytes; aad: Buffer.t; data: Buffer.t }
    let init ~key ~iv = { key; iv; aad = Buffer.create 64; data = Buffer.create 128 }
    let add_aad t b = Buffer.add_bytes t.aad b
    let update t b = Buffer.add_bytes t.data b
    let finalize t ~tag = Gcm.decrypt ~key:t.key ~iv:t.iv ~aad:(Buffer.to_bytes t.aad) ~tag (Buffer.to_bytes t.data)
  end
end