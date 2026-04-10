type t = {
  mutable a : int32;
  mutable b : int32;
  mutable c : int32;
  mutable d : int32;
  mutable e : int32;
  mutable f : int32;
  mutable g : int32;
  mutable h : int32;
  buffer : bytes;
  mutable buffer_len : int;
  mutable total_len : int64;
  w : int32 array;
  w1 : int32 array;
}

let iv =
  ( 0x7380166Fl,
    0x4914B2B9l,
    0x172442D7l,
    0xDA8A0600l,
    0xA96F30BCl,
    0x163138AAl,
    0xE38DEE4Dl,
    0xB0FB0E4El )

let init () =
  let a, b, c, d, e, f, g, h = iv in
  {
    a; b; c; d; e; f; g; h;
    buffer = Bytes.make 64 '\000';
    buffer_len = 0;
    total_len = 0L;
    w = Array.make 68 0l;
    w1 = Array.make 64 0l;
  }

(* 满足 mli 的 empty 导出 *)
let empty = init ()

let ( ++ ) = Int32.add
let ( ^^ ) = Int32.logxor
let ( &&& ) = Int32.logand
let ( ||| ) = Int32.logor
let lnot = Int32.lognot

let[@inline always] rotl x n =
  let n = n land 31 in
  if n = 0 then x
  else Int32.((logor (shift_left x n) (shift_right_logical x (32 - n))))

let[@inline always] p0 x = x ^^ rotl x 9 ^^ rotl x 17
let[@inline always] p1 x = x ^^ rotl x 15 ^^ rotl x 23

let t_shifted_array =
  let arr = Array.make 64 0l in
  for j = 0 to 63 do
    let t = if j < 16 then 0x79CC4519l else 0x7A879D8Al in
    arr.(j) <- rotl t j
  done;
  arr

let compress state block off =
  let w = state.w in
  let w1 = state.w1 in
  for j = 0 to 15 do
    Array.unsafe_set w j (Bytes.get_int32_be block (off + (j * 4)))
  done;
  for j = 16 to 67 do
    let x = (Array.unsafe_get w (j - 16)) ^^ (Array.unsafe_get w (j - 9)) ^^ (rotl (Array.unsafe_get w (j - 3)) 15) in
    Array.unsafe_set w j ((p1 x) ^^ (rotl (Array.unsafe_get w (j - 13)) 7) ^^ (Array.unsafe_get w (j - 6)))
  done;
  for j = 0 to 63 do
    Array.unsafe_set w1 j ((Array.unsafe_get w j) ^^ (Array.unsafe_get w (j + 4)))
  done;

  let a = ref state.a in let b = ref state.b in
  let c = ref state.c in let d = ref state.d in
  let e = ref state.e in let f = ref state.f in
  let g = ref state.g in let h = ref state.h in

  for j = 0 to 63 do
    let a12 = rotl !a 12 in
    let ss1 = rotl (a12 ++ !e ++ Array.unsafe_get t_shifted_array j) 7 in
    let ss2 = ss1 ^^ a12 in
    let tt1 = 
      if j < 16 then (!a ^^ !b ^^ !c) ++ !d ++ ss2 ++ (Array.unsafe_get w1 j)
      else ((!a &&& !b) ||| (!a &&& !c) ||| (!b &&& !c)) ++ !d ++ ss2 ++ (Array.unsafe_get w1 j)
    in
    let tt2 = 
      if j < 16 then (!e ^^ !f ^^ !g) ++ !h ++ ss1 ++ (Array.unsafe_get w j)
      else ((!e &&& !f) ||| ((lnot !e) &&& !g)) ++ !h ++ ss1 ++ (Array.unsafe_get w j)
    in
    d := !c; c := rotl !b 9; b := !a; a := tt1;
    h := !g; g := rotl !f 19; f := !e; e := p0 tt2
  done;

  state.a <- state.a ^^ !a;
  state.b <- state.b ^^ !b;
  state.c <- state.c ^^ !c;
  state.d <- state.d ^^ !d;
  state.e <- state.e ^^ !e;
  state.f <- state.f ^^ !f;
  state.g <- state.g ^^ !g;
  state.h <- state.h ^^ !h

(* 修正：添加了 ~off ~len 标签，并返回 state 以匹配接口 *)
let update_subbytes state src ~off ~len =
  if off < 0 || len < 0 || off + len > Bytes.length src then invalid_arg "Sm3.update_subbytes";
  state.total_len <- Int64.add state.total_len (Int64.of_int len);
  let pos = ref off in
  let remaining = ref len in
  if !remaining > 0 && state.buffer_len > 0 then (
    let fill = min (64 - state.buffer_len) !remaining in
    Bytes.blit src !pos state.buffer state.buffer_len fill;
    state.buffer_len <- state.buffer_len + fill;
    pos := !pos + fill;
    remaining := !remaining - fill;
    if state.buffer_len = 64 then (compress state state.buffer 0; state.buffer_len <- 0)
  );
  while !remaining >= 64 do
    compress state src !pos;
    pos := !pos + 64;
    remaining := !remaining - 64
  done;
  if !remaining > 0 then (
    Bytes.blit src !pos state.buffer 0 !remaining;
    state.buffer_len <- !remaining
  );
  state

let update_bytes state bytes = 
  update_subbytes state bytes ~off:0 ~len:(Bytes.length bytes)

let update_string state s = 
  update_bytes state (Bytes.unsafe_of_string s)

let set_u64_be bytes off x =
  let open Int64 in
  for i = 0 to 7 do
    let shift = (7 - i) * 8 in
    Bytes.set bytes (off + i) (Char.chr (to_int (shift_right_logical x shift) land 0xff))
  done

let finalize state =
  let bit_len = Int64.shift_left state.total_len 3 in
  let pad_zeros = if state.buffer_len < 56 then 55 - state.buffer_len else 119 - state.buffer_len in
  let final_block = Bytes.make (state.buffer_len + 1 + pad_zeros + 8) '\000' in
  Bytes.blit state.buffer 0 final_block 0 state.buffer_len;
  Bytes.set final_block state.buffer_len '\x80';
  set_u64_be final_block (Bytes.length final_block - 8) bit_len;
  let off = ref 0 in
  while !off < Bytes.length final_block do
    compress state final_block !off;
    off := !off + 64
  done;
  let out = Bytes.create 32 in
  Bytes.set_int32_be out 0 state.a;
  Bytes.set_int32_be out 4 state.b;
  Bytes.set_int32_be out 8 state.c;
  Bytes.set_int32_be out 12 state.d;
  Bytes.set_int32_be out 16 state.e;
  Bytes.set_int32_be out 20 state.f;
  Bytes.set_int32_be out 24 state.g;
  Bytes.set_int32_be out 28 state.h;
  Bytes.unsafe_to_string out

let to_hex raw =
  let hex = "0123456789abcdef" in
  let out = Bytes.create (String.length raw * 2) in
  String.iteri (fun i ch ->
    let v = Char.code ch in
    Bytes.set out (2 * i) hex.[v lsr 4];
    Bytes.set out ((2 * i) + 1) hex.[v land 0x0f]
  ) raw;
  Bytes.unsafe_to_string out

let finalize_hex state = to_hex (finalize state)

let digest_string s =
  finalize (update_string (init ()) s)

let digest_bytes b =
  finalize (update_bytes (init ()) b)

let digest_hex s = to_hex (digest_string s)

(* HMAC-SM3 *)
let hmac ~key data =
  let block_size = 64 in
  let key =
    if String.length key > block_size then digest_string key else key
  in
  let key = key ^ String.make (block_size - String.length key) '\000' in
  let ipad = Bytes.make block_size '\x36' in
  let opad = Bytes.make block_size '\x5c' in
  for i = 0 to block_size - 1 do
    let k = Char.code key.[i] in
    Bytes.set ipad i (Char.chr (k lxor Char.code (Bytes.get ipad i)));
    Bytes.set opad i (Char.chr (k lxor Char.code (Bytes.get opad i)))
  done;
  let inner = digest_string (Bytes.unsafe_to_string ipad ^ data) in
  digest_string (Bytes.unsafe_to_string opad ^ inner)

let hmac_hex ~key data = to_hex (hmac ~key data)

(* KDF based on SM3, as specified in GM/T 0003.4-2012 *)
let kdf ~z ~klen =
  if klen < 0 then invalid_arg "Sm3.kdf: klen must be non-negative";
  (* GM/T 0003.4-2012 specifies maximum derived key length is (2^32 - 1) * 32 bytes *)
  if klen > (Int64.to_int (Int64.sub (Int64.shift_left 1L 32) 1L)) * 32 then
    invalid_arg "Sm3.kdf: klen exceeds maximum allowed (2^32 - 1) * 32";
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
    let block = digest_bytes block_bytes in
    let take = min 32 (klen - ((ct - 1) * 32)) in
    Bytes.blit_string block 0 out ((ct - 1) * 32) take
  done;
  Bytes.unsafe_to_string out