type t = {
  a : int32;
  b : int32;
  c : int32;
  d : int32;
  e : int32;
  f : int32;
  g : int32;
  h : int32;
  buffer : bytes;
  buffer_len : int;
  total_len : int64;
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

let empty =
  let a, b, c, d, e, f, g, h = iv in
  let w = Array.make 68 0l in
  let w1 = Array.make 64 0l in
  { a; b; c; d; e; f; g; h; buffer = Bytes.make 64 '\000'; buffer_len = 0; total_len = 0L; w; w1 }

let init () = empty

let ( ++ ) = Int32.add
let ( ^^ ) = Int32.logxor
let ( &&& ) = Int32.logand
let ( ||| ) = Int32.logor
let lnot = Int32.lognot

(* Inline these for performance - used directly in compress loop *)
(* Keep as external for test compatibility, but will be inlined in compress *)
let rotl x n =
  let n = n land 31 in
  if n = 0 then x
  else Int32.(logor (shift_left x n) (shift_right_logical x (32 - n)))

let rotl_7 x = rotl x 7
let rotl_9 x = rotl x 9
let rotl_12 x = rotl x 12
let rotl_15 x = rotl x 15
let rotl_17 x = rotl x 17
let rotl_19 x = rotl x 19
let rotl_23 x = rotl x 23

let p0 x = x ^^ rotl_9 x ^^ rotl_17 x
let p1 x = x ^^ rotl_15 x ^^ rotl_23 x

let t_array =
  let arr = Array.make 64 0l in
  for j = 0 to 15 do arr.(j) <- 0x79CC4519l done;
  for j = 16 to 63 do arr.(j) <- 0x7A879D8Al done;
  arr

let get_u32_be bytes off =
  let open Int32 in
  logor
    (shift_left (of_int (Char.code (Bytes.get bytes off))) 24)
    (logor
       (shift_left (of_int (Char.code (Bytes.get bytes (off + 1)))) 16)
       (logor
          (shift_left (of_int (Char.code (Bytes.get bytes (off + 2)))) 8)
          (of_int (Char.code (Bytes.get bytes (off + 3))))))

let set_u64_be bytes off x =
  let open Int64 in
  Bytes.set bytes off (Char.chr (to_int (shift_right_logical x 56) land 0xff));
  Bytes.set bytes (off + 1) (Char.chr (to_int (shift_right_logical x 48) land 0xff));
  Bytes.set bytes (off + 2) (Char.chr (to_int (shift_right_logical x 40) land 0xff));
  Bytes.set bytes (off + 3) (Char.chr (to_int (shift_right_logical x 32) land 0xff));
  Bytes.set bytes (off + 4) (Char.chr (to_int (shift_right_logical x 24) land 0xff));
  Bytes.set bytes (off + 5) (Char.chr (to_int (shift_right_logical x 16) land 0xff));
  Bytes.set bytes (off + 6) (Char.chr (to_int (shift_right_logical x 8) land 0xff));
  Bytes.set bytes (off + 7) (Char.chr (to_int x land 0xff))

let compress state block off =
  let w = state.w in
  let w1 = state.w1 in
  (* Reset arrays to zero - necessary because arrays are reused *)
  for i = 0 to 67 do w.(i) <- 0l done;
  for i = 0 to 63 do w1.(i) <- 0l done;
  for j = 0 to 15 do
    w.(j) <- get_u32_be block (off + (j * 4))
  done;
  (* Precompute rotl_15 and rotl_7 for w expansion using local bindings *)
  for j = 16 to 67 do
    let x = w.(j - 16) ^^ w.(j - 9) ^^ rotl (w.(j - 3)) 15 in
    let y = p1 x in
    w.(j) <- y ^^ rotl (w.(j - 13)) 7 ^^ w.(j - 6)
  done;
  for j = 0 to 63 do
    w1.(j) <- w.(j) ^^ w.(j + 4)
  done;
  let a = ref state.a
  and b = ref state.b
  and c = ref state.c
  and d = ref state.d
  and e = ref state.e
  and f = ref state.f
  and g = ref state.g
  and h = ref state.h in
  for j = 0 to 63 do
    let t = t_array.(j) in
    let a12 = rotl !a 12 in
    let ss1 = rotl (a12 ++ !e ++ rotl t j) 7 in
    let ss2 = ss1 ^^ a12 in
    let tt1 =
      if j < 16 then
        (!a ^^ !b ^^ !c) ++ !d ++ ss2 ++ w1.(j)
      else
        ((!a &&& !b) ||| (!a &&& !c) ||| (!b &&& !c)) ++ !d ++ ss2 ++ w1.(j)
    in
    let tt2 =
      if j < 16 then
        (!e ^^ !f ^^ !g) ++ !h ++ ss1 ++ w.(j)
      else
        ((!e &&& !f) ||| (lnot !e &&& !g)) ++ !h ++ ss1 ++ w.(j)
    in
    d := !c;
    c := rotl !b 9;
    b := !a;
    a := tt1;
    h := !g;
    g := rotl !f 19;
    f := !e;
    e := p0 tt2
  done;
  {
    state with
    a = state.a ^^ !a;
    b = state.b ^^ !b;
    c = state.c ^^ !c;
    d = state.d ^^ !d;
    e = state.e ^^ !e;
    f = state.f ^^ !f;
    g = state.g ^^ !g;
    h = state.h ^^ !h;
  }

let process_bytes state src off len =
  if off < 0 || len < 0 || off + len > Bytes.length src then invalid_arg "Sm3.update_subbytes";
  let total_len = Int64.add state.total_len (Int64.of_int len) in
  let state = { state with total_len } in
  let state = ref state in
  let pos = ref off in
  let remaining = ref len in
  (* Ensure w and w1 arrays are properly initialized for reuse *)
  if !remaining > 0 && !state.buffer_len > 0 then (
    let fill = min (64 - !state.buffer_len) !remaining in
    Bytes.blit src !pos !state.buffer !state.buffer_len fill;
    let new_len = !state.buffer_len + fill in
    pos := !pos + fill;
    remaining := !remaining - fill;
    state := { !state with buffer_len = new_len };
    if new_len = 64 then state := { (compress !state !state.buffer 0) with buffer_len = 0 });
  while !remaining >= 64 do
    state := compress !state src !pos;
    pos := !pos + 64;
    remaining := !remaining - 64
  done;
  if !remaining > 0 then (
    Bytes.blit src !pos !state.buffer 0 !remaining;
    state := { !state with buffer_len = !remaining });
  !state

let update_subbytes state bytes ~off ~len = process_bytes state bytes off len
let update_bytes state bytes = process_bytes state bytes 0 (Bytes.length bytes)
let update_string state s = update_bytes state (Bytes.unsafe_of_string s)

let store_u32_be bytes off x =
  let open Int32 in
  Bytes.set bytes off (Char.chr (to_int (shift_right_logical x 24) land 0xff));
  Bytes.set bytes (off + 1) (Char.chr (to_int (shift_right_logical x 16) land 0xff));
  Bytes.set bytes (off + 2) (Char.chr (to_int (shift_right_logical x 8) land 0xff));
  Bytes.set bytes (off + 3) (Char.chr (to_int x land 0xff))

let finalize state =
  let bit_len = Int64.mul state.total_len 8L in
  let pad_zeros = if state.buffer_len < 56 then 55 - state.buffer_len else 119 - state.buffer_len in
  let final_block = Bytes.make (state.buffer_len + 1 + pad_zeros + 8) '\000' in
  Bytes.blit state.buffer 0 final_block 0 state.buffer_len;
  Bytes.set final_block state.buffer_len '\x80';
  set_u64_be final_block (Bytes.length final_block - 8) bit_len;
  let rec loop st off =
    if off >= Bytes.length final_block then st
    else loop (compress st final_block off) (off + 64)
  in
  let st = loop { state with buffer_len = 0 } 0 in
  let out = Bytes.create 32 in
  store_u32_be out 0 st.a;
  store_u32_be out 4 st.b;
  store_u32_be out 8 st.c;
  store_u32_be out 12 st.d;
  store_u32_be out 16 st.e;
  store_u32_be out 20 st.f;
  store_u32_be out 24 st.g;
  store_u32_be out 28 st.h;
  Bytes.unsafe_to_string out

let to_hex raw =
  let hex = "0123456789abcdef" in
  let out = Bytes.create (String.length raw * 2) in
  String.iteri
    (fun i ch ->
      let v = Char.code ch in
      Bytes.set out (2 * i) hex.[v lsr 4];
      Bytes.set out ((2 * i) + 1) hex.[v land 0x0f])
    raw;
  Bytes.unsafe_to_string out

let finalize_hex state = to_hex (finalize state)
let digest_string s = finalize (update_string empty s)
let digest_bytes b = finalize (update_bytes empty b)
let digest_hex s = to_hex (digest_string s)
