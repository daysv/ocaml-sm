type t = int array

let limbs = 16
let prod_limbs = limbs * 2
let base = 1 lsl 16
let mask = base - 1

let zero = Array.make limbs 0

let one =
  let x = Array.make limbs 0 in
  x.(0) <- 1;
  x

let copy x = Array.init limbs (fun i -> x.(i))

let of_int n =
  let x = Array.make limbs 0 in
  x.(0) <- n land mask;
  x

let compare a b =
  let rec loop i =
    if i < 0 then 0
    else
      let c = Stdlib.compare a.(i) b.(i) in
      if c <> 0 then c else loop (i - 1)
  in
  loop (limbs - 1)

let equal a b = compare a b = 0
let is_zero a = Array.for_all (( = ) 0) a

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> invalid_arg "U256.of_hex"

let of_hex s =
  let s =
    if String.length s >= 2 && String.sub s 0 2 = "0x" then String.sub s 2 (String.length s - 2) else s
  in
  if String.length s > 64 then invalid_arg "U256.of_hex";
  let padded = String.make (64 - String.length s) '0' ^ s in
  Array.init limbs (fun limb ->
      let base_idx = 64 - ((limb + 1) * 4) in
      let acc = ref 0 in
      for i = 0 to 3 do
        acc := (!acc lsl 4) lor hex_value padded.[base_idx + i]
      done;
      !acc)

let to_hex x =
  let out = Bytes.create 64 in
  let hex = "0123456789abcdef" in
  for limb = 0 to limbs - 1 do
    let v = x.(limbs - 1 - limb) in
    for i = 0 to 3 do
      let nibble = (v lsr ((3 - i) * 4)) land 0xf in
      Bytes.set out ((limb * 4) + i) hex.[nibble]
    done
  done;
  Bytes.unsafe_to_string out

let of_bytes_be bytes =
  if Bytes.length bytes > 32 then invalid_arg "U256.of_bytes_be";
  let padded = Bytes.make 32 '\000' in
  Bytes.blit bytes 0 padded (32 - Bytes.length bytes) (Bytes.length bytes);
  Array.init limbs (fun limb ->
      let off = 32 - ((limb + 1) * 2) in
      (Char.code (Bytes.get padded off) lsl 8) lor Char.code (Bytes.get padded (off + 1)))

let to_bytes_be x =
  let out = Bytes.create 32 in
  for limb = 0 to limbs - 1 do
    let v = x.(limbs - 1 - limb) in
    let off = limb * 2 in
    Bytes.set out off (Char.chr ((v lsr 8) land 0xff));
    Bytes.set out (off + 1) (Char.chr (v land 0xff))
  done;
  out

let add_raw a b =
  let out = Array.make limbs 0 in
  let carry = ref 0 in
  for i = 0 to limbs - 1 do
    let sum = a.(i) + b.(i) + !carry in
    out.(i) <- sum land mask;
    carry := sum lsr 16
  done;
  (out, !carry <> 0)

let sub_raw a b =
  let out = Array.make limbs 0 in
  let borrow = ref 0 in
  for i = 0 to limbs - 1 do
    let diff = a.(i) - b.(i) - !borrow in
    if diff < 0 then (
      out.(i) <- diff + base;
      borrow := 1
    ) else (
      out.(i) <- diff;
      borrow := 0
    )
  done;
  (out, !borrow <> 0)

let sub_small a n =
  let out = copy a in
  let rec loop i borrow =
    if borrow = 0 then ()
    else if i >= limbs then invalid_arg "U256.sub_small"
    else
      let diff = out.(i) - borrow in
      if diff < 0 then (
        out.(i) <- diff + base;
        loop (i + 1) 1
      ) else out.(i) <- diff
  in
  loop 0 n;
  out

let get_bit x bit =
  if bit < 0 || bit >= 256 then invalid_arg "U256.get_bit";
  let limb = bit / 16 in
  let offset = bit mod 16 in
  (x.(limb) lsr offset) land 1 = 1

let mul_raw a b =
  let out = Array.make prod_limbs 0 in
  for i = 0 to limbs - 1 do
    let carry = ref 0 in
    let ai = a.(i) in
    for j = 0 to limbs - 1 do
      let k = i + j in
      let sum = out.(k) + (ai * b.(j)) + !carry in
      out.(k) <- sum land mask;
      carry := sum lsr 16
    done;
    out.(i + limbs) <- out.(i + limbs) + !carry
  done;
  let carry = ref 0 in
  for i = 0 to prod_limbs - 1 do
    let sum = out.(i) + !carry in
    out.(i) <- sum land mask;
    carry := sum lsr 16
  done;
  if !carry <> 0 then invalid_arg "U256.mul_raw";
  out

let compare_var a b len =
  let rec loop i =
    if i < 0 then 0
    else
      let c = Stdlib.compare a.(i) b.(i) in
      if c <> 0 then c else loop (i - 1)
  in
  loop (len - 1)

let copy_var a len = Array.init len (fun i -> a.(i))

let shift_left_bits a len s =
  if s = 0 then copy_var a len
  else
    let out = Array.make (len + 1) 0 in
    let carry = ref 0 in
    for i = 0 to len - 1 do
      let v = (a.(i) lsl s) lor !carry in
      out.(i) <- v land mask;
      carry := v lsr 16
    done;
    out.(len) <- !carry;
    out

let shift_right_bits a len s =
  if s = 0 then copy_var a len
  else
    let out = Array.make len 0 in
    let carry_mask = (1 lsl s) - 1 in
    let carry_shift = 16 - s in
    for i = len - 1 downto 0 do
      let carry = if i + 1 < len then (a.(i + 1) land carry_mask) lsl carry_shift else 0 in
      out.(i) <- (a.(i) lsr s) lor carry
    done;
    out

let sub_mul_shift u v q j =
  let borrow = ref 0 in
  let carry = ref 0 in
  for i = 0 to limbs - 1 do
    let prod = (v.(i) * q) + !carry in
    carry := prod lsr 16;
    let diff = u.(j + i) - (prod land mask) - !borrow in
    if diff < 0 then (
      u.(j + i) <- diff + base;
      borrow := 1
    ) else (
      u.(j + i) <- diff;
      borrow := 0
    )
  done;
  let diff = u.(j + limbs) - !carry - !borrow in
  if diff < 0 then (
    u.(j + limbs) <- diff + base;
    true
  ) else (
    u.(j + limbs) <- diff;
    false
  )

let add_mul_shift u v q j =
  let carry = ref 0 in
  for i = 0 to limbs - 1 do
    let sum = u.(j + i) + (v.(i) * q) + !carry in
    u.(j + i) <- sum land mask;
    carry := sum lsr 16
  done;
  let rec propagate idx c =
    if c = 0 then ()
    else
      let sum = u.(idx) + c in
      u.(idx) <- sum land mask;
      propagate (idx + 1) (sum lsr 16)
  in
  propagate (j + limbs) !carry

let reduce_mod product modulus =
  let u = Array.make (prod_limbs + 2) 0 in
  Array.blit product 0 u 0 prod_limbs;
  let v = copy_var modulus limbs in
  for j = limbs downto 0 do
    let top = u.(j + limbs) in
    if top <> 0 then (
      let numerator = (top lsl 16) + u.(j + limbs - 1) in
      let mut_q = numerator / v.(limbs - 1) in
      let q =
        if mut_q >= base then base - 1 else mut_q
      in
      let rec adjust q =
        if q <= 0 then 0
        else if q * v.(limbs - 2) <= ((numerator - (q * v.(limbs - 1))) lsl 16) + u.(j + limbs - 2) then q
        else adjust (q - 1)
      in
      let q = adjust q in
      if q > 0 then (
        let borrow = sub_mul_shift u v q j in
        if borrow then add_mul_shift u v 1 j
      )
    )
  done;
  let r = Array.make limbs 0 in
  Array.blit u 0 r 0 limbs;
  while compare r modulus >= 0 do
    let r', _ = sub_raw r modulus in
    Array.blit r' 0 r 0 limbs
  done;
  r

let add_mod a b modulus =
  let sum, carry = add_raw a b in
  if carry || compare sum modulus >= 0 then fst (sub_raw sum modulus) else sum

let sub_mod a b modulus =
  let diff, borrow = sub_raw a b in
  if borrow then fst (add_raw diff modulus) else diff

let double_mod a modulus = add_mod a a modulus

let mul_mod a b modulus =
  let prod = mul_raw a b in
  reduce_mod prod modulus

let square_mod a modulus = mul_mod a a modulus

let pow_mod base_x exp modulus =
  let result = ref one in
  let base = ref base_x in
  for bit = 0 to 255 do
    if get_bit exp bit then result := mul_mod !result !base modulus;
    base := square_mod !base modulus
  done;
  !result
