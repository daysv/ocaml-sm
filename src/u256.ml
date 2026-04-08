type t = int64 array

let limbs = 8
let prod_limbs = limbs * 2
let base = 0x1_0000_0000L
let mask = 0xffff_ffffL

let zero = Array.make limbs 0L

let one =
  let x = Array.make limbs 0L in
  x.(0) <- 1L;
  x

let copy x = Array.init limbs (fun i -> x.(i))

let of_int n =
  let x = Array.make limbs 0L in
  x.(0) <- Int64.of_int n;
  x

let compare a b =
  let rec loop i =
    if i < 0 then 0
    else
      let c = Int64.compare a.(i) b.(i) in
      if c <> 0 then c else loop (i - 1)
  in
  loop (limbs - 1)

let equal a b = compare a b = 0
let is_zero a = Array.for_all (( = ) 0L) a

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> invalid_arg "U256.of_hex"

let of_hex_var s =
  let s =
    if String.length s >= 2 && String.sub s 0 2 = "0x" then
      String.sub s 2 (String.length s - 2)
    else s
  in
  if String.length s = 0 then [||]
  else
    let limb_count = (String.length s + 7) / 8 in
    let padded = String.make (limb_count * 8 - String.length s) '0' ^ s in
    Array.init limb_count (fun limb ->
        let base_idx = (limb_count * 8) - ((limb + 1) * 8) in
        let acc = ref 0L in
        for i = 0 to 7 do
          let v = Int64.of_int (hex_value padded.[base_idx + i]) in
          acc := Int64.logor (Int64.shift_left !acc 4) v
        done;
        !acc)

let of_hex s =
  let s =
    if String.length s >= 2 && String.sub s 0 2 = "0x" then
      String.sub s 2 (String.length s - 2)
    else s
  in
  if String.length s > 64 then invalid_arg "U256.of_hex";
  let padded = String.make (64 - String.length s) '0' ^ s in
  Array.init limbs (fun limb ->
      let base_idx = 64 - ((limb + 1) * 8) in
      let acc = ref 0L in
      for i = 0 to 7 do
        let v = Int64.of_int (hex_value padded.[base_idx + i]) in
        acc := Int64.logor (Int64.shift_left !acc 4) v
      done;
      !acc)

let to_hex x =
  let out = Bytes.create 64 in
  let hex = "0123456789abcdef" in
  for limb = 0 to limbs - 1 do
    let v = x.(limbs - 1 - limb) in
    for i = 0 to 7 do
      let nibble = Int64.to_int (Int64.logand (Int64.shift_right_logical v ((7 - i) * 4)) 0xFL) in
      Bytes.set out ((limb * 8) + i) hex.[nibble]
    done
  done;
  Bytes.unsafe_to_string out

let of_bytes_be bytes =
  if Bytes.length bytes > 32 then invalid_arg "U256.of_bytes_be";
  let padded = Bytes.make 32 '\000' in
  Bytes.blit bytes 0 padded (32 - Bytes.length bytes) (Bytes.length bytes);
  Array.init limbs (fun limb ->
      let off = 32 - ((limb + 1) * 4) in
      Int64.logor
        (Int64.shift_left (Int64.of_int (Char.code (Bytes.get padded off))) 24)
        (Int64.logor
           (Int64.shift_left (Int64.of_int (Char.code (Bytes.get padded (off + 1)))) 16)
           (Int64.logor
              (Int64.shift_left (Int64.of_int (Char.code (Bytes.get padded (off + 2)))) 8)
              (Int64.of_int (Char.code (Bytes.get padded (off + 3)))))))

let to_bytes_be x =
  let out = Bytes.create 32 in
  for limb = 0 to limbs - 1 do
    let v = x.(limbs - 1 - limb) in
    let off = limb * 4 in
    Bytes.set out off (Char.chr (Int64.to_int (Int64.shift_right_logical v 24) land 0xff));
    Bytes.set out (off + 1) (Char.chr (Int64.to_int (Int64.shift_right_logical v 16) land 0xff));
    Bytes.set out (off + 2) (Char.chr (Int64.to_int (Int64.shift_right_logical v 8) land 0xff));
    Bytes.set out (off + 3) (Char.chr (Int64.to_int v land 0xff))
  done;
  out

let add_raw a b =
  let out = Array.make limbs 0L in
  let carry = ref 0L in
  for i = 0 to limbs - 1 do
    let sum = Int64.add (Int64.add a.(i) b.(i)) !carry in
    out.(i) <- Int64.logand sum mask;
    carry := Int64.shift_right_logical sum 32
  done;
  (out, !carry <> 0L)

let sub_raw a b =
  let out = Array.make limbs 0L in
  let borrow = ref 0L in
  for i = 0 to limbs - 1 do
    let diff = Int64.sub (Int64.sub a.(i) b.(i)) !borrow in
    if Int64.compare diff 0L < 0 then (
      out.(i) <- Int64.add diff base;
      borrow := 1L
    ) else (
      out.(i) <- diff;
      borrow := 0L
    )
  done;
  (out, !borrow <> 0L)

let sub_small a n =
  let out = copy a in
  let rec loop i borrow =
    if borrow = 0L then ()
    else if i >= limbs then invalid_arg "U256.sub_small"
    else
      let diff = Int64.sub out.(i) borrow in
      if Int64.compare diff 0L < 0 then (
        out.(i) <- Int64.add diff base;
        loop (i + 1) 1L
      ) else out.(i) <- diff
  in
  loop 0 (Int64.of_int n);
  out

let get_bit x bit =
  if bit < 0 || bit >= 256 then invalid_arg "U256.get_bit";
  let limb = bit / 32 in
  let offset = bit land 31 in
  Int64.(logand (shift_right_logical x.(limb) offset) 1L = 1L)

let get_nibble x bit =
  if bit < 0 || bit >= 256 || bit land 3 <> 0 then invalid_arg "U256.get_nibble";
  let limb = bit / 32 in
  let offset = bit land 31 in
  Int64.to_int Int64.(logand (shift_right_logical x.(limb) offset) 0xFL)

let mul_raw a b =
  let out = Array.make prod_limbs 0L in
  for i = 0 to limbs - 1 do
    let ai = a.(i) in
    let carry = ref 0L in
    for j = 0 to limbs - 1 do
      let k = i + j in
      let prod = Int64.mul ai b.(j) in
      let sum = Int64.add (Int64.add out.(k) (Int64.logand prod mask)) !carry in
      out.(k) <- Int64.logand sum mask;
      carry := Int64.add (Int64.shift_right_logical prod 32) (Int64.shift_right_logical sum 32)
    done;
    let rec propagate k c =
      if Int64.compare c 0L <> 0 then (
        if k >= prod_limbs then invalid_arg "U256.mul_raw";
        let sum = Int64.add out.(k) c in
        out.(k) <- Int64.logand sum mask;
        propagate (k + 1) (Int64.shift_right_logical sum 32)
      )
    in
    propagate (i + limbs) !carry
  done;
  out

let copy_var a len = Array.init len (fun i -> a.(i))

let compare_var a b len =
  let rec loop i =
    if i < 0 then 0
    else
      let c = Int64.compare a.(i) b.(i) in
      if c <> 0 then c else loop (i - 1)
  in
  loop (len - 1)

let slice_var a off len = Array.init len (fun i -> if off + i < Array.length a then a.(off + i) else 0L)

let compare_var_len a alen b blen =
  let len = max alen blen in
  let rec loop i =
    if i < 0 then 0
    else
      let ai = if i < alen then a.(i) else 0L in
      let bi = if i < blen then b.(i) else 0L in
      let c = Int64.compare ai bi in
      if c <> 0 then c else loop (i - 1)
  in
  loop (len - 1)

let sub_raw_var a alen b blen =
  let len = max alen blen in
  let out = Array.make len 0L in
  let borrow = ref 0L in
  for i = 0 to len - 1 do
    let ai = if i < alen then a.(i) else 0L in
    let bi = if i < blen then b.(i) else 0L in
    let diff = Int64.sub (Int64.sub ai bi) !borrow in
    if Int64.compare diff 0L < 0 then (
      out.(i) <- Int64.add diff base;
      borrow := 1L
    ) else (
      out.(i) <- diff;
      borrow := 0L
    )
  done;
  (out, !borrow <> 0L)

let mul_raw_var a alen b blen =
  let out = Array.make (alen + blen) 0L in
  for i = 0 to alen - 1 do
    let ai = a.(i) in
    let carry = ref 0L in
    for j = 0 to blen - 1 do
      let k = i + j in
      let prod = Int64.mul ai b.(j) in
      let sum = Int64.add (Int64.add out.(k) (Int64.logand prod mask)) !carry in
      out.(k) <- Int64.logand sum mask;
      carry := Int64.add (Int64.shift_right_logical prod 32) (Int64.shift_right_logical sum 32)
    done;
    let rec propagate k c =
      if Int64.compare c 0L <> 0 then (
        if k >= Array.length out then invalid_arg "U256.mul_raw_var";
        let sum = Int64.add out.(k) c in
        out.(k) <- Int64.logand sum mask;
        propagate (k + 1) (Int64.shift_right_logical sum 32)
      )
    in
    propagate (i + blen) !carry
  done;
  out

let mul_scalar v q =
  let out = Array.make (limbs + 1) 0L in
  let carry = ref 0L in
  for i = 0 to limbs - 1 do
    let prod = Int64.mul v.(i) q in
    let sum = Int64.add (Int64.add (Int64.logand prod mask) !carry) 0L in
    out.(i) <- Int64.logand sum mask;
    carry := Int64.add (Int64.shift_right_logical prod 32) (Int64.shift_right_logical sum 32)
  done;
  out.(limbs) <- !carry;
  out

let sub_shifted u v q j =
  let prod = mul_scalar v q in
  let borrow = ref 0L in
  for i = 0 to limbs do
    let diff = Int64.sub (Int64.sub u.(j + i) prod.(i)) !borrow in
    if Int64.compare diff 0L < 0 then (
      u.(j + i) <- Int64.add diff base;
      borrow := 1L
    ) else (
      u.(j + i) <- diff;
      borrow := 0L
    )
  done;
  !borrow <> 0L

let add_shifted u v j =
  let carry = ref 0L in
  for i = 0 to limbs - 1 do
    let sum = Int64.add (Int64.add u.(j + i) v.(i)) !carry in
    u.(j + i) <- Int64.logand sum mask;
    carry := Int64.shift_right_logical sum 32
  done;
  let rec propagate idx c =
    if Int64.compare c 0L <> 0 then (
      if idx >= Array.length u then invalid_arg "U256.add_shifted";
      let sum = Int64.add u.(idx) c in
      u.(idx) <- Int64.logand sum mask;
      propagate (idx + 1) (Int64.shift_right_logical sum 32)
    )
  in
  propagate (j + limbs) !carry

let reduce_mod product modulus =
  let u = Array.make (prod_limbs + 2) 0L in
  Array.blit product 0 u 0 prod_limbs;
  let v = copy_var modulus limbs in
  let v_top = v.(limbs - 1) in
  let v_next = v.(limbs - 2) in
  for j = limbs downto 0 do
    let top = u.(j + limbs) in
    if Int64.compare top 0L <> 0 then (
      let numerator = Int64.logor (Int64.shift_left top 32) u.(j + limbs - 1) in
      let mut_q = Int64.unsigned_div numerator v_top in
      let q = if Int64.unsigned_compare mut_q mask > 0 then mask else mut_q in
      let rec adjust q =
        if Int64.compare q 0L <= 0 then 0L
        else
          let rhat = Int64.sub numerator (Int64.mul q v_top) in
          let left = Int64.mul q v_next in
          let right = Int64.logor (Int64.shift_left rhat 32) u.(j + limbs - 2) in
          if Int64.unsigned_compare left right <= 0 then q else adjust (Int64.sub q 1L)
      in
      let q = adjust q in
      if Int64.compare q 0L > 0 then (
        let borrow = sub_shifted u v q j in
        if borrow then add_shifted u v j
      )
    )
  done;
  let r = Array.make limbs 0L in
  Array.blit u 0 r 0 limbs;
  while compare r modulus >= 0 do
    let r', _ = sub_raw r modulus in
    Array.blit r' 0 r 0 limbs
  done;
  r

let sm2_p =
  of_hex "FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFF"

let sm2_n =
  of_hex "FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFF7203DF6B21C6052B53BBF40939D54123"

let mu_p =
  of_hex_var "10000000100000001000000010000000100000002000000020000000200000003"

let mu_n =
  of_hex_var "1000000010000000100000001000000018dfc2096fa323c0112ac6361f15149a0"

let reduce_barrett modulus mu product =
  let q1 = slice_var product (limbs - 1) (limbs + 1) in
  let q2 = mul_raw_var q1 (Array.length q1) mu (Array.length mu) in
  let q3 = slice_var q2 (limbs + 1) (limbs + 1) in
  let r1 = slice_var product 0 (limbs + 1) in
  let r2 = slice_var (mul_raw_var q3 (limbs + 1) modulus limbs) 0 (limbs + 1) in
  let r, _ = sub_raw_var r1 (limbs + 1) r2 (limbs + 1) in
  let rec loop r =
    if compare_var_len r (limbs + 1) modulus limbs >= 0 then
      let r', _ = sub_raw_var r (limbs + 1) modulus limbs in
      loop r'
    else r
  in
  loop r

let add_mod a b modulus =
  let sum, carry = add_raw a b in
  if carry || compare sum modulus >= 0 then fst (sub_raw sum modulus) else sum

let sub_mod a b modulus =
  let diff, borrow = sub_raw a b in
  if borrow then fst (add_raw diff modulus) else diff

let double_mod a modulus = add_mod a a modulus

let mul_mod a b modulus =
  let prod = mul_raw a b in
  if equal modulus sm2_p then reduce_barrett modulus mu_p prod
  else if equal modulus sm2_n then reduce_barrett modulus mu_n prod
  else (
    let acc = ref zero in
    let cur = ref a in
    for bit = 0 to 255 do
      if get_bit b bit then acc := add_mod !acc !cur modulus;
      cur := double_mod !cur modulus
    done;
    !acc
  )

let square_mod a modulus = mul_mod a a modulus

let pow_mod base_x exp modulus =
  let table = Array.make 16 zero in
  table.(0) <- one;
  table.(1) <- base_x;
  for i = 2 to 15 do
    table.(i) <- mul_mod table.(i - 1) base_x modulus
  done;
  let result = ref one in
  let first = ref true in
  for window = 63 downto 0 do
    if !first then first := false
    else
      for _ = 1 to 4 do
        result := square_mod !result modulus
      done;
    let nibble = get_nibble exp (window * 4) in
    if nibble <> 0 then result := mul_mod !result table.(nibble) modulus
  done;
  !result
