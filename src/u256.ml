type t = int64 array

let limbs = 8
let base = 0x1_0000_0000L
let mask = 0xffff_ffffL

let zero = Array.make limbs 0L

let one =
  let x = Array.make limbs 0L in
  x.(0) <- 1L;
  x

let copy x = Array.init limbs (fun i -> x.(i))
let of_int n = let x = Array.make limbs 0L in x.(0) <- Int64.of_int n; x

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
      let nibble = Int64.to_int (Int64.logand (Int64.shift_right_logical v ((7 - i) * 4)) 0xfL) in
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

let add_mod a b modulus =
  let sum, carry = add_raw a b in
  if carry || compare sum modulus >= 0 then fst (sub_raw sum modulus) else sum

let sub_mod a b modulus =
  let diff, borrow = sub_raw a b in
  if borrow then fst (add_raw diff modulus) else diff

let double_mod a modulus = add_mod a a modulus

let get_bit x bit =
  if bit < 0 || bit >= 256 then invalid_arg "U256.get_bit";
  let limb = bit / 32 in
  let offset = bit mod 32 in
  Int64.(logand (shift_right_logical x.(limb) offset) 1L = 1L)

let mul_mod a b modulus =
  let acc = ref zero in
  let cur = ref a in
  for bit = 0 to 255 do
    if get_bit b bit then acc := add_mod !acc !cur modulus;
    cur := double_mod !cur modulus
  done;
  !acc

let square_mod a modulus = mul_mod a a modulus

let pow_mod base_x exp modulus =
  let result = ref one in
  let base = ref base_x in
  for bit = 0 to 255 do
    if get_bit exp bit then result := mul_mod !result !base modulus;
    base := square_mod !base modulus
  done;
  !result
