let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> invalid_arg "hex"

let bytes_of_hex s =
  let out = Bytes.create (String.length s / 2) in
  for i = 0 to Bytes.length out - 1 do
    Bytes.set out i (Char.chr ((hex_value s.[2 * i] lsl 4) lor hex_value s.[(2 * i) + 1]))
  done;
  out

let hex_of_int32 x = Printf.sprintf "%08lx" x

let test_zero_vector () =
  let key = Bytes.make 16 '\000' in
  let iv = Bytes.make 16 '\000' in
  let z = Zuc.init ~key ~iv in
  let ks = Zuc.keystream_words z 2 in
  Alcotest.(check string) "word0" "27bede74" (hex_of_int32 ks.(0));
  Alcotest.(check string) "word1" "018082da" (hex_of_int32 ks.(1))

let test_ff_vector () =
  let key = Bytes.make 16 '\255' in
  let iv = Bytes.make 16 '\255' in
  let z = Zuc.init ~key ~iv in
  let ks = Zuc.keystream_words z 2 in
  Alcotest.(check string) "word0" "0657cfa0" (hex_of_int32 ks.(0));
  Alcotest.(check string) "word1" "7096398b" (hex_of_int32 ks.(1))

let test_crypt_roundtrip () =
  let key = bytes_of_hex "00112233445566778899aabbccddeeff" in
  let iv = bytes_of_hex "ffeeddccbbaa99887766554433221100" in
  let plain = Bytes.of_string "hello zuc stream cipher" in
  let cipher = Zuc.crypt_with_key ~key ~iv plain in
  let back = Zuc.crypt_with_key ~key ~iv cipher in
  Alcotest.(check string) "roundtrip" (Bytes.to_string plain) (Bytes.to_string back)

let () =
  Alcotest.run "zuc"
    [
      ("zuc", [
           Alcotest.test_case "zero vector" `Quick test_zero_vector;
           Alcotest.test_case "ff vector" `Quick test_ff_vector;
           Alcotest.test_case "crypt roundtrip" `Quick test_crypt_roundtrip;
         ]);
    ]
