let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> invalid_arg "hex"

let bytes_of_hex s =
  if String.length s mod 2 <> 0 then invalid_arg "bytes_of_hex";
  let out = Bytes.create (String.length s / 2) in
  for i = 0 to Bytes.length out - 1 do
    let hi = hex_value s.[2 * i] in
    let lo = hex_value s.[(2 * i) + 1] in
    Bytes.set out i (Char.chr ((hi lsl 4) lor lo))
  done;
  out

let hex_of_bytes b =
  let hex = "0123456789abcdef" in
  let out = Bytes.create (Bytes.length b * 2) in
  Bytes.iteri
    (fun i ch ->
      let v = Char.code ch in
      Bytes.set out (2 * i) hex.[v lsr 4];
      Bytes.set out ((2 * i) + 1) hex.[v land 0xf])
    b;
  Bytes.unsafe_to_string out

let test_standard_vector () =
  let key = bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let plain = bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let cipher = Sm4.encrypt_block_with_key key plain in
  Alcotest.(check string)
    "sm4 encrypt"
    "681edf34d206965e86b3e94f536e4246"
    (hex_of_bytes cipher);
  Alcotest.(check string)
    "sm4 decrypt"
    (hex_of_bytes plain)
    (hex_of_bytes (Sm4.decrypt_block_with_key key cipher))

let test_million_round_vector () =
  let key = bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let rec loop n block =
    if n = 0 then block else loop (n - 1) (Sm4.encrypt_block_with_key key block)
  in
  let out = loop 1_000_000 (bytes_of_hex "0123456789abcdeffedcba9876543210") in
  Alcotest.(check string)
    "sm4 million rounds"
    "595298c7c6fd271f0402f804c33d3f66"
    (hex_of_bytes out)

let test_cbc_vectors () =
  let key = bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let iv = bytes_of_hex "000102030405060708090a0b0c0d0e0f" in
  let plain = bytes_of_hex "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff" in
  let cipher = Sm4.Cbc.encrypt_no_pad ~key ~iv plain in
  Alcotest.(check string)
    "cbc encrypt"
    "4691e99a3261b6144f6aa68bea48dbbd16e96658f113bf22cbe041bb87f2e011"
    (hex_of_bytes cipher);
  Alcotest.(check string)
    "cbc decrypt"
    (hex_of_bytes plain)
    (hex_of_bytes (Sm4.Cbc.decrypt_no_pad ~key ~iv cipher))

let test_cbc_pkcs7 () =
  let key = bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let iv = bytes_of_hex "000102030405060708090a0b0c0d0e0f" in
  let plain = Bytes.of_string "hello sm4 cbc" in
  let cipher = Sm4.Cbc.encrypt_pkcs7 ~key ~iv plain in
  Alcotest.(check (option string))
    "cbc pkcs7"
    (Some "hello sm4 cbc")
    (Option.map Bytes.to_string (Sm4.Cbc.decrypt_pkcs7 ~key ~iv cipher))

let test_ctr_vector () =
  let key = bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let iv = bytes_of_hex "000102030405060708090a0b0c0d0e0f" in
  let plain = bytes_of_hex "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff" in
  let cipher = Sm4.Ctr.crypt ~key ~iv plain in
  Alcotest.(check string)
    "ctr encrypt"
    "0689be5279f30edaa2145d392d7517956f162f7804f69a765208b95e4ddd43e5"
    (hex_of_bytes cipher);
  Alcotest.(check string)
    "ctr decrypt"
    (hex_of_bytes plain)
    (hex_of_bytes (Sm4.Ctr.crypt ~key ~iv cipher))

let test_gcm_vector () =
  let key = bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let iv = bytes_of_hex "000102030405060708090a0b" in
  let aad = bytes_of_hex "feedfacedeadbeeffeedfacedeadbeefabaddad2" in
  let plain = bytes_of_hex "00112233445566778899aabbccddeeff0011223344556677" in
  let out = Sm4.Gcm.encrypt ~key ~iv ~aad plain in
  Alcotest.(check string)
    "gcm ciphertext"
    "55303aa2f5e4cf68ec192910178188aa88e80b9e0404d8c8"
    (hex_of_bytes out.ciphertext);
  Alcotest.(check string)
    "gcm tag"
    "ff097c44e4db46f55548e8d8022b8f9f"
    (hex_of_bytes out.tag);
  Alcotest.(check (option string))
    "gcm decrypt"
    (Some (hex_of_bytes plain))
    (Option.map hex_of_bytes (Sm4.Gcm.decrypt ~key ~iv ~aad ~tag:out.tag out.ciphertext))

let test_stream_api () =
  let key = bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let iv = bytes_of_hex "000102030405060708090a0b0c0d0e0f" in
  let plain = Bytes.of_string "hello stream api for sm4" in
  let cbc_enc = Sm4.Stream.Cbc_encrypt.init ~key ~iv ~padding:`Pkcs7 in
  Sm4.Stream.Cbc_encrypt.update cbc_enc (Bytes.sub plain 0 7);
  Sm4.Stream.Cbc_encrypt.update cbc_enc (Bytes.sub plain 7 (Bytes.length plain - 7));
  let cbc_cipher = Option.get (Sm4.Stream.Cbc_encrypt.finalize cbc_enc) in
  let cbc_dec = Sm4.Stream.Cbc_decrypt.init ~key ~iv ~padding:`Pkcs7 in
  Sm4.Stream.Cbc_decrypt.update cbc_dec (Bytes.sub cbc_cipher 0 16);
  Sm4.Stream.Cbc_decrypt.update cbc_dec (Bytes.sub cbc_cipher 16 (Bytes.length cbc_cipher - 16));
  Alcotest.(check string)
    "stream cbc"
    (Bytes.to_string plain)
    (Bytes.to_string (Option.get (Sm4.Stream.Cbc_decrypt.finalize cbc_dec)));
  let ctr_ctx = Sm4.Stream.Ctr.init ~key ~iv in
  let ctr_cipher_a = Sm4.Stream.Ctr.update ctr_ctx (Bytes.sub plain 0 5) in
  let ctr_cipher_b = Sm4.Stream.Ctr.update ctr_ctx (Bytes.sub plain 5 (Bytes.length plain - 5)) in
  let ctr_cipher = Bytes.cat ctr_cipher_a ctr_cipher_b in
  let ctr_ctx2 = Sm4.Stream.Ctr.init ~key ~iv in
  let ctr_plain_a = Sm4.Stream.Ctr.update ctr_ctx2 (Bytes.sub ctr_cipher 0 9) in
  let ctr_plain_b = Sm4.Stream.Ctr.update ctr_ctx2 (Bytes.sub ctr_cipher 9 (Bytes.length ctr_cipher - 9)) in
  let ctr_plain = Bytes.cat ctr_plain_a ctr_plain_b in
  Alcotest.(check string) "stream ctr" (Bytes.to_string plain) (Bytes.to_string ctr_plain);
  let gcm_e = Sm4.Stream.Gcm_encrypt.init ~key ~iv:(Bytes.sub iv 0 12) in
  Sm4.Stream.Gcm_encrypt.add_aad gcm_e (Bytes.of_string "aad");
  Sm4.Stream.Gcm_encrypt.update gcm_e (Bytes.of_string "hello ");
  Sm4.Stream.Gcm_encrypt.update gcm_e (Bytes.of_string "gcm");
  let gcm_out = Sm4.Stream.Gcm_encrypt.finalize gcm_e in
  let gcm_d = Sm4.Stream.Gcm_decrypt.init ~key ~iv:(Bytes.sub iv 0 12) in
  Sm4.Stream.Gcm_decrypt.add_aad gcm_d (Bytes.of_string "aad");
  Sm4.Stream.Gcm_decrypt.update gcm_d (Bytes.sub gcm_out.ciphertext 0 4);
  Sm4.Stream.Gcm_decrypt.update gcm_d (Bytes.sub gcm_out.ciphertext 4 (Bytes.length gcm_out.ciphertext - 4));
  Alcotest.(check (option string))
    "stream gcm"
    (Some "hello gcm")
    (Option.map Bytes.to_string (Sm4.Stream.Gcm_decrypt.finalize gcm_d ~tag:gcm_out.tag))

let () =
  Alcotest.run "sm4"
    [
      ("sm4", [
           Alcotest.test_case "standard vector" `Quick test_standard_vector;
           Alcotest.test_case "million rounds" `Quick test_million_round_vector;
           Alcotest.test_case "cbc vectors" `Quick test_cbc_vectors;
           Alcotest.test_case "cbc pkcs7" `Quick test_cbc_pkcs7;
           Alcotest.test_case "ctr vector" `Quick test_ctr_vector;
           Alcotest.test_case "gcm vector" `Quick test_gcm_vector;
           Alcotest.test_case "stream api" `Quick test_stream_api;
         ]);
    ]
