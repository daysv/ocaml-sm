let priv = Sm2.scalar_of_hex "128B2FA8BD433C6C068C8D803DFF79792A519A55171B1B650C23661D15897263"
let k = Sm2.scalar_of_hex "6CB28D993E6D4F2773B8C2D8EE4D646C4C92A415F7F5C7B8E2C6B1B43F5C4D4E"

let sample_pkcs8_pem =
  "-----BEGIN PRIVATE KEY-----\n\
MIGHAgEAMBMGByqGSM49AgEGCCqBHM9VAYItBG0wawIBAQQgWKhkuODUI4X4vJNU\n\
+ZY/SSAZprg6aTmjPpYYNJlp9quhRANCAAQBjFJfQ2vAShuv8ZpxvUHyvjKIKKN2\n\
6D1iH7YrWznbZF8BGz2si+SlpeJAy2eWcqh7877LruQb5B8uT+bUryJR\n\
-----END PRIVATE KEY-----\n"

let sample_sec1_pem =
  "-----BEGIN EC PRIVATE KEY-----\n\
MHcCAQEEIFioZLjg1COF+LyTVPmWP0kgGaa4Omk5oz6WGDSZafaroAoGCCqBHM9V\n\
AYItoUQDQgAEAYxSX0NrwEobr/Gacb1B8r4yiCijdug9Yh+2K1s522RfARs9rIvk\n\
paXiQMtnlnKoe/O+y67kG+QfLk/m1K8iUQ==\n\
-----END EC PRIVATE KEY-----\n"

let sample_public_pem =
  "-----BEGIN PUBLIC KEY-----\n\
MFkwEwYHKoZIzj0CAQYIKoEcz1UBgi0DQgAEAYxSX0NrwEobr/Gacb1B8r4yiCij\n\
dug9Yh+2K1s522RfARs9rIvkpaXiQMtnlnKoe/O+y67kG+QfLk/m1K8iUQ==\n\
-----END PUBLIC KEY-----\n"

let sample_sec1_der_hex =
  "3077020101042058a864b8e0d42385f8bc9354f9963f492019a6b83a6939a33e9618349969f6aba00a06082a811ccf5501822da14403420004018c525f436bc04a1baff19a71bd41f2be328828a376e83d621fb62b5b39db645f011b3dac8be4a5a5e240cb679672a87bf3becbaee41be41f2e4fe6d4af2251"

let sample_pkcs8_der_hex =
  "308187020100301306072a8648ce3d020106082a811ccf5501822d046d306b020101042058a864b8e0d42385f8bc9354f9963f492019a6b83a6939a33e9618349969f6aba14403420004018c525f436bc04a1baff19a71bd41f2be328828a376e83d621fb62b5b39db645f011b3dac8be4a5a5e240cb679672a87bf3becbaee41be41f2e4fe6d4af2251"

let sample_public_der_hex =
  "3059301306072a8648ce3d020106082a811ccf5501822d03420004018c525f436bc04a1baff19a71bd41f2be328828a376e83d621fb62b5b39db645f011b3dac8be4a5a5e240cb679672a87bf3becbaee41be41f2e4fe6d4af2251"

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
  Bytes.unsafe_to_string out

let hex_of_string s =
  let out = Bytes.create (String.length s * 2) in
  let hex = "0123456789abcdef" in
  String.iteri
    (fun i c ->
      let v = Char.code c in
      Bytes.set out (2 * i) hex.[v lsr 4];
      Bytes.set out ((2 * i) + 1) hex.[v land 0xf])
    s;
  Bytes.unsafe_to_string out

let test_public_key () =
  let pub = Sm2.derive_public_key priv in
  let x, y = Sm2.point_to_hex pub in
  Alcotest.(check string)
    "pub x"
    "d5548c7825cbb56150a3506cd57464af8a1ae0519dfaf3c58221dc810caf28dd"
    x;
  Alcotest.(check string)
    "pub y"
    "921073768fe3d59ce54e79a49445cf73fed23086537027264d168946d479533e"
    y

let test_sign_verify () =
  let pub = Sm2.derive_public_key priv in
  let digest = Sm3.digest_string "message digest" in
  let r, s = Sm2.sign_digest ~k ~priv ~digest in
  Alcotest.(check string)
    "sig r"
    "f77c1eaf3858bf4e440631d5d13b1ebfbe9ae1a50e41c9a94733d8890cfa708f"
    (Sm2.scalar_to_hex r);
  Alcotest.(check string)
    "sig s"
    "7663ec69b44ee98a91dde8abcf339e5c71d584bcc410fd668ff9406b0a9f4efa"
    (Sm2.scalar_to_hex s);
  Alcotest.(check bool) "verify" true (Sm2.verify_digest ~pub ~digest ~signature:(r, s));
  Alcotest.(check bool)
    "verify altered digest"
    false
    (Sm2.verify_digest ~pub ~digest:(Sm3.digest_string "message digesu") ~signature:(r, s))

let test_encrypt_decrypt () =
  let pub = Sm2.derive_public_key priv in
  let cipher = Sm2.encrypt ~k ~pub "hello sm2" in
  Alcotest.(check int) "cipher length" 106 (String.length cipher);
  Alcotest.(check string)
    "cipher sm3"
    "0d4e1af929831e30426b70e51be57b7a5915fe29325bc88ec2ae2f181fc9c90e"
    (Sm3.finalize_hex (Sm3.update_string Sm3.empty cipher));
  Alcotest.(check (option string))
    "decrypt"
    (Some "hello sm2")
    (Sm2.decrypt ~priv cipher)

let test_signature_der () =
  let digest = Sm3.digest_string "message digest" in
  let signature = Sm2.sign_digest ~k ~priv ~digest in
  let der = Sm2.encode_signature_der signature in
  Alcotest.(check string)
    "signature der"
    "3045022100f77c1eaf3858bf4e440631d5d13b1ebfbe9ae1a50e41c9a94733d8890cfa708f02207663ec69b44ee98a91dde8abcf339e5c71d584bcc410fd668ff9406b0a9f4efa"
    (hex_of_string der);
  Alcotest.(check (option string))
    "signature der roundtrip"
    (Some (hex_of_string der))
    (Option.map (fun sigv -> hex_of_string (Sm2.encode_signature_der sigv)) (Sm2.decode_signature_der der))

let test_der_pem_compat () =
  let key = Option.get (Sm2.decode_private_key_pkcs8_der (bytes_of_hex sample_pkcs8_der_hex)) in
  Alcotest.(check string)
    "pkcs8 der exact"
    sample_pkcs8_der_hex
    (hex_of_string (Sm2.encode_private_key_pkcs8_der key));
  Alcotest.(check string)
    "sec1 der exact"
    sample_sec1_der_hex
    (hex_of_string (Sm2.encode_private_key_sec1_der key));
  Alcotest.(check string)
    "pkcs8 pem exact"
    sample_pkcs8_pem
    (Sm2.encode_private_key_pem `Pkcs8 key);
  Alcotest.(check string)
    "sec1 pem exact"
    sample_sec1_pem
    (Sm2.encode_private_key_pem `Sec1 key);
  let pub = Option.get (Sm2.decode_public_key_der (bytes_of_hex sample_public_der_hex)) in
  Alcotest.(check string)
    "public der exact"
    sample_public_der_hex
    (hex_of_string (Sm2.encode_public_key_der pub));
  Alcotest.(check string)
    "public pem exact"
    sample_public_pem
    (Sm2.encode_public_key_pem pub);
  Alcotest.(check bool) "decode pkcs8 pem" true (Option.is_some (Sm2.decode_private_key_pem sample_pkcs8_pem));
  Alcotest.(check bool) "decode sec1 pem" true (Option.is_some (Sm2.decode_private_key_pem sample_sec1_pem));
  Alcotest.(check bool) "decode public pem" true (Option.is_some (Sm2.decode_public_key_pem sample_public_pem))

let test_encrypted_private_key_pem () =
  let key = Sm2.private_key_of_scalar priv in
  let salt = Bytes.of_string "12345678salt1234" in
  let iv = Bytes.of_string "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f" in
  let pem = Sm2.encode_private_key_encrypted_pem ~password:"secret" ~salt ~iv ~iterations:4096 key in
  Alcotest.(check bool)
    "encrypted pem label"
    true
    (String.starts_with ~prefix:"-----BEGIN ENCRYPTED PRIVATE KEY-----\n" pem);
  let decoded = Sm2.decode_private_key_encrypted_pem ~password:"secret" pem in
  Alcotest.(check bool) "encrypted pem decode" true (Option.is_some decoded);
  Alcotest.(check bool) "encrypted pem bad password" false (Option.is_some (Sm2.decode_private_key_encrypted_pem ~password:"wrong" pem))

let test_top_module () =
  Alcotest.(check string) "top sm3" "66c7f0f462eeedd9d1f2d46bdc10e4e24167c4875cf2f7a2297da02b8f4ba8e0" (Sm.Sm3.digest_hex "abc");
  let pub = Sm.Sm2.derive_public_key priv in
  let x, _ = Sm.Sm2.point_to_hex pub in
  Alcotest.(check string)
    "top sm2"
    "d5548c7825cbb56150a3506cd57464af8a1ae0519dfaf3c58221dc810caf28dd"
    x

let () =
  Alcotest.run "sm2"
    [
      ("sm2", [
           Alcotest.test_case "derive public key" `Quick test_public_key;
           Alcotest.test_case "sign verify" `Quick test_sign_verify;
           Alcotest.test_case "encrypt decrypt" `Quick test_encrypt_decrypt;
           Alcotest.test_case "signature der" `Quick test_signature_der;
           Alcotest.test_case "der pem compat" `Quick test_der_pem_compat;
           Alcotest.test_case "encrypted private key pem" `Quick test_encrypted_private_key_pem;
           Alcotest.test_case "top module" `Quick test_top_module;
         ]);
    ]
