let check_hex name input expected =
  Alcotest.(check string) name expected (Sm3.digest_hex input)

let check_incremental () =
  let ctx = Sm3.init () |> fun t -> Sm3.update_string t "ab" |> fun t -> Sm3.update_string t "c" in
  Alcotest.(check string)
    "incremental abc"
    "66c7f0f462eeedd9d1f2d46bdc10e4e24167c4875cf2f7a2297da02b8f4ba8e0"
    (Sm3.finalize_hex ctx)

let check_subbytes () =
  let src = Bytes.of_string "__abcdefghijklmnopqrstuvwxyz__" in
  let ctx = Sm3.init () |> fun t -> Sm3.update_subbytes t src ~off:2 ~len:26 in
  Alcotest.(check string)
    "subbytes"
    (Sm3.digest_hex "abcdefghijklmnopqrstuvwxyz")
    (Sm3.finalize_hex ctx)

let suite =
  [
    ("sm3", [
         Alcotest.test_case "empty string" `Quick (fun () ->
             check_hex
               "empty"
               ""
               "1ab21d8355cfa17f8e61194831e81a8f22bec8c728fefb747ed035eb5082aa2b");
         Alcotest.test_case "abc vector" `Quick (fun () ->
             check_hex
               "abc"
               "abc"
               "66c7f0f462eeedd9d1f2d46bdc10e4e24167c4875cf2f7a2297da02b8f4ba8e0");
         Alcotest.test_case "64-byte vector" `Quick (fun () ->
             check_hex
               "abcd x16"
               "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
               "debe9ff92275b8a138604889c18e5a4d6fdb70e5387e5765293dcba39c0c5732");
         Alcotest.test_case "long message (512 bits)" `Quick (fun () ->
             check_hex
               "64 zeros"
               "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
               "46b58571be41685c253194d20ec7f82b659cc8c6b753f26d4e9ec85bc91c231e");
         Alcotest.test_case "empty input shape" `Quick (fun () ->
             Alcotest.(check int) "digest length" 64 (String.length (Sm3.digest_hex "")));
         Alcotest.test_case "incremental update" `Quick check_incremental;
         Alcotest.test_case "subbytes update" `Quick check_subbytes;
       ])
  ]

let () = Alcotest.run "sm3" suite
