(lang dune 3.22)

(name sm)

(generate_opam_files true)

(package
 (name sm)
 (synopsis
  "Pure OCaml National Cryptographic Algorithm Library (SM2/SM3/SM4/ZUC)")
 (description
  "Pure OCaml National Cryptographic Algorithm Library, supporting SM2 (key derivation, signature generation and verification, encryption and decryption, key exchange, DER/PEM/ASN.1 encoding/decoding), SM3 (digest), SM4 (single-block, CBC, CTR, GCM, streaming context), and ZUC-128 (keystream generation and stream encryption/decryption).")
 (version 0.1.0)
 (license MIT)
 (authors "days_v")
 (maintainers "days_v <days_v@qq.com>")
 (homepage "https://github.com/daysv/ocaml-sm")
 (bug_reports "https://github.com/daysv/ocaml-sm/issues")
 (depends
  (dune
   (>= 3.22))
  (ocaml
   (>= 4.14.2))
  (alcotest :with-test)))

(source
 (github daysv/ocaml-sm))
