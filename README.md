# SM - Chinese National Cryptographic Algorithms in Pure OCaml

[![Version](https://img.shields.io/badge/version-0.1.1-blue.svg)](https://github.com/daysv/ocaml-sm)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![OCaml](https://img.shields.io/badge/OCaml-4.14.2%2B-orange.svg)](https://ocaml.org)

A pure OCaml implementation of the Chinese National Cryptographic Algorithm suite, providing SM2 (elliptic curve cryptography), SM3 (cryptographic hash), SM4 (block cipher), and ZUC-128 (stream cipher) algorithms.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [API Overview](#api-overview)
  - [SM2 - Elliptic Curve Cryptography](#sm2---elliptic-curve-cryptography)
  - [SM3 - Cryptographic Hash](#sm3---cryptographic-hash)
  - [SM4 - Block Cipher](#sm4---block-cipher)
  - [ZUC - Stream Cipher](#zuc---stream-cipher)
- [Documentation](#documentation)
- [Testing](#testing)
- [License](#license)

## Features

- **SM2**: Key generation, signature generation/verification, encryption/decryption, key exchange, DER/PEM/ASN.1 encoding and decoding
- **SM3**: Hash digest computation, HMAC, KDF (Key Derivation Function)
- **SM4**: Single-block operations, CBC, CTR, GCM modes, streaming context support
- **ZUC-128**: Keystream generation and stream encryption/decryption
- **Pure OCaml**: No external C dependencies, works across all platforms supported by OCaml

## Installation

### Via OPAM

```bash
opam install sm
```

### From Source

```bash
git clone https://github.com/daysv/ocaml-sm.git
cd ocaml-sm
dune build
```

Add `sm` to your `dune` project dependencies:

```lisp
(executable
 (name my_app)
 (libraries sm))
```

## Quick Start

```ocaml
open Sm

(* SM3 Hash *)
let hash = Sm3.digest_string "Hello, SM!"
let hash_hex = Sm3.digest_hex "Hello, SM!"

(* SM4 Encryption (CBC mode) *)
let key = Bytes.of_string "0123456789abcdef"  (* 16 bytes *)
let iv = Bytes.of_string "0123456789abcdef"   (* 16 bytes *)
let plaintext = Bytes.of_string "Secret message"
let ciphertext = Sm4.Cbc.encrypt_pkcs7 ~key ~iv plaintext
let decrypted = Sm4.Cbc.decrypt_pkcs7 ~key ~iv ciphertext
```

## API Overview

### SM2 - Elliptic Curve Cryptography

SM2 is an elliptic curve cryptographic algorithm based on the SM2 curve parameters, defined in GM/T 0003-2012.

**Key Management**

```ocaml
(* Generate or import private key *)
let scalar = Sm2.scalar_of_hex "..."
let priv_key = Sm2.private_key_of_scalar scalar
let pub_point = Sm2.derive_public_key scalar

(* PEM encoding/decoding *)
let pem = Sm2.encode_private_key_pem `Pkcs8 priv_key
let priv_key' = Sm2.decode_private_key_pem pem
```

**Digital Signature**

```ocaml
let digest = Sm2.digest_for_sign ~id:"1234567812345678" pub_point message
let (r, s) = Sm2.sign_digest ~k:ephemeral_scalar ~priv:scalar ~digest
let valid = Sm2.verify_digest ~pub:pub_point ~digest ~signature:(r, s)
```

**Encryption/Decryption**

```ocaml
let ciphertext = Sm2.encrypt ~k:ephemeral_scalar ~pub:pub_point plaintext
let plaintext = Sm2.decrypt ~priv:scalar ciphertext
```

**Key Exchange**

```ocaml
let result = Sm2.key_exchange
  ~role:`Initiator
  ~self_id:"1234567812345678"
  ~self_static:static_priv
  ~self_ephemeral:ephemeral_scalar
  ~peer_id:"8765432187654321"
  ~peer_static:peer_pub
  ~peer_ephemeral:peer_ephemeral_pub
  ~key_length:16
```

### SM3 - Cryptographic Hash

SM3 produces a 256-bit (32-byte) hash value, specified in GM/T 0004-2012.

**Basic Hashing**

```ocaml
let hash = Sm3.digest_string "message"           (* raw bytes *)
let hash_hex = Sm3.digest_hex "message"          (* hex string *)

(* Incremental hashing *)
let ctx = Sm3.init ()
let ctx = Sm3.update_string ctx "chunk1"
let ctx = Sm3.update_string ctx "chunk2"
let hash = Sm3.finalize ctx
```

**HMAC**

```ocaml
let tag = Sm3.hmac ~key:"secret_key" "message"
let tag_hex = Sm3.hmac_hex ~key:"secret_key" "message"
```

**Key Derivation (KDF)**

```ocaml
let derived_key = Sm3.kdf ~z:shared_secret ~klen:32
```

### SM4 - Block Cipher

SM4 is a 128-bit block cipher with a 128-bit key, defined in GM/T 0002-2012.

**Single Block**

```ocaml
let ciphertext = Sm4.encrypt_block_with_key key plaintext   (* 16-byte blocks *)
let plaintext = Sm4.decrypt_block_with_key key ciphertext
```

**CBC Mode**

```ocaml
(* With PKCS#7 padding *)
let ciphertext = Sm4.Cbc.encrypt_pkcs7 ~key ~iv plaintext
let plaintext = Sm4.Cbc.decrypt_pkcs7 ~key ~iv ciphertext

(* No padding *)
let ciphertext = Sm4.Cbc.encrypt_no_pad ~key ~iv plaintext
```

**CTR Mode**

```ocaml
let ciphertext = Sm4.Ctr.crypt ~key ~iv plaintext
(* Encryption and decryption are identical in CTR mode *)
let plaintext = Sm4.Ctr.crypt ~key ~iv ciphertext
```

**GCM Mode** (Authenticated Encryption)

```ocaml
let result = Sm4.Gcm.encrypt ~key ~iv ~aad:"additional_data" plaintext
let ciphertext = result.Sm4.Gcm.ciphertext
let tag = result.Sm4.Gcm.tag

let plaintext = Sm4.Gcm.decrypt ~key ~iv ~aad:"additional_data" ~tag ciphertext
```

**Streaming API**

```ocaml
(* CBC encryption with streaming *)
module Stream = Sm4.Stream.Cbc_encrypt
let ctx = Stream.init ~key ~iv ~padding:`Pkcs7
Stream.update ctx chunk1
Stream.update ctx chunk2
let final_block = Stream.finalize ctx
```

### ZUC - Stream Cipher

ZUC is a word-oriented stream cipher that produces a 32-bit keystream word per call, specified in GM/T 0001-2012.

**Stream Encryption**

```ocaml
let ctx = Zuc.init ~key ~iv
let keystream_word = Zuc.next_word ctx
let keystream_words = Zuc.keystream_words ctx 10

(* Encrypt/decrypt bytes *)
let ciphertext = Zuc.crypt ctx plaintext

(* One-shot encryption *)
let ciphertext = Zuc.crypt_with_key ~key ~iv plaintext
```

## Documentation

Full API documentation can be generated using `odoc`:

```bash
dune build @doc
```

The generated HTML documentation will be available in `_build/default/_doc/_html/`.

## Testing

Run the test suite:

```bash
dune runtest
```

Test files are located in the `test/` directory, covering:
- SM2 cryptographic operations
- SM3 hash and HMAC
- SM4 block cipher modes
- ZUC stream cipher

## License

MIT License - see the [LICENSE](LICENSE) file for details.
