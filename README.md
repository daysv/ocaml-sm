# sm

纯 OCaml 国密算法库，当前包含：

- `SM2`：密钥、公钥派生、签名验签、加解密、密钥交换、DER/PEM/ASN.1 编解码
- `SM3`：摘要
- `SM4`：单块、`CBC`、`CTR`、`GCM`，以及流式上下文 API
- `ZUC-128`：密钥流生成与流加解密

## 构建与测试

```sh
dune build @all
dune runtest
```

## 快速示例

```ocaml
let digest_hex = Sm.Sm3.digest_hex "abc"
```

```ocaml
let key = Bytes.of_string "\x01\x23\x45\x67\x89\xab\xcd\xef\xfe\xdc\xba\x98\x76\x54\x32\x10"
let iv = Bytes.of_string "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f"
let plain = Bytes.of_string "hello sm4"
let cipher = Sm.Sm4.Cbc.encrypt_pkcs7 ~key ~iv plain
```

```ocaml
let priv = Sm.Sm2.scalar_of_hex "128B2FA8BD433C6C068C8D803DFF79792A519A55171B1B650C23661D15897263"
let pub = Sm.Sm2.derive_public_key priv
let digest = Sm.Sm3.digest_string "message digest"
let k = Sm.Sm2.scalar_of_hex "6CB28D993E6D4F2773B8C2D8EE4D646C4C92A415F7F5C7B8E2C6B1B43F5C4D4E"
let signature = Sm.Sm2.sign_digest ~k ~priv ~digest
let verified = Sm.Sm2.verify_digest ~pub ~digest ~signature
```

```ocaml
let alice = Sm.Sm2.private_key_of_scalar priv
let bob =
  Sm.Sm2.private_key_of_scalar
    (Sm.Sm2.scalar_of_hex "0F1E2D3C4B5A69788796A5B4C3D2E1F00123456789ABCDEFFEDCBA9876543210")
let ra = Sm.Sm2.scalar_of_hex "1234567890ABCDEFFEDCBA098765432112233445566778899AABBCCDDEEFF0011"
let rb = Sm.Sm2.scalar_of_hex "2234567890ABCDEFFEDCBA098765432112233445566778899AABBCCDDEEFF0011"
let result =
  Sm.Sm2.key_exchange
    ~role:`Initiator
    ~self_id:"Alice"
    ~self_static:alice
    ~self_ephemeral:ra
    ~peer_id:"Bob"
    ~peer_static:bob.public_point
    ~peer_ephemeral:(Sm.Sm2.ephemeral_public_key rb)
    ~key_length:16
```

```ocaml
let sm4_ctr =
  let ctx = Sm.Sm4.Stream.Ctr.init ~key ~iv in
  let part1 = Sm.Sm4.Stream.Ctr.update ctx (Bytes.of_string "hello ") in
  let part2 = Sm.Sm4.Stream.Ctr.update ctx (Bytes.of_string "world") in
  Bytes.cat part1 part2
```

```ocaml
let key = Sm.Sm2.private_key_of_scalar priv
let salt = Bytes.of_string "12345678salt1234"
let iv = Bytes.of_string "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f"
let pem =
  Sm.Sm2.encode_private_key_encrypted_pem
    ~password:"secret"
    ~salt
    ~iv
    ~iterations:4096
    key
```

```ocaml
let key = Bytes.of_string "\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff"
let iv = Bytes.of_string "\xff\xee\xdd\xcc\xbb\xaa\x99\x88\x77\x66\x55\x44\x33\x22\x11\x00"
let z = Sm.Zuc.init ~key ~iv
let cipher = Sm.Zuc.crypt z (Bytes.of_string "hello zuc")
```
