# SM - 纯 OCaml 实现的中国国家密码算法库

[[版本](https://img.shields.io/badge/版本-0.1.1-blue.svg)](https://github.com/daysv/ocaml-sm)
[[许可证](https://img.shields.io/badge/许可证-MIT-green.svg)](LICENSE)
[[OCaml](https://img.shields.io/badge/OCaml-4.14.2%2B-orange.svg)](https://ocaml.org)

纯 OCaml 实现的中国国家密码算法套件，提供 SM2（椭圆曲线密码）、SM3（密码杂凑）、SM4（分组密码）和 ZUC-128（流密码）算法。

## 目录

- [特性](#特性)
- [安装](#安装)
- [快速开始](#快速开始)
- [API 概览](#api-概览)
  - [SM2 - 椭圆曲线公钥密码算法](#sm2---椭圆曲线公钥密码算法)
  - [SM3 - 密码杂凑算法](#sm3---密码杂凑算法)
  - [SM4 - 分组密码算法](#sm4---分组密码算法)
  - [ZUC - 流密码算法](#zuc---流密码算法)
- [文档](#文档)
- [测试](#测试)
- [许可证](#许可证)

## 特性

- **SM2**：密钥生成、签名生成与验证、加解密、密钥交换、DER/PEM/ASN.1 编解码
- **SM3**：杂凑值计算、HMAC、KDF（密钥派生函数）
- **SM4**：单分组操作、CBC、CTR、GCM 模式、流式上下文支持
- **ZUC-128**：密钥流生成与流式加解密
- **纯 OCaml**：无外部 C 依赖，可在 OCaml 支持的所有平台上运行

## 安装

### 通过 OPAM

```bash
opam install sm
```

### 从源码构建

```bash
git clone https://github.com/daysv/ocaml-sm.git
cd ocaml-sm
dune build
```

在你的 `dune` 项目中添加 `sm` 依赖：

```lisp
(executable
 (name my_app)
 (libraries sm))
```

## 快速开始

```ocaml
open Sm

(* SM3 杂凑 *)
let hash = Sm3.digest_string "Hello, SM!"
let hash_hex = Sm3.digest_hex "Hello, SM!"

(* SM4 加密（CBC 模式） *)
let key = Bytes.of_string "0123456789abcdef"  (* 16 字节 *)
let iv = Bytes.of_string "0123456789abcdef"   (* 16 字节 *)
let plaintext = Bytes.of_string "Secret message"
let ciphertext = Sm4.Cbc.encrypt_pkcs7 ~key ~iv plaintext
let decrypted = Sm4.Cbc.decrypt_pkcs7 ~key ~iv ciphertext
```

## API 概览

### SM2 - 椭圆曲线公钥密码算法

SM2 是基于 SM2 曲线参数的椭圆曲线密码算法，定义于 GM/T 0003-2012。

**密钥管理**

```ocaml
(* 生成或导入私钥 *)
let scalar = Sm2.scalar_of_hex "..."
let priv_key = Sm2.private_key_of_scalar scalar
let pub_point = Sm2.derive_public_key scalar

(* PEM 编解码 *)
let pem = Sm2.encode_private_key_pem `Pkcs8 priv_key
let priv_key' = Sm2.decode_private_key_pem pem
```

**数字签名**

```ocaml
let digest = Sm2.digest_for_sign ~id:"1234567812345678" pub_point message
let (r, s) = Sm2.sign_digest ~k:ephemeral_scalar ~priv:scalar ~digest
let valid = Sm2.verify_digest ~pub:pub_point ~digest ~signature:(r, s)
```

**加解密**

```ocaml
let ciphertext = Sm2.encrypt ~k:ephemeral_scalar ~pub:pub_point plaintext
let plaintext = Sm2.decrypt ~priv:scalar ciphertext
```

**密钥交换**

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

### SM3 - 密码杂凑算法

SM3 输出 256 位（32 字节）的杂凑值，定义于 GM/T 0004-2012。

**基本杂凑**

```ocaml
let hash = Sm3.digest_string "message"           (* 原始字节 *)
let hash_hex = Sm3.digest_hex "message"          (* 十六进制字符串 *)

(* 增量杂凑 *)
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

**密钥派生（KDF）**

```ocaml
let derived_key = Sm3.kdf ~z:shared_secret ~klen:32
```

### SM4 - 分组密码算法

SM4 是分组长度为 128 比特、密钥长度为 128 比特的分组密码算法，定义于 GM/T 0002-2012。

**单分组**

```ocaml
let ciphertext = Sm4.encrypt_block_with_key key plaintext   (* 16 字节分组 *)
let plaintext = Sm4.decrypt_block_with_key key ciphertext
```

**CBC 模式**

```ocaml
(* 使用 PKCS#7 填充 *)
let ciphertext = Sm4.Cbc.encrypt_pkcs7 ~key ~iv plaintext
let plaintext = Sm4.Cbc.decrypt_pkcs7 ~key ~iv ciphertext

(* 无填充 *)
let ciphertext = Sm4.Cbc.encrypt_no_pad ~key ~iv plaintext
```

**CTR 模式**

```ocaml
let ciphertext = Sm4.Ctr.crypt ~key ~iv plaintext
(* CTR 模式下加密与解密操作相同 *)
let plaintext = Sm4.Ctr.crypt ~key ~iv ciphertext
```

**GCM 模式**（认证加密）

```ocaml
let result = Sm4.Gcm.encrypt ~key ~iv ~aad:"additional_data" plaintext
let ciphertext = result.Sm4.Gcm.ciphertext
let tag = result.Sm4.Gcm.tag

let plaintext = Sm4.Gcm.decrypt ~key ~iv ~aad:"additional_data" ~tag ciphertext
```

**流式 API**

```ocaml
(* 流式 CBC 加密 *)
module Stream = Sm4.Stream.Cbc_encrypt
let ctx = Stream.init ~key ~iv ~padding:`Pkcs7
Stream.update ctx chunk1
Stream.update ctx chunk2
let final_block = Stream.finalize ctx
```

### ZUC - 流密码算法

ZUC 是面向字的流密码算法，每次调用产生一个 32 位的密钥流字，定义于 GM/T 0001-2012。

**流式加解密**

```ocaml
let ctx = Zuc.init ~key ~iv
let keystream_word = Zuc.next_word ctx
let keystream_words = Zuc.keystream_words ctx 10

(* 加解密字节 *)
let ciphertext = Zuc.crypt ctx plaintext

(* 一次性加解密 *)
let ciphertext = Zuc.crypt_with_key ~key ~iv plaintext
```

## 文档

可以使用 `odoc` 生成完整的 API 文档：

```bash
dune build @doc
```

生成的 HTML 文档位于 `_build/default/_doc/_html/` 目录。

## 测试

运行测试套件：

```bash
dune runtest
```

测试文件位于 `test/` 目录，覆盖：
- SM2 密码运算
- SM3 杂凑与 HMAC
- SM4 分组密码模式
- ZUC 流密码

## 许可证

MIT 许可证 - 详见 [LICENSE](LICENSE) 文件。
