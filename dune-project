(lang dune 3.22)

(name sm)

(generate_opam_files true)

(package
 (name sm)
 (synopsis "纯 OCaml 国密算法库 (SM2/SM3/SM4/ZUC)")
 (description
  "纯 OCaml 国密算法库，支持 SM2（密钥派生、签名验签、加解密、密钥交换、DER/PEM/ASN.1 编解码）、SM3（摘要）、SM4（单块、CBC、CTR、GCM、流式上下文）以及 ZUC-128（密钥流生成与流加解密）")
 (version 0.1.0)
 (license MIT)
 (authors "days_v")
 (maintainers "days_v <days_v@qq.com>")
 (homepage "https://github.com/daysv/ocaml-sm")
 (bug_reports "https://github.com/daysv/ocaml-sm/issues")
 (depends
  (dune
   (>= 3.22))
  (alcotest :with-test)))
