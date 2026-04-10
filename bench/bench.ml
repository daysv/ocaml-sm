open Printf

let ( let* ) = Option.bind

module Benchmark = struct
  type result = {
    name : string;
    ops : int;
    total_time_ns : int;
    throughput_mbps : float;
    ops_per_sec : float;
    time_per_op_ns : float;
  }

  let run ~name ~iterations ~data_size f =
    let start = Unix.gettimeofday () in
    for _ = 1 to iterations do
      f ()
    done;
    let finish = Unix.gettimeofday () in
    let total_time_ns = int_of_float ((finish -. start) *. 1_000_000_000.0) in
    let total_bytes = Int64.of_int (iterations * data_size) in
    let throughput_mbps =
      let total_mb = Int64.to_float total_bytes /. (1024.0 *. 1024.0) in
      let total_sec = Float.of_int total_time_ns /. 1_000_000_000.0 in
      total_mb /. total_sec
    in
    let ops_per_sec = Float.of_int iterations /. (Float.of_int total_time_ns /. 1_000_000_000.0) in
    let time_per_op_ns = Float.of_int total_time_ns /. Float.of_int iterations in
    {
      name;
      ops = iterations;
      total_time_ns;
      throughput_mbps;
      ops_per_sec;
      time_per_op_ns;
    }

  let print_result r =
    printf "  %-30s | %12.2f ops/s | %12.2f ns/op | %12.2f MB/s\n"
      r.name r.ops_per_sec r.time_per_op_ns r.throughput_mbps

  let print_header () =
    printf "\n%-32s | %14s | %14s | %14s\n" "Operation" "Ops/sec" "Time/op" "Throughput";
    printf "%s\n" (String.make 80 '-')
end

module Hex = struct
  let hex_value = function
    | '0'..'9' as c -> Char.code c - Char.code '0'
    | 'a'..'f' as c -> 10 + Char.code c - Char.code 'a'
    | 'A'..'F' as c -> 10 + Char.code c - Char.code 'A'
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
end

let () =
  printf "SM3/SM4 Benchmark\n";
  printf "=================\n";

  (* SM3 Benchmarks *)
  printf "\n[SM3 Hash Benchmarks]\n";
  Benchmark.print_header ();

  let test_data_small = String.make 64 'a' in
  let test_data_1kb = String.make 1024 'b' in
  let test_data_64kb = String.make (64 * 1024) 'c' in
  let test_data_1mb = String.make (1024 * 1024) 'd' in

  let r1 = Benchmark.run ~name:"SM3 digest (64B)" ~iterations:10_000 ~data_size:64
    (fun () -> ignore (Sm3.digest_string test_data_small)) in
  Benchmark.print_result r1;

  let r2 = Benchmark.run ~name:"SM3 digest (1KB)" ~iterations:5_000 ~data_size:1024
    (fun () -> ignore (Sm3.digest_string test_data_1kb)) in
  Benchmark.print_result r2;

  let r3 = Benchmark.run ~name:"SM3 digest (64KB)" ~iterations:500 ~data_size:(64*1024)
    (fun () -> ignore (Sm3.digest_string test_data_64kb)) in
  Benchmark.print_result r3;

  let r4 = Benchmark.run ~name:"SM3 digest (1MB)" ~iterations:50 ~data_size:(1024*1024)
    (fun () -> ignore (Sm3.digest_string test_data_1mb)) in
  Benchmark.print_result r4;

  (* SM4 Block Cipher Benchmarks *)
  printf "\n[SM4 Block Cipher Benchmarks]\n";
  Benchmark.print_header ();

  let sm4_key = Hex.bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let sm4_plain = Hex.bytes_of_hex "0123456789abcdeffedcba9876543210" in
  let sm4_cipher = ref (Bytes.create 16) in

  let r5 = Benchmark.run ~name:"SM4 encrypt (16B block)" ~iterations:10_000 ~data_size:16
    (fun () -> sm4_cipher := Sm4.encrypt_block_with_key sm4_key sm4_plain) in
  Benchmark.print_result r5;

  let r6 = Benchmark.run ~name:"SM4 decrypt (16B block)" ~iterations:10_000 ~data_size:16
    (fun () -> ignore (Sm4.decrypt_block_with_key sm4_key !sm4_cipher)) in
  Benchmark.print_result r6;

  (* Pre-generate round keys *)
  let round_keys = Sm4.key_schedule sm4_key in
  let r7 = Benchmark.run ~name:"SM4 encrypt (pre-keyed)" ~iterations:10_000 ~data_size:16
    (fun () -> ignore (Sm4.encrypt_block round_keys sm4_plain)) in
  Benchmark.print_result r7;

  let r8 = Benchmark.run ~name:"SM4 decrypt (pre-keyed)" ~iterations:10_000 ~data_size:16
    (fun () -> ignore (Sm4.decrypt_block round_keys !sm4_cipher)) in
  Benchmark.print_result r8;

  (* SM4 CBC Mode Benchmarks *)
  printf "\n[SM4 CBC Mode Benchmarks]\n";
  Benchmark.print_header ();

  let iv = Hex.bytes_of_hex "000102030405060708090a0b0c0d0e0f" in
  let cbc_plain = Bytes.make (16 * 1000) 'x' in
  let cbc_cipher = ref (Bytes.create 0) in

  let r9 = Benchmark.run ~name:"SM4 CBC encrypt (16KB)" ~iterations:500 ~data_size:(16*1000)
    (fun () -> cbc_cipher := Sm4.Cbc.encrypt_no_pad ~key:sm4_key ~iv cbc_plain) in
  Benchmark.print_result r9;

  let r10 = Benchmark.run ~name:"SM4 CBC decrypt (16KB)" ~iterations:500 ~data_size:(16*1000)
    (fun () -> ignore (Sm4.Cbc.decrypt_no_pad ~key:sm4_key ~iv !cbc_cipher)) in
  Benchmark.print_result r10;

  let cbc_plain_64kb = Bytes.make (16 * 4000) 'y' in
  let r11 = Benchmark.run ~name:"SM4 CBC encrypt (64KB)" ~iterations:100 ~data_size:(16*4000)
    (fun () -> cbc_cipher := Sm4.Cbc.encrypt_no_pad ~key:sm4_key ~iv cbc_plain_64kb) in
  Benchmark.print_result r11;

  let r12 = Benchmark.run ~name:"SM4 CBC decrypt (64KB)" ~iterations:100 ~data_size:(16*4000)
    (fun () -> ignore (Sm4.Cbc.decrypt_no_pad ~key:sm4_key ~iv !cbc_cipher)) in
  Benchmark.print_result r12;

  (* SM4 CTR Mode Benchmarks *)
  printf "\n[SM4 CTR Mode Benchmarks]\n";
  Benchmark.print_header ();

  let ctr_plain = Bytes.make (16 * 1000) 'z' in
  let r13 = Benchmark.run ~name:"SM4 CTR crypt (16KB)" ~iterations:500 ~data_size:(16*1000)
    (fun () -> ignore (Sm4.Ctr.crypt ~key:sm4_key ~iv ctr_plain)) in
  Benchmark.print_result r13;

  let ctr_plain_1mb = Bytes.make (1024 * 1024) 'w' in
  let r14 = Benchmark.run ~name:"SM4 CTR crypt (1MB)" ~iterations:10 ~data_size:(1024*1024)
    (fun () -> ignore (Sm4.Ctr.crypt ~key:sm4_key ~iv ctr_plain_1mb)) in
  Benchmark.print_result r14;

  (* SM4 GCM Mode Benchmarks *)
  printf "\n[SM4 GCM Mode Benchmarks]\n";
  Benchmark.print_header ();

  let gcm_iv = Bytes.make 12 '\x00' in
  let gcm_aad = Bytes.of_string "additional data" in
  let gcm_plain_16kb = Bytes.make (16 * 1000) 'v' in
  let gcm_out = ref { Sm4.Gcm.ciphertext = Bytes.empty; tag = Bytes.empty } in

  let r15 = Benchmark.run ~name:"SM4 GCM encrypt (16KB)" ~iterations:200 ~data_size:(16*1000)
    (fun () -> gcm_out := Sm4.Gcm.encrypt ~key:sm4_key ~iv:gcm_iv ~aad:gcm_aad gcm_plain_16kb) in
  Benchmark.print_result r15;

  let r16 = Benchmark.run ~name:"SM4 GCM decrypt (16KB)" ~iterations:200 ~data_size:(16*1000)
    (fun () -> ignore (Sm4.Gcm.decrypt ~key:sm4_key ~iv:gcm_iv ~aad:gcm_aad ~tag:!gcm_out.tag !gcm_out.ciphertext)) in
  Benchmark.print_result r16;

  let gcm_plain_64kb = Bytes.make (16 * 4000) 'u' in
  let r17 = Benchmark.run ~name:"SM4 GCM encrypt (64KB)" ~iterations:50 ~data_size:(16*4000)
    (fun () -> gcm_out := Sm4.Gcm.encrypt ~key:sm4_key ~iv:gcm_iv ~aad:gcm_aad gcm_plain_64kb) in
  Benchmark.print_result r17;

  let r18 = Benchmark.run ~name:"SM4 GCM decrypt (64KB)" ~iterations:50 ~data_size:(16*4000)
    (fun () -> ignore (Sm4.Gcm.decrypt ~key:sm4_key ~iv:gcm_iv ~aad:gcm_aad ~tag:!gcm_out.tag !gcm_out.ciphertext)) in
  Benchmark.print_result r18;

  (* SM2 Benchmarks *)
  printf "\n[SM2 Benchmarks]\n";
  Benchmark.print_header ();

  (* Generate key pair for SM2 benchmarks *)
  let priv_key = Sm2.scalar_of_hex "128B4514A6D3AA93F6C1C3F3E5D3C8F9A8B7C6D5E4F3A2B1C0D9E8F7A6B5C4D3" in
  let pub_key = Sm2.derive_public_key priv_key in
  let test_message = "Hello, SM2 benchmark test message!" in
  let test_digest = String.make 32 '\xab' in

  (* SM2 Sign/Verify *)
  let r19 = Benchmark.run ~name:"SM2 sign (fixed digest)" ~iterations:1_000 ~data_size:32
    (fun () -> ignore (Sm2.sign_digest ~k:priv_key ~priv:priv_key ~digest:test_digest)) in
  Benchmark.print_result r19;

  let signature = Sm2.sign_digest ~k:priv_key ~priv:priv_key ~digest:test_digest in
  let r20 = Benchmark.run ~name:"SM2 verify (fixed digest)" ~iterations:1_000 ~data_size:32
    (fun () -> ignore (Sm2.verify_digest ~pub:pub_key ~digest:test_digest ~signature)) in
  Benchmark.print_result r20;

  (* SM2 Encrypt/Decrypt *)
  let r21 = Benchmark.run ~name:"SM2 encrypt (64B)" ~iterations:1_000 ~data_size:64
    (fun () -> ignore (Sm2.encrypt ~k:priv_key ~pub:pub_key test_message)) in
  Benchmark.print_result r21;

  let encrypted = Sm2.encrypt ~k:priv_key ~pub:pub_key test_message in
  let r22 = Benchmark.run ~name:"SM2 decrypt (64B)" ~iterations:1_000 ~data_size:64
    (fun () -> ignore (Sm2.decrypt ~priv:priv_key encrypted)) in
  Benchmark.print_result r22;

  let long_message = String.make 1024 't' in
  let r23 = Benchmark.run ~name:"SM2 encrypt (1KB)" ~iterations:500 ~data_size:1024
    (fun () -> ignore (Sm2.encrypt ~k:priv_key ~pub:pub_key long_message)) in
  Benchmark.print_result r23;

  let encrypted_long = Sm2.encrypt ~k:priv_key ~pub:pub_key long_message in
  let r24 = Benchmark.run ~name:"SM2 decrypt (1KB)" ~iterations:500 ~data_size:1024
    (fun () -> ignore (Sm2.decrypt ~priv:priv_key encrypted_long)) in
  Benchmark.print_result r24;

  printf "\nBenchmark completed.\n"