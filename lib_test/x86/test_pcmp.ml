
(**
# rappel (don't forget -x option)


## pcmpeqb, pcmpgtb, pcmpeqw, pcmpgtw ...  instructions

move to xmm0 register a 128-bit value "0f 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 00"
move to xmm1 register a 128-bit value "0f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"

    mov rax, 0x0706050403020100
    mov rbx, 0x0f0e0d0c0b0a0908
    pinsrq xmm0, rax, 0
    pinsrq xmm0, rbx, 1

    mov rax, 0x0000000000000000
    mov rbx, 0x0f00000000000000
    pinsrq xmm1, rax, 0
    pinsrq xmm1, rbx, 1

Cases:
   1) pcmpeqb xmm1, xmm0
      expected: first and last bytes in xmm1 should be 0xFF

   2) pcmpgtb xmm1, xmm0
      expected: first and last bytes in xmm1 should be 0x00,
      all others bytes should be 0xFF

## pminsb, pminub, pmaxsb, pmaxub, pminsw ... instructions

move to xmm0 register a 128-bit value "0f 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 00"
move to xmm1 register a 128-bit value "ff 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ff"

    mov rax, 0x0706050403020100
    mov rbx, 0x0f0e0d0c0b0a0908
    pinsrq xmm0, rax, 0
    pinsrq xmm0, rbx, 1

    mov rax, 0x00000000000000ff
    mov rbx, 0xff00000000000000
    pinsrq xmm1, rax, 0
    pinsrq xmm1, rbx, 1

Cases:
   1) pminub xmm1, xmm0
      expected: first byte in xmm1 should be 0x0f
   2) pminsb xmm1, xmm0
      expected: xmm1 stay unchanged
   3) pmaxub xmm1, xmm0
      expected: all bytes in xmm1 should be equal
      to corresponded bytes in xmm0, except first and last
      bytes, which stay unchanchged
   4) pmaxsb xmm1, xmm0
      expected: xmm1 = xmm0

*)

open Core_kernel.Std
open Bap.Std
open OUnit2

module Dis = Disasm_expert.Basic
module Env = X86_env.R32

let arch  = `x86
let width = 32

let of_bytes s =
  let str = String.filter ~f:(fun c -> Char.(c <> ' ')) s in
  Word.of_string @@ sprintf "0x%s:256u" str

let insn_bil x =
  let bytes = Bigstring.of_string x in
  let mem = Or_error.ok_exn @@
    Memory.create LittleEndian (Word.zero 32) bytes in
  let mem, insn =
    Or_error.ok_exn @@
    Dis.with_disasm ~backend:"llvm" (Arch.to_string arch) ~f:(fun dis ->
        let dis = Dis.store_asm dis |> Dis.store_kinds in
        match Dis.insn_of_mem dis mem with
        | Ok (mem', Some insn, `finished) -> Ok (mem', insn)
        | _ -> Error (Error.of_string "invalid insn")) in
  let module T = (val (target_of_arch arch)) in
  Or_error.ok_exn @@ T.lift mem insn

let test insn_name bytes x y expected _ctxt =
  let xmm0 = Env.ymms.(0) in
  let xmm1 = Env.ymms.(1) in
  let bil = Bil.[
      move xmm0 (int x);
      move xmm1 (int y);
    ] @ insn_bil bytes in
  let c = Stmt.eval bil (new Bili.context) in
  assert_bool ("got wrong result for " ^ insn_name) @@
  match c#lookup xmm1 with
  | None -> false
  | Some r ->
    match Bil.Result.value r with
    | Bil.Imm word -> Word.equal word expected
    | _ -> false

let test_eqb =
  let pcmpeqb = "\x66\x0f\x74\xc8" in (** pcmpeqb %xmm1, %xmm0 *)
  let x = of_bytes "0f 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 00" in
  let y = of_bytes "0f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00" in
  let e = of_bytes "ff 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ff" in
  test "pcmpeqb" pcmpeqb x y e

let test_gtb =
  let pcmpgtb = "\x66\x0f\x64\xc8" in (** pcmpgtb %xmm1, %xmm0 *)
  let x = of_bytes "0f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00" in
  let y = of_bytes "0f 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 00" in
  let e = of_bytes "00 ff ff ff ff ff ff ff ff ff ff ff ff ff ff 00" in
  test "pcmpgtb" pcmpgtb x y e

let test_pmin_pmax name bytes expected =
  let x = of_bytes "0f 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 00" in
  let y = of_bytes "ff 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ff" in
  test name bytes x y expected

let test_pminub =
  let bytes = "\x66\x0f\xda\xc8" in (** pminub %xmm0, %xmm1 *)
  let expected = of_bytes "0f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00" in
  test_pmin_pmax "pminub" bytes expected

let test_pminsb =
  let bytes = "\x66\x0f\x38\x38\xc8" in  (** pminsb %xmm0, %xmm1 *)
  let expected = of_bytes "ff 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ff" in
  test_pmin_pmax "pminsb" bytes expected

let test_pmaxub =
  let bytes = "\x66\x0f\xde\xc8" in (** pmaxub %xmm0, %xmm1 *)
  let expected = of_bytes "ff 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 ff" in
  test_pmin_pmax "pmaxub" bytes expected

let test_pmaxsb =
  let bytes = "\x66\x0f\x38\x3c\xc8" in (** pmaxsb %xmm0, %xmm1 *)
  let expected = of_bytes "0f 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 00" in
  test_pmin_pmax "pmaxsb" bytes expected

let suite = "pcmp" >::: [
    "pcmpeqb"   >:: test_eqb;
    "pcmpgtb"   >:: test_gtb;
    "pminub"    >:: test_pminub;
    "pminsb"    >:: test_pminsb;
    "pmaxub"    >:: test_pmaxub;
    "pmaxsb"    >:: test_pmaxsb;
  ]
