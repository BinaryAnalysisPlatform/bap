(**
# rappel (don't forget -x option)
## mov to xmm0 register a 128-bit value "50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 44 43 42 41"

    mov rax, 0x4847464544434241
    mov rbx, 0x504F4E4D4C4B4A49
    pinsrq xmm0, rax, 0
    pinsrq xmm0, rbx, 1

So, xmm0 will be used for original value, xmm1 - for mask, and xmm2
for our tests.

Cases:
1) mov to xmm1 register a 128 bit value with permutation indexes, just
   bytes numbers as they are, i.e "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 02 01 00"
   expected: xmm2 value should be the same as before

    mov rbx, 0x0706050403020100
    mov rcx, 0x0f0e0d0c0b0a0908
    pinsrq xmm1, rbx, 0
    pinsrq xmm1, rcx, 1
    pshufb xmm2, xmm1

2) permutation mask = "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 01 01 01",
   i.e. last three bytes of destination should be the same as byte #01.
   expected: xmm2 = "50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 44 42 42 42"

   movaps xmm2, xmm0
   mov rbx, 0x0706050403010101
   mov rcx, 0x0f0e0d0c0b0a0908
   pinsrq xmm1, rbx, 0
   pinsrq xmm1, rcx, 1
   pshufb xmm2, xmm1

3) permutation mask = "01 01 01 0C 0B 0A 09 08 07 06 05 04 03 02 01 00",
   i.e. first three bytes of destination should be the same as byte #01.
   expected: xmm2 = "42 42 42 4D 4C 4B 4A 49 48 47 46 45 44 43 42 41"

   movaps xmm2, xmm0
   mov rbx, 0x0706050403020100
   mov rcx, 0x0101010c0b0a0908
   pinsrq xmm1, rbx, 0
   pinsrq xmm1, rcx, 1
   pshufb xmm2, xmm1

4) permutation mask = "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 80 80 80",
   i.e. last three bytes of destination shold be eqaul to zero.
   expected: xmm2 = "50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 44 00 00 00"

   movaps xmm2, xmm0
   mov rbx, 0x0706050403808080
   mov rcx, 0x0f0e0d0c0b0a0908
   pinsrq xmm1, rbx, 0
   pinsrq xmm1, rcx, 1
   pshufb xmm2, xmm1

*)

open Core_kernel.Std
open Bap.Std
open OUnit2

module Dis = Disasm_expert.Basic
open X86_env

let of_bytes s =
  let str = String.filter ~f:(fun c -> Char.(c <> ' ')) s in
  Word.of_string @@ sprintf "0x%s:256u" str

let (ident_mask, origin) as no_permutations =
  of_bytes "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 02 01 00",
  of_bytes "50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 44 43 42 41"

let last_three_0x42 =
  of_bytes "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 01 01 01",
  of_bytes "50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 44 42 42 42"

let first_three_0x42 =
  of_bytes "01 01 01 0C 0B 0A 09 08 07 06 05 04 03 02 01 00",
  of_bytes "42 42 42 4D 4C 4B 4A 49 48 47 46 45 44 43 42 41"

let last_three_0x00 =
  of_bytes "0F 0E 0D 0C 0B 0A 09 08 07 06 05 04 03 80 80 80",
  of_bytes "50 4F 4E 4D 4C 4B 4A 49 48 47 46 45 44 00 00 00"

let mem_er = Word.of_string "0x42424242:32u"
let mem_ok = Word.of_string "0x42424220:32u"

let insn_bil x =
  let arch = `x86 in
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

let pshufb_rr = "\x66\x0f\x38\x00\xc1" (** pshufb %xmm0, %xmm1  *)
let pshufb_rm = "\x66\x0f\x38\x00\x00" (** pshufb %xmm0, (%eax) *)

(** tests that permutations works as expected  *)
let test_rr (mask, expected) ctxt =
  let xmm0 = X86_env.ymms.(0) in
  let xmm1 = X86_env.ymms.(1) in
  let bil = Bil.[
      move xmm0 (int origin);
      move xmm1 (int mask);
    ] @ insn_bil pshufb_rr in
  let c = Stmt.eval bil (new Bili.context) in
  assert_bool "pshufb: got wrong result" @@
  match c#lookup xmm0 with
  | None -> false
  | Some r ->
    match Bil.Result.value r with
    | Bil.Imm word -> Word.equal word expected
    | _ -> false

class exn_visitor = object
  inherit [bool] Stmt.visitor
  method! enter_cpuexn _ _ = true
end

(** tests that memory alignment check is performed, i.e. cpuexn could be
    raise in some cases raised *)
let test_rm addr expected ctxt =
  let open X86_env.R32 in
  let xmm0 = X86_env.ymms.(0) in
  let bil = Bil.[
      move rax (int addr);
      move mem
        (store ~mem:(var mem) ~addr:(int addr) (int ident_mask) LittleEndian `r256);
      move xmm0 (int origin);
    ] @ insn_bil pshufb_rm in
  assert_bool "pshufb: memory alignment" @@
  (new exn_visitor)#run bil false

let suite = "pshufb" >::: [
    "no permuatations"          >:: test_rr no_permutations;
    "last three bytes = 0x42"   >:: test_rr last_three_0x42;
    "first three bytes = 0x42"  >:: test_rr first_three_0x42;
    "last three bytes = 0x0"    >:: test_rr last_three_0x00;
    "memory alignment is ok"    >:: test_rm mem_ok false;
    "memory alignment is error" >:: test_rm mem_er true;
  ]
