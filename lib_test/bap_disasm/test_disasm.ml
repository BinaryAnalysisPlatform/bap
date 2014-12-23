open Core_kernel.Std
open Bap.Std
open Result
open OUnit2

module Dis = Disasm.Basic

let err fmt = Or_error.errorf fmt

let x86_64 = "x86_64", [
    "\x48\x83\xec\x08", ["SUB64ri8"; "RSP"; "RSP"; "0x8"],
    "sub $0x8,%rsp";

    "\xe8\x47\xee\xff\xff", ["CALL64pcrel32"; "-0x11b9";],
    "callq 942040";

    "\x8b\x40\x10", ["MOV32rm"; "EAX"; "RAX"; "0x1"; "nil"; "0x10"; "nil"],
    "mov 0x10(%rax),%eax";

    "\x48\x83\xc4\x08", ["ADD64ri8"; "RSP"; "RSP"; "0x8"],
    "add $0x8, %rsp";

    "\xc3", ["RET"],
    "retq"
  ]

let memory_of_string data =
  Memory.create LittleEndian
    (Addr.zero 32)
    (Bigstring.of_string data) |> Or_error.ok_exn

let string_of_strings insn =
  Sexp.to_string (sexp_of_list sexp_of_string insn)

let printer r =
  Or_error.sexp_of_t (sexp_of_list sexp_of_string) r |>
  Sexp.to_string

let strings_of_insn insn =
  let name = Dis.Insn.name insn in
  let ops = Dis.Insn.ops insn |> Array.to_list |>
            List.map ~f:(Dis.Op.to_string) in
  (name :: ops)


let insn_of_mem arch data ctxt =
  let mem = memory_of_string data in
  Dis.create ~backend:"llvm" arch >>= fun dis ->
  Dis.insn_of_mem dis mem >>= function
  | mem,None,rest -> err "Failed to disassemble instruction"
  | _,_,`left _ -> err "some memory was left"
  | mem,(Some insn),`finished ->
    assert_equal ~printer:(string_of_int)
      (String.length data) (Memory.size mem);
    return (strings_of_insn insn)

let test_insn_of_mem  (arch,samples) ctxt =
  let test (data,expect,_) =
    assert_equal ~ctxt ~printer
      (Ok expect) (insn_of_mem arch data ctxt)  in
  List.iter samples ~f:test

let test_run_all (arch,samples) ctxt =
  let mem =
    samples |> List.map ~f:fst3 |> String.concat |> memory_of_string in
  Dis.create ~backend:"llvm" arch >>= fun dis ->
  Dis.run
    ~return:Or_error.return ~init:()
    ~invalid:(fun _ _ () -> err "got invalid instruction")
    ~hit:(fun _ _ _ () -> err "hit should be called")
    ~stopped:(fun s () ->
        Or_error.return @@
        List.iter2_exn  samples  (Dis.insns s)
          ~f:(fun (data,exp,asm) -> function
              | (_,None) -> assert_string "bad instruction"
              | (mem, Some r) ->
                assert_equal ~ctxt ~printer
                  (Ok exp) (Ok (strings_of_insn r));
                assert_equal ~ctxt ~printer:Int.to_string
                  (String.length data) (Memory.size mem))) dis mem

let test_run_all data ctxt =
  let printer x = Sexp.to_string (Or_error.sexp_of_t sexp_of_unit x) in
  assert_equal ~ctxt ~printer
    (Ok ()) (test_run_all data ctxt)

let () = Plugins.load ()

let suite = "Disasm.Basic" >::: [
    "x86_64/one" >:: test_insn_of_mem x86_64;
    "x86_64/all" >:: test_run_all     x86_64;
  ]
