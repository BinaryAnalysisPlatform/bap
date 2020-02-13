open Bap_core_theory

open Core_kernel
open Bap_future.Std
open OUnit2
open Format
open Word_size
open Bap.Std

type case = {
  name : string;
  arch : arch;
  addr : int;
  code : string;
  bil : string;
  asm : string;
}

let arm = {
  name = "arm-project";
  arch = `armv7;
  addr = 16;
  code = "\x1e\xff\x2f\xe1";
  bil  = "jmp LR";
  asm  = "bx lr";
}

let x86 = {
  name = "x86-project";
  arch = `x86;
  addr = 10;
  code = "\xeb\xfe";
  asm  = "jmp -0x2";
  bil  = "jmp 0xA";
}

let normalize = String.filter ~f:(function
    | '\n' | '\r' | ' ' | '\t' | '{' | '}' -> false
    | _ -> true)

let assert_normalized ~expect got ctxt : unit =
  assert_equal ~ctxt (normalize expect) (normalize got) ~printer:ident

let tag = Value.Tag.register (module String)
    ~name:"test"
    ~uuid:"6b3dab52-f3f5-493d-9db7-ad3eed33add8"

let addr_width case = Arch.addr_size case.arch |> Size.in_bits

let test_substitute case =
  Toplevel.reset ();
  let sub_name = Format.asprintf "test_%a" Arch.pp case.arch in
  let addr = sprintf "%#x" in
  let min_addr = addr case.addr in
  let max_addr = addr (case.addr + String.length case.code - 1) in
  let base = Addr.of_int case.addr ~width:(addr_width case) in
  let rooter = Rooter.create @@ Seq.of_list [base] in
  let symbolizer = Symbolizer.create @@ fun addr ->
    Option.some_if Addr.(base = addr) sub_name in
  let agent =
    let name = sprintf "test-project-symbolizer-for-%s" case.name in
    KB.Agent.register name in
  Symbolizer.provide agent symbolizer;
  Rooter.provide rooter;
  let file = "/dev/null" in
  let input =
    let mem =
      Memory.create LittleEndian base (Bigstring.of_string case.code)
      |> ok_exn in
    let code =
      Memmap.add Memmap.empty mem (Value.create Image.section "bap.test") in
    let data = code in
    Project.Input.create case.arch file ~code ~data in

  let p = Project.create input |> ok_exn in
  let mem,_ = Memmap.lookup (Project.memory p) base |> Seq.hd_exn in
  let test expect s =
    let p = Project.substitute p mem tag s in
    let s = Option.value_exn (Project.memory p |>
                              Memmap.find_map ~f:(Value.get tag)) in
    expect >:: assert_normalized ~expect s in
  let has_filename = "filename-is-provided" >:: fun ctxt ->
      match Project.get p filename with
      | None -> assert_failure "filename is not set"
      | Some file' -> assert_equal ~ctxt ~printer:ident file file' in
  [
    test case.asm "$asm";
    test case.bil "$bil";
    test sub_name "$symbol";
    test sub_name "$symbol_name";
    test min_addr "$symbol_min_addr";
    test max_addr "$symbol_max_addr";
    test min_addr "$symbol_addr";
    test min_addr "$block_addr";
    test max_addr "$block_max_addr";
    test min_addr "$addr";
    test min_addr "$min_addr";
    test max_addr "$max_addr";
    has_filename;
  ]



let suite () = "Project" >::: [
    "ARM" >::: test_substitute arm;
    "386" >::: test_substitute x86;
  ]
