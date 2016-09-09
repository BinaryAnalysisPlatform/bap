open Core_kernel.Std
open Bap_future.Std
open OUnit2
open Format
open Word_size
open Bap.Std

type case = {
  arch : arch;
  addr : int;
  code : string;
  bil : string;
  asm : string;
}

let arm = {
  arch = `armv7;
  addr = 16;
  code = "\x00\x20\xA0\xE3";
  bil  = "R2 := 0x0:32";
  asm  = "mov r2, #0x0";
}

let x86 = {
  arch = `x86;
  addr = 10;
  code = "\x89\x34\x24";
  asm  = "movl %esi, (%esp)";
  bil  = "mem32 := mem32 with [ESP,el]:u32 <- ESI";
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

let test_substitute case : test list =
  let sub_name = Format.asprintf "test_%a" Arch.pp case.arch in
  let addr = sprintf "%#x" in
  let min_addr = addr case.addr in
  let max_addr = addr (case.addr + String.length case.code - 1) in
  let base = Addr.of_int case.addr ~width:(addr_width case) in
  let name addr = Option.some_if Addr.(base = addr) sub_name in
  let symbolizer = Stream.map Project.Info.arch (fun _ ->
      Ok (Symbolizer.create name)) in
  let new_rooter _ = Ok ([base] |> Seq.of_list |> Rooter.create) in
  let rooter = Stream.map Project.Info.arch ~f:new_rooter in
  let input =
    let file = "/dev/null" in
    let mem =
      Memory.create LittleEndian base (Bigstring.of_string case.code)
      |> ok_exn in
    let code =
      Memmap.add Memmap.empty mem (Value.create Image.section "bap.test") in
    let data = code in
    Project.Input.create case.arch file ~code ~data in

  let p = Project.create ~rooter ~symbolizer input |> ok_exn in
  let mem,_ = Memmap.lookup (Project.memory p) base |> Seq.hd_exn in
  let test expect s =
    let p = Project.substitute p mem tag s in
    let s = Option.value_exn (Project.memory p |>
                              Memmap.find_map ~f:(Value.get tag)) in
    expect >:: assert_normalized ~expect s in
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
  ]



let suite () = "Project" >::: [
    "ARM" >::: test_substitute arm;
    "386" >::: test_substitute x86;
  ]
