open Core_kernel.Std
open OUnit2
open Format
open Word_size
open Bap.Std
open Project

let arm_mov_r2_0 = "\x00\x20\xA0\xE3"
let x86_mov_mem_esp_esi = "\x89\x34\x24"

let str_printer expect_err = Sexp.to_string_hum (sexp_of_string expect_err)

let create_addr = function
  | W32 -> Addr.of_int ~width:32
  | W64 -> Addr.of_int ~width:64

let create_mem ?(addr_size=W32) ?(start_addr=0x00)
    ?(size=0x10) ?(hexstring="\x61\x61\x61\x61") () =
  let addr = create_addr W32 start_addr in
  let hexstring = Bigstring.of_string hexstring in
  let m = Memory.create LittleEndian addr hexstring in
  match m with
  | Ok m -> m
  | Error err -> assert_failure (Error.to_string_hum err)

let create_project architecture =
  let mem = create_mem () in
  let dis =
    disassemble architecture mem in
  let symtab = Table.add Table.empty mem ".text" |> ok_exn in
  {arch = architecture; disasm = dis; memory = Memmap.empty;
   storage = String.Map.empty; symbols = symtab;
   base = mem }

let substitute project ~kind ~hexstring =
  let memory =
    let tag =(Tag.create text kind) in
    Memmap.add project.memory (create_mem ~hexstring ()) tag in
  let project_result = Project.substitute {project with memory} in
  Memmap.fold ~init:[] project_result.memory ~f:(fun acc tag ->
      match Tag.value text tag with
      | None -> acc
      | Some text -> text :: acc)

let strip = String.filter ~f:(function
    | '\n' | '\r' | ' ' | '\t' -> false
    | _ -> true)

let test_substitution ctxt ~arch ~kind ~expect ~hexstring =
  let project = create_project arch in
  let result = substitute project ~kind ~hexstring in
  let msg = sprintf "Incorrect %s substitution" kind in
  assert_equal ~ctxt ~printer:str_printer ~msg expect
    (match List.hd result with
     | Some x -> strip x
     | None -> assert_failure "Empty result")

let expect_x86_mov_mem_esp_esi =
  "{mem32:=mem32with[low:32[ESP],el]:u32<-low:32[ESI]}"

let suite = "Project" >::: [
    "sub_asm" >::
    test_substitution ~arch:`armv7 ~kind:"$asm"
      ~expect:"movr2,#0x0" ~hexstring:arm_mov_r2_0;

    "sub_bil_arm" >::
    test_substitution ~arch:`armv7 ~kind:"$bil"
      ~expect:"{R2:=0x0:32}" ~hexstring:arm_mov_r2_0;

    "sub_bil_x86" >::
    test_substitution ~arch:`x86 ~kind:"$bil"
      ~expect:expect_x86_mov_mem_esp_esi ~hexstring:x86_mov_mem_esp_esi;

    "sub_addr" >::
    test_substitution ~arch:`armv7 ~kind:"$addr"
      ~expect:"0x0" ~hexstring:arm_mov_r2_0;

    "sub_max_addr_arm" >::
    test_substitution ~arch:`armv7 ~kind:"$max_addr"
      ~expect:"0x3" ~hexstring:arm_mov_r2_0;

    "sub_max_addr_x86" >::
    test_substitution ~arch:`x86 ~kind:"$max_addr"
      ~expect:"0x2" ~hexstring:x86_mov_mem_esp_esi;

    "sub_symbol" >::
    test_substitution ~arch:`armv7 ~kind:"$symbol"
      ~expect:".text" ~hexstring:arm_mov_r2_0;

    "sub_symbol_addr" >::
    test_substitution ~arch:`armv7 ~kind:"$symbol_addr"
      ~expect:"0x0" ~hexstring:arm_mov_r2_0;
  ]
