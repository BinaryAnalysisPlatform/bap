open Core_kernel
open Bap_core_theory
open Bap_knowledge
open Bap.Std
open OUnit2

open KB.Syntax

module Cfg = Graphs.Cfg
module Dis = Disasm_expert.Basic

let find_pairs = Stub_resolver.find_pairs

let run dis mem =
  Or_error.ok_exn @@
  Dis.run dis mem
    ~init:[]
    ~return:Result.return
    ~stop_on:[`Valid]
    ~invalid:(fun state _ pos -> Dis.step state pos)
    ~hit:(fun state mem insn insns ->
        Dis.step state ((mem, Insn.of_basic insn) :: insns))

let block_of_bytes addr b =
  let code = Bigstring.of_string b in
  let mem =
    Or_error.ok_exn @@
    Memory.create LittleEndian addr code in
  let dis = Or_error.ok_exn @@ Dis.create ~backend:"llvm"
      (Arch.to_string `x86_64) in
  let insns = run dis mem in
  Block.create mem insns

let add_symbol symtab name addr bytes =
  let block = block_of_bytes addr bytes in
  Symtab.add_symbol symtab
    (name, block, Cfg.Node.insert block Cfg.empty)

let provide_stubs stubs =
  KB.promise (Value.Tag.slot Sub.stub) @@ fun label ->
  KB.collect Theory.Label.name label >>| function
  | Some name when Set.mem stubs name ->
    printf "has stub %s\n" name;
    Some ()
  | _ -> None

let provide_aliases funs =
  KB.promise (Value.Tag.slot Sub.aliases) @@ fun label ->
  KB.collect Theory.Label.name label >>| function
  | None ->
    printf "no als \n";   None
  | Some name ->
    match Map.find funs name with
    | None -> printf "no aliases for %s\n" name; None
    | Some als ->
      printf "there are %d alisases for %s\n" (List.length als) name;
      Some als

let create_program names =
  let nop = "\x66\x90" in
  let step = Addr.of_int64 2L in
  let rec loop symtab addr = function
    | [] -> symtab
    | name :: names ->
      let symtab = add_symbol symtab name addr nop in
      loop symtab Addr.(addr + step) names in
  let symtab = loop Symtab.empty (Addr.zero 64) names in
  Program.lift symtab

let tid_of_name_exn prog name =
  Term.to_sequence sub_t prog |>
  Seq.find_map
    ~f:(fun s ->
        if Sub.name s = name
        then Some (Term.tid s)
        else None) |>
  Option.value_exn

(* todo: remove *)
let str_of_map m =
  let ts = Tid.to_string in
  Map.fold ~init:"" m ~f:(fun ~key ~data s ->
      sprintf "%s%s-->%s; "
        s (ts key) (ts data))

let collect_stubs s =
  Map.fold s ~init:(Set.empty (module String))
    ~f:(fun ~key ~data:als s ->
        match als with
        | [] -> s
        | _ -> Set.add s key)

let test1 _ctxt =
  let impl  = "a" in
  let stub = "a_stub" in
  let funs = String.Map.of_alist_exn [
      impl, [];
      stub, [impl];
      "b_stub", ["b"];
    ] in
  let prog = create_program (Map.keys funs) in
  provide_stubs   (collect_stubs funs);
  provide_aliases funs;
  let pairs = find_pairs prog in
  let expected =
    let tid1 = tid_of_name_exn prog stub in
    let tid2 = tid_of_name_exn prog impl in
    Map.add_exn (Map.empty (module Tid)) tid1 tid2 in
  printf "expected: %s\n" (str_of_map expected);
  printf "pairs   : %s\n" (str_of_map pairs);
  assert_bool "test1" (Map.equal Tid.equal expected pairs)




let suite = "stub-resolver" >::: [
    "test 1"    >:: test1;
  ]
