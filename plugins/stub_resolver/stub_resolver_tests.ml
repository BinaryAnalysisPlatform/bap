(**
   In order to test stub resolver we generate a dummy programs like
   in the example below:

   0000000b: program
   00000008: sub a()
   00000007:


   0000000a: sub b()
   00000009:

   ...

   and provide a knowledge about each symbol:
   if it's a stub and if it has aliases.

*)


open Core_kernel
open Bap_core_theory
open Bap_knowledge
open Bap.Std
open OUnit2

open KB.Syntax

module Cfg = Graphs.Cfg
module Dis = Disasm_expert.Basic

type sym = {
  is_stub : bool;
  aliases : string list;
}

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

let collect_stubs syms =
  Map.fold syms ~init:(Set.empty (module String))
    ~f:(fun ~key:name ~data:{is_stub} stubs ->
        if is_stub
        then Set.add stubs name
        else stubs)

let provide_stubs syms =
  let stubs = collect_stubs syms in
  KB.promise (Value.Tag.slot Sub.stub) @@ fun label ->
  KB.collect Theory.Label.name label >>| function
  | Some name when Set.mem stubs name -> Some ()
  | _ -> None

let provide_aliases syms =
  KB.promise Theory.Label.aliases @@ fun label ->
  KB.collect Theory.Label.name label >>| function
  | None -> Set.empty (module String)
  | Some name ->
    match Map.find syms name with
    | None -> Set.empty (module String)
    | Some {aliases} -> Set.of_list (module String) aliases

let cfg_of_block b = Cfg.Node.insert b Cfg.empty

let create_program syms =
  let nop = "\x66\x90" in
  let step = Addr.of_int64 2L in
  let rec loop symtab addr = function
    | [] -> symtab
    | name :: names ->
      let symtab = add_symbol symtab name addr nop in
      loop symtab Addr.(addr + step) names in
  let symtab = loop Symtab.empty (Addr.zero 64) (Map.keys syms) in
  let prog = Program.lift symtab in
  provide_stubs syms;
  provide_aliases syms;
  prog

let tid_for_name_exn prog name =
  Term.to_sequence sub_t prog |>
  Seq.find_map
    ~f:(fun s ->
        if Sub.name s = name
        then Some (Term.tid s)
        else None) |>
  Option.value_exn

let run name symbols expected _ctxt =
  let syms =
    List.fold symbols ~init:(Map.empty (module String))
      ~f:(fun syms (name,data) ->
          Map.add_exn syms name data) in
  let prog = create_program syms in
  let expected =
    List.fold expected
      ~init:(Map.empty (module Tid))
      ~f:(fun tids (stub, impl) ->
          Map.add_exn tids
            (tid_for_name_exn prog stub)
            (tid_for_name_exn prog impl)) in
  let pairs = Stub_resolver.run prog in
  assert_bool name (Map.equal Tid.equal expected pairs)

let symbol ?(is_stub=false) ?(aliases=[]) name =
  name, {is_stub; aliases}

let test name ~symbols ~expected =
  name >:: run name symbols expected

let suite = "stub-resolver" >::: [

    test "simple case: we have pairs"
      ~symbols:[
        symbol "alpha";
        symbol "alpha_2" ~is_stub:true ~aliases:["alpha"];
      ]
      ~expected:["alpha_2", "alpha"];

    test "simple case: no pairs"
      ~symbols:[
        symbol "bravo";
        symbol "bravo_2" ~is_stub:true;
      ]
      ~expected:[];

    test "simple case: still no pairs"
      ~symbols:[
        symbol "charlie";
        symbol "charlie_2" ~is_stub:true ~aliases:["some-sym"];
      ]
      ~expected:[];

    test "stubs only"
      ~symbols:[
        symbol "delta" ~is_stub:true;
        symbol "delta_2" ~is_stub:true ~aliases:["delta"];
      ]
      ~expected:[];

    test "impl only"
      ~symbols:[
        symbol "echo";
        symbol "echo_2" ~aliases:["echo"];
      ]
      ~expected:[];

    test "impl can be aliases as well"
      ~symbols:[
        symbol "foxtrot" ~aliases:["golf"];
        symbol "golf" ~is_stub:true;
      ]
      ~expected:["golf", "foxtrot"];

    test "many alias"
      ~symbols:[
        symbol "hotel" ~aliases:["india"; "juliet"];
        symbol "india" ~is_stub:true;
      ]
      ~expected:["india", "hotel"];

    test "ambiguous impl"
      ~symbols:[
        symbol "kilo" ~aliases:["kilo_2"; "kilo_3"];
        symbol "kilo_2" ~is_stub:true;
        symbol "kilo_3" ~is_stub:true;
      ]
      ~expected:[];

    test "ambiguous stubs"
      ~symbols:[
        symbol "lima";
        symbol "lima_2" ~is_stub:true ~aliases:["lima"];
        symbol "lima_3" ~is_stub:true ~aliases:["lima"];
      ]
      ~expected:[];

    test "crossreference"
      ~symbols:[
        symbol "mike" ~aliases:["november"];
        symbol "november" ~is_stub:true ~aliases:["mike"];
      ]
      ~expected:["november", "mike"];

    test "many pairs"
      ~symbols:[
        symbol "papa" ;
        symbol "quebec";
        symbol "romeo";
        symbol "siera" ~is_stub:true ~aliases:["papa"];
        symbol "tango" ~is_stub:true ~aliases:["quebec"];
        symbol "uniform" ~is_stub:true ~aliases:["romeo"];

      ]
      ~expected:["siera",   "papa";
                 "tango",   "quebec";
                 "uniform", "romeo";];
  ]
