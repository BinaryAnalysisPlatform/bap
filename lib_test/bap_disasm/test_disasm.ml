open Core_kernel
open Bap.Std
open Result
open OUnit2

module Dis = Disasm_expert.Basic
module Rec = Disasm_expert.Recursive
module Cfg = Graphs.Cfg

let err fmt = Or_error.errorf fmt

let leaq = "\x48\x8d\x00"
let lea = "\x67\x8d\x00"
let sub = "\x48\x83\xec\x08"
let call = "\xe8\x47\xee\xff\xff"
let mov = "\x8b\x40\x10"
let add = "\x48\x83\xc4\x08"
let ret = "\xc3"
let overlap = "\x2d\xdd\xc3\x54\x55"
let valid_insn_seq = "\x55\x54\xE9\xFC\xFF\xFF\xFF"
let x86_64 = "x86_64", [
    (* sub $0x8,%rsp *)
    sub , ["SUB64ri8"; "RSP"; "RSP"; "0x8"], [];
    (* callq 942040 *)
    call , ["CALL64pcrel32"; "-0x11b9";],
    [`Call; `May_affect_control_flow];
    (* mov 0x10(%rax),%eax *)
    mov, ["MOV32rm"; "EAX"; "RAX"; "0x1"; "Nil"; "0x10"; "Nil"],
    [`May_load];
    (* add $0x8, %rsp *)
    add, ["ADD64ri8"; "RSP"; "RSP"; "0x8"], [];
    (* "retq" *)
    ret, ["RET"],
    [`Return; `Barrier; `Terminator; `May_affect_control_flow]
  ]

let call1 = "\xe8\x01\x00\x00\x00"

let mem_equal x y =
  Memory.(Addr.(min_addr x = min_addr y && max_addr x = max_addr y))

let assert_memory =
  assert_equal ~printer:Memory.to_string ~cmp:mem_equal

let memory_of_string ?(start=0) ?(width=64) data =
  Memory.create LittleEndian
    (Addr.of_int start ~width)
    (Bigstring.of_string data) |> Or_error.ok_exn

let string_of_strings insn =
  Sexp.to_string (sexp_of_list sexp_of_string insn)

let printer r =
  Or_error.sexp_of_t (sexp_of_list sexp_of_string) r |>
  Sexp.to_string

let strings_of_insn insn =
  let name = Dis.Insn.name insn in
  let ops = Dis.Insn.ops insn |> Array.to_list |>
            List.map ~f:(Op.to_string) in
  (name :: ops)

let insn_of_mem arch data _ctxt =
  Bap_toplevel.reset ();
  let mem = memory_of_string data in
  Dis.with_disasm ~backend:"llvm" arch ~f:(fun dis ->
      Dis.insn_of_mem dis mem >>= function
      | mem,None,rest -> err "Failed to disassemble instruction"
      | _,_,`left _ -> err "some memory was left"
      | mem,(Some insn),`finished ->
        assert_equal ~printer:(string_of_int)
          (String.length data) (Memory.length mem);
        return (strings_of_insn insn))

let assert_equal_regexp s s' =
  let pat = sprintf "%s.?" s in
  let msg =
    sprintf "failed to find %s by pattern %s in %s" s' pat s in
  assert_bool msg Str.(string_match (regexp pat) s' 0)

let assert_strings_equal ctxt ss ss' =
  match ss, ss' with
  | name :: ss, name' :: ss' ->
    assert_equal_regexp name name';
    assert_equal ~ctxt ss ss'
  | _ -> assert_string "got different strings"

let test_insn_of_mem  (arch,samples) ctxt =
  let test (data,expect,_) =
    match insn_of_mem arch data ctxt with
    | Ok strs ->
      assert_strings_equal ctxt expect strs
    | Error err -> assert_string (Error.to_string_hum err) in
  List.iter samples ~f:test

let test_run_all (arch,samples) ctxt =
  Bap_toplevel.reset ();
  let mem =
    samples |> List.map ~f:fst3 |> String.concat |> memory_of_string in
  Dis.with_disasm ~backend:"llvm" arch ~f:(fun dis ->
      let dis = Dis.store_kinds dis in
      Dis.run
        ~return:Or_error.return ~init:()
        ~invalid:(fun _ _ () -> err "got invalid instruction")
        ~hit:(fun _ _ _ () -> err "hit should be called")
        ~stopped:(fun s () ->
            Or_error.return @@
            List.iter2_exn  samples  (Dis.insns s)
              ~f:(fun (data,exp,kinds) -> function
                  | (_,None) -> assert_string "bad instruction"
                  | (mem, Some r) ->
                    assert_strings_equal ctxt exp (strings_of_insn r);
                    assert_equal ~ctxt ~printer:Int.to_string
                      (String.length data) (Memory.length mem);
                    List.iter kinds ~f:(fun expected ->
                        let name =
                          string_of_sexp @@  sexp_of_kind expected in
                        assert_bool name (Dis.Insn.is r expected));
                )) dis mem)

let test_run_all data ctxt =
  let printer x = Sexp.to_string (Or_error.sexp_of_t sexp_of_unit x) in
  assert_equal ~ctxt ~printer
    (Ok ()) (test_run_all data ctxt)

(*
     The test case is the following function.

     int f(const char *p) {
         int c = 0;
         for (; p != '\0'; ++p, ++c);
         return c;
     }

This is the actual assembler output, that we've got with the following
command: [arm-linux-gnueabi-gcc -marm]


f:
;; Block 1
0C:    str	fp, [sp, #-4]!    ; 04 B0 2D E5
10:    add	fp, sp, #0        ; 00 B0 8D E2
14:    sub	sp, sp, #20       ; 14 D0 4D E2
18:    str	r0, [fp, #-16]    ; 10 00 0B E5
1C:    mov	r3, #0            ; 00 30 A0 E3
20:    str	r3, [fp, #-8]     ; 08 30 0B E5
24:    b	.L4               ; 05 00 00 EA

;; Block 2
.L5:
28:    ldr	r3, [fp, #-16]    ; 10 30 1B E5
2C:    add	r3, r3, #1        ; 01 30 83 E2
30:    str	r3, [fp, #-16]
34:    ldr	r3, [fp, #-8]
38:    add	r3, r3, #1
3C:    str	r3, [fp, #-8]

;; Block 3
.L4:
40:    ldr	r3, [fp, #-16]
44:    cmp	r3, #0
48:    bne	.L5

;; Block 4
4C:    ldr	r3, [fp, #-8]
50:    mov	r0, r3
54:    sub	sp, fp, #0
58:    ldr	fp, [sp], #4
5C:    bx	lr                ; 1E FF 2F E1


0000840C  04 B0 2D E5 00 B0 8D E2 14 D0 4D E2 10 00 0B E5 |..-.......M.....|
0000841C  00 30 A0 E3 08 30 0B E5 05 00 00 EA 10 30 1B E5 |.0...0.......0..|
0000842C  01 30 83 E2 10 30 0B E5 08 30 1B E5 01 30 83 E2 |.0...0...0...0..|
0000843C  08 30 0B E5 10 30 1B E5 00 00 53 E3 F6 FF FF 1A |.0...0....S.....|
0000844C  08 30 1B E5 03 00 A0 E1 00 D0 4B E2 04 B0 9D E4 |.0........K.....|
0000845C  1E FF 2F E1                                     |../.            |

 *)

let block1 = [
  "\x04\xB0\x2D\xE5";
  "\x00\xB0\x8D\xE2";
  "\x14\xD0\x4D\xE2";
  "\x10\x00\x0B\xE5";
  "\x00\x30\xA0\xE3";
  "\x08\x30\x0B\xE5";
  "\x05\x00\x00\xEA";
]
let block2 = [
  "\x10\x30\x1B\xE5";
  "\x01\x30\x83\xE2";
  "\x10\x30\x0B\xE5";
  "\x08\x30\x1B\xE5";
  "\x01\x30\x83\xE2";
  "\x08\x30\x0B\xE5";
]
let block3 = [
  "\x10\x30\x1B\xE5";
  "\x00\x00\x53\xE3";
  "\xF6\xFF\xFF\x1A";
]
let block4 = [
  "\x08\x30\x1B\xE5";
  "\x03\x00\xA0\xE1";
  "\x00\xD0\x4B\xE2";
  "\x04\xB0\x9D\xE4";
  "\x1E\xFF\x2F\xE1";
]

let strlen = List.concat [
    block1;
    block2;
    block3;
    block4
  ]

(* CFG written as a list of entries, where each entry is
   (blk, preds, succs), where
   blk - block number, starting from 1
   preds - list of block numbers of predcessors
   succs is a list of destinations, where each destination is
   (blk_num, kind).
*)

type dest_kind = [`Jump | `Cond | `Fall ] [@@deriving sexp]

type graph = (int * int list * (int * dest_kind) list) list
[@@deriving sexp_of]

let graph : graph = [
  1, [],    [3, `Jump];
  2, [3],   [3, `Fall];
  3, [1;2], [2, `Cond; 4, `Fall];
  4, [3],   []
]

let create_block addr blk =
  let data = String.concat blk in
  Memory.create LittleEndian
    (Addr.of_int32 addr)
    (Bigstring.of_string data) |> Or_error.ok_exn

let blocks = [|
  create_block 0x840Cl block1;
  create_block 0x8428l block2;
  create_block 0x8440l block3;
  create_block 0x844Cl block4;
|]

let run_rec () =
  let mem = create_block 0x840Cl strlen in
  Rec.run `armv7 mem

let test_cfg test ctxt =
  match run_rec () with
  | Ok r -> test r ctxt
  | Error err -> assert_string (Error.to_string_hum err)

let amount dis ctxt =
  assert_equal ~ctxt ~printer:Int.to_string
    ~msg:"Expected 4 basic blocks"
    4 (Cfg.number_of_nodes (Rec.cfg dis))

let equal_addrs m1 m2 = Memory.(min_addr m1 = min_addr m2)
let string_of_mem mem = Addr.string_of_value (Memory.min_addr mem)
let unexpected_block mem =
  failwithf "Unexpected block starting from %s" (string_of_mem mem) ()

let addresses dis ctxt =
  Rec.cfg dis |> Cfg.nodes |> Seq.iter ~f:(fun blk ->
      let mem = Block.memory blk in
      match Array.find blocks ~f:(equal_addrs mem) with
      | None -> unexpected_block mem
      | Some mem' ->
        let msg = sprintf "Block at %s has incorrect size"
            (string_of_mem mem) in
        assert_equal ~ctxt ~printer:Int.to_string
          ~msg (Memory.length mem') (Memory.length mem))

let build_graph dis : graph =
  let blk_num blk =
    let mem = Block.memory blk in
    match Array.findi blocks ~f:(Fn.const (equal_addrs mem)) with
    | Some (i,_) -> i + 1
    | None -> unexpected_block mem in
  let cfg = Rec.cfg dis in
  Cfg.nodes cfg |> Seq.fold ~init:[] ~f:(fun graph blk ->
      let preds = Seq.map (Cfg.Node.preds blk cfg) ~f:(fun blk -> blk_num blk) in
      let dests = Seq.map (Cfg.Node.outputs blk cfg) ~f:(fun e ->
          blk_num (Cfg.Edge.dst e), Cfg.Edge.label e) in
      (blk_num blk, Seq.to_list preds, Seq.to_list dests) :: graph)

let structure cfg ctxt =
  let sort x = List.sort ~compare:Polymorphic_compare.compare x in
  let deepsort graph =
    List.map graph ~f:(fun (id,preds,succs) ->
        id, sort preds, sort succs) |> sort in
  let printer g = Sexp.to_string (sexp_of_graph g) in
  let got = deepsort (build_graph cfg) in
  let expect = deepsort graph in
  assert_equal ~ctxt ~printer ~msg:"Wrong graph structure" expect got

(* test one instruction cfg *)
let test_micro_cfg insn ctxt =
  Bap_toplevel.reset ();
  let open Or_error in
  let mem = Bigstring.of_string insn |>
            Memory.create LittleEndian (Addr.of_int64 0L) |>
            ok_exn in
  let dis = Rec.run `x86_64 mem |> ok_exn in
  assert_bool "No errors" (Rec.errors dis = []);
  assert_bool "One block" (Rec.cfg dis |> Cfg.number_of_nodes = 1);
  Rec.cfg dis |> Cfg.nodes |>
  Seq.to_list |> List.hd_exn
  |> Block.insns |> function
  | [mem, _] ->
    let max_addr = Addr.of_int ~width:64 (String.length insn - 1) in
    assert_equal ~printer:Addr.to_string ~ctxt
      (Addr.of_int64 0L) (Memory.min_addr mem);
    assert_equal ~printer:Addr.to_string ~ctxt
      max_addr (Memory.max_addr mem)
  | [] -> assert_string "No instructions"
  | _ :: _ -> assert_string "More than one instruction"

(* call 1
   ret
   ret
   ret

   should emit structure

   +-------------------+
   |0:     call 1      +------+
   +---------+---------+      |
             |                |
   +---------+---------+      |
   |5:      ret        |      |
   +-------------------+      |
                              |
   +-------------------+      |
   |6:      ret        +<-----+
   +-------------------+

   +-------------------+
   |7:      ret        +
   +-------------------+
*)

let has_dest cfg src dst kind =
  let e = Cfg.Edge.create src dst kind in
  Cfg.Edge.mem e cfg


let sort_by_addr =
  List.sort ~compare:(fun x y ->
      Addr.compare (Block.addr x) (Block.addr y))


let call1_3ret _ctxt =
  Bap_toplevel.reset ();
  let mem = String.concat [call1; ret; ret; ret] |>
            memory_of_string in
  let dis = Rec.run `x86_64 mem |> Or_error.ok_exn in
  assert_bool "No errors" (Rec.errors dis = []);
  assert_bool "Three block" (Rec.cfg dis |> Cfg.number_of_nodes = 4);
  let cfg = Rec.cfg dis in
  match Cfg.nodes cfg |> Seq.to_list |> sort_by_addr with
  | [b1;b2;b3;b4] ->
    let call = memory_of_string ~width:64 call1 in
    let ret1 = memory_of_string ret ~start:5 ~width:64 in
    let ret2 = memory_of_string ret ~start:6 ~width:64 in
    let ret3 = memory_of_string ret ~start:7 ~width:64 in
    assert_memory call (Block.memory b1);
    assert_memory ret1 (Block.memory b2);
    assert_memory ret2 (Block.memory b3);
    assert_memory ret3 (Block.memory b4);
    assert_bool "b1 -> jump b3" @@ has_dest cfg b1 b3 `Jump;
    assert_bool "b1 -> fall b2" @@ has_dest cfg b1 b2 `Fall;
    assert_bool "b2 has no succs" @@
    Seq.is_empty (Cfg.Node.succs b2 cfg);
    assert_bool "b3 has no succs" @@
    Seq.is_empty (Cfg.Node.succs b3 cfg);
  | _ -> assert false

let suite () = "Disasm.Basic" >::: [
    "x86_64/one"            >:: test_insn_of_mem x86_64;
    "x86_64/all"            >:: test_run_all     x86_64;
    "recurse"               >:: test_cfg amount;
    "addresses"             >:: test_cfg addresses;
    "structure"             >:: test_cfg structure;
    "ret"                   >:: test_micro_cfg ret;
    "sub"                   >:: test_micro_cfg sub;
    "call1_3ret"            >:: call1_3ret;
  ]
