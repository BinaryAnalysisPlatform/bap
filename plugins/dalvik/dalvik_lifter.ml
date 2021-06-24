open Bap_core_theory
open KB.Syntax
open Dalvik_disasm
open Dalvik_java

(* Instruction reference is based on https://source.android.com/devices/tech/dalvik/dalvik-bytecode
 *)

let package = "dalvik"

(* modular arithmetics for 4 bit values *)
module M4 = Bitvec.Make(struct let modulus = Bitvec.modulus 4 end)
module M8 = Bitvec.Make(struct let modulus = Bitvec.modulus 8 end)
module M16 = Bitvec.Make(struct let modulus = Bitvec.modulus 16 end)
module M32 = Bitvec.M32
module M64 = Bitvec.M64

module Dalvik(Core : Theory.Core) = struct
  open Core
  open Java

  module Types
    (* This one should load the type length by the index *)
    let get_length _i = 64
  end

  let pass = perform Theory.Effect.Sort.bot
  let skip = perform Theory.Effect.Sort.bot

  let frame = var current_frame

  let set_reg x v =
    set current_frame (store frame x v)

  let get_reg x = load frame x

  (* FIXME: its wrong, just a stub for now *)
  (* Returns the result of the preceding invoke-* instructions *)
  let get_result s =
      match s with
      | 8 -> load frame 0
      | 16 -> load frame 0
      | _ -> failwith "WRONG result width"

  let allocate_object dst len =
      seq (set_reg dst (var brk)) (set brk (add (var brk) (int value len)))

  let new_array dst len _typ : unit Theory.Effect.t KB.t =
      unlabeled >>= fun lbl ->
      blk lbl (allocated_object (int reg_name dst) len) skip

  let get_memory addr pos =
      let stride = M64.int (Theory.Bitv.size value / 8) in
      let off = add addr (mul pos (int value stride)) in
      get heap (load (var heap) off)

  let set_memory addr pos data =
      let stride = M64.int (Theory.Bitv.size value / 8) in
      let off = add addr (mul pos (int value stride)) in
      set heap (store (var heap) off data)

  let set_slot dst pos data =
      set_memory (get_reg dst) pos data

  let get_slot src pos =
      get_memory (get_reg src) pos data

  let data_block = function
      | [] -> pass
      | xs -> List.reduce_exn xs ~f:seq

  let block xs =
      unlabeled >>= fun lbl ->
      blk lbl (data_block xs) skip

  let array_get dst src pos =
      let value = get_slot src pos in
      (* FIXME: set the destination right! *)
      dst := value

  let mov x y = set_reg x (get_reg y)
  let const x y = set_reg x y
  let cmp d x y =
      (* Boils to Theory.Bitv *)
      let open Core.Basic in
      let x = int reg_name (M4.int x)
      and y = int reg_name (M4.int x)
      and d = int reg_name (M4.int z) in
      let res = sub x y in
      if is_zero res then set_reg d 0 else
          if sgt x y then set_reg d 1 else set_reg d -1

  let mov_rr x y =
    let x = int reg_name x
    and y = int reg_name y in
    mov x y

  let mov_rr4 x y =
    mov_rr (M4.int x) (M4.int y)

  let mov_rr816 x y =
    mov_rr (M8.int x) (M16.int y)

  let mov_rr16 x y =
    mov_rr (M16.int x) (M16.int y)

  let movw_rr x y =
    let x = int regpair_name x
    and y = int regpair_name y in
    mov x y

  let movw_rr4 x y =
    movw_rr (M4.int x) (M4.int y)

  let movw_rr816 x y =
    movw_rr (M8.int x) (M16.int y)

  let movw_rr16 x y =
    movw_rr (M16.int x) (M16.int y)

  let mov_r x =
    let x = int reg_name (M4.int x) in
    mov x (get_result 8)

  let movw_r x =
    let x = int regpair_name (M8.int x) in
    mov x (get_result 16)

  let mov_rc x y =
    let x = int reg_name (M4.int x) in
    const x y

  let mov_rc16 x c =
    let x = int reg_name (M8.int x) in
    const x y

  let movw_rc x c =
    let x = int regpair_name x in
    const x y

  let movw_rc8 x c =
    movw_rc (M8.int x) c

  let movw_rc32 x c =
    movw_rc (M32.int x) c

  let move eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    blk lbl eff skip

  let apply f x y =
    let x = int reg_name (M4.int x)
    and y = int reg_name (M4.int y) in
    mov x (f y)

  let apply2 f dest x y =
    let x = int reg_name (M8.int x)
    and y = int reg_name (M8.int y)
    and dest = int reg_name (M8.int dest) in
    mov dest (f x y)

  let apply2c f dest x c =
    let x = int reg_name (M8.int x)
    and c = Bitv.define c
    and dest = int reg_name (M8.int dest) in
    mov dest (f x c)

  let fapply f x y =
    let open Fbasic in
    let x = float reg_name (M4.int x)
    and y = float reg_name (M4.int y) in
    mov x (f y)

  let fapply2 f dest x y =
    let open Fbasic in
    let x = float reg_name (M4.int x)
    and y = float reg_name (M4.int y)
    and dest = float reg_name (M8.int dest) in
    mov dest (f x y)

  (* TODO: Merge these two functions together *)
  let filled_new_array dst len data =
      Theory.Var.fresh value >>= fun i ->
      let dst = int reg_name dst in
      let data = int value data in
      block [
          set i (int value Bitvec.zero);
          allocate_object dst len;
          repeat (ult (var i) (int value len)) @@
          data_block [
              set_slot dst (var i) data;
              set i (add (var i) (int value Bitvec.one))
          ]
      ]

  let fill_memory_data addr data len =
      Theory.Var.fresh value >>= fun i ->
      let data = int value data in
      block [
        set i (int value Bitvec.zero);
        repeat (ult (var i) (int value len)) @@
        data_block [
            set_memory addr (var i) data;
            set i (add (var i) (int value Bitvec.one))
        ]
      ]

  let fill_array_data dst data len =
      let addr = int reg_name dst in
      fill_memory_data addr data len

  let array_put dst src pos =
      (* FIXME: Load the data first? *)
      let dst = int reg_name dst in
      set_slot dst pos src

  let array_put_static src idx len =
      (* How to find the destination address? *)
      let addr = 0 in
      let src = int reg_name src in
      fill_memory_data addr src len

  let nop =
    KB.return @@
    Theory.Effect.empty Theory.Effect.Sort.top

  let jmp =
    KB.return @@
    Theory.Effect.jmp

  let run
    : insn -> unit Theory.Effect.t KB.t =
    function
    | (OP_NOP,[]) -> nop
    | (OP_MOVE, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (mov_rr4 x y)
    | (OP_MOVE_FROM16, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (mov_rr816 x y)
    | (OP_MOVE_16, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (mov_rr16 x y)
    | (OP_MOVE_WIDE, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (movw_rr4 x y)
    | (OP_MOVE_WIDE_FROM16, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (movw_rr816 x y)
    | (OP_MOVE_WIDE16, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (movw_rr16 x y)
    (* Object registers essentially do the same, but with objects *)
    | (OP_MOVE_OBJECT, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (mov_rr4 x y)
    | (OP_MOVE_OBJECT_FROM16, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (mov_rr816 x y)
    | (OP_MOVE_OBJECT_16, [OPR_REGISTER x; OPR_REGISTER y]) ->
      move (mov_rr16 x y)
    (* Result accessing just takes the value from the frame *)
    | (OP_MOVE_RESULT, [OPR_REGISTER x]) ->
      move (mov_r x)
    | (OP_MOVE_RESULT_WIDE, [OPR_REGISTER x]) ->
      move (movw_r x)
    | (OP_MOVE_RESULT_OBJECT, [OPR_REGISTER x]) ->
      move (mov_r x)
    | (OP_MOVE_EXCEPTION, [OPR_REGISTER x]) ->
      (* FIXME: Not sure what to do here, unsupported for now *)
      failwith "UNSUPPORTED"
    | (OP_RETURN_VOID)
    | (OP_RETURN)
    | (OP_RETURN_WIDE)
    | (OP_RETURN_OBJECT) ->
      (* FIXME: Not sure what to do here, unsupported for now *)
      failwith "UNSUPPORTED"
    | (OP_CONST_4, [OPR_REGISTER x; OPR_CONST c]) ->
      move (mov_rc x c)
    | (OP_CONST_16, [OPR_REGISTER x; OPR_CONST c]) ->
      move (mov_rc16 x c)
    | (OP_CONST, [OPR_REGISTER x; OPR_CONST c]) ->
      move (mov_rc32 x c)
    | (OP_CONST_HIGH16, [OPR_REGISTER x; OPR_CONST c]) ->
      move (mov_rc16 x c)
    | (OP_CONST_WIDE_16, [OPR_REGISTER x; OPR_CONST c]) ->
      move (movw_rc16 x c)
    | (OP_CONST_WIDE_32, [OPR_REGISTER x; OPR_CONST c]) ->
      move (movw_rc32 x c)
    | (OP_CONST_WIDE, [OPR_REGISTER x; OPR_CONST c]) ->
      (* FIXME: *)
      failwith "UNIMPLEMENTED"
    | (OP_CONST_WIDE_HIGH16, [OPR_REGISTER x; OPR_CONST c]) ->
      move (movw_rc16 x c)
    (* Index based operations load the indexes from the corresponding DEX section *)
    | (OP_CONST_STRING, [OPR_REGISTER x; OPR_INDEX i]) ->
      move (movi x i)
    (* FIXME: What's the difference? *)
    | (OP_CONST_STRING_JUMBO, [OPR_REGISTER x; OPR_INDEX i]) ->
      move (movi x i)
    | (OP_CONST_CLASS, [OPR_REGISTER x; OPR_INDEX i]) ->
      move (movi x i)
    | (OP_MONITOR_ENTER, [OPR_REGISTER _]) ->
      nop
    | (OP_MONITOR_EXIT, [OPR_REGISTER _]) ->
      nop
    | (OP_CHECK_CAST, [OPR_REGISTER x; OPR_INDEX i]) ->
      (* FIXME: What to do here? *)
      nop
    | (OP_INSTANCE_OF, [OPR_REGISTER x; OPR_REGISTER y; OPR_INDEX i]) ->
      (* FIXME: What to do here? *)
      nop
    | (OP_ARRAY_LENGTH, [OPR_REGISTER x; OPR_REGISTER y]) ->
      (* FIXME: What to do here? *)
      nop
    | (OP_NEW_INSTANCE, [OPR_REGISTER x; OPR_INDEX i]) ->
      (* FIXME: What to do here? *)
      nop
    | (OP_NEW_ARRAY, [OPR_REGISTER dst; OPR_REGISTER size; OPR_INDEX i]) ->
      let len = int reg_name size in
      new_array dst len i
    (* This one, not sure what to do - is this a variable-length array? *)
    | (OP_FILLED_NEW_ARRAY, [OPR_REGISTER dst; OPR_INDEX i; OPR_CONST data]) ->
      let len = Types.get_length i in
      filled_new_array (M4.int dst) (M32.int64 len) (M32.int64 data)
    | (OP_FILLED_NEW_ARRAY_RANGE, [OPR_REGISTER dst; OPR_INDEX i; OPR_REGISTER y]) ->
      filled_new_array (M8.int dst) (M32.int64 len)
    | (OP_FILL_ARRAY_DATA, [OPR_REGISTER dst; OPR_OFFSET data]) ->
      fill_array_data (M8.int dst) (M32.int64 data)
    | (OP_THROW, _) -> nop
    | (OP_GOTO, [OPR_OFFSET o]) ->
      jmp o
    | (OP_GOTO_16, [OPR_OFFSET o]) ->
      jmp o
    | (OP_GOTO_32, [OPR_OFFSET o]) ->
      jmp o
    (* TODO: Switch tables *)
    | (OP_PACKED_SWITCH, [OPR_REGISTER x; OPR_OFFSET o]) ->
      failwith "UNIMPLEMENTED"
    | (OP_SPARSE_SWITCH, [OPR_REGISTER x; OPR_OFFSET o]) ->
      failwith "UNIMPLEMENTED"
    | (OP_CMPL_FLOAT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
      failwith "NO FLOATS YET"
    | (OP_CMPG_FLOAT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
      failwith "NO FLOATS yET"
    | (OP_CMPL_DOUBLE, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
      failwith "NO FLOATS YET"
    | (OP_CMPG_DOUBLE, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
      failwith "NO FLOATS YET"
    | (OP_CMP_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
      cmp dest x y
    | (OP_IF_EQ, [OPR_REGISTER x; OPR_REGISTER y; OPR_OFFSET o]) ->
      if eq x y then jmp o
    | (OP_IF_NE, [OPR_REGISTER x; OPR_REGISTEr y; OPR_OFFSET o]) ->
      if neq x y then jmp o
    | (OP_IF_LT, [OPR_REGISTER x; OPR_REGISTER y; OPR_OFFSET o]) ->
      if slt x y then jmp o
    | (OP_IF_GE, [OPR_REGISTER x; OPR_REGISTER y; OPR_OFFSET o]) ->
      if sgt x y || eq x y then jmp o
    | (OP_IF_GT, [OPR_REGISTER x; OPR_REGISTER y; OPR_OFFSET o]) ->
      if sgt x y then jmp o
    | (OP_IF_LE, [OPR_REGISTER x; OPR_REGISTER y; OPR_OFFSET o]) ->
      if slt x y || eq x y then jmp o
    | (OP_IF_EQZ, [OPR_REGISTER x; OPR_OFFSET o]) ->
      if eq x zero then jmp o
    | (OP_IF_NEZ, [OPR_REGISTER x; OPR_OFFSET o]) ->
      if neq x zero then jmp o
    | (OP_IF_LTZ, [OPR_REGISTER x; OPR_OFFSET o]) ->
      if slt x zero then jmp o
    | (OP_IF_GEZ, [OPR_REGISTER x; OPR_OFFSET o]) ->
      if sgt x zero || eq x zero then jmp o
    | (OP_IF_GTZ, [OPR_REGISTER x; OPR_OFFSET o]) ->
      if sgt x zero then jmp o
    | (OP_IF_LEZ, [OPR_REGISTER x; OPR_OFFSET o]) ->
      if sle x zero || eq x zero then jmp o

    | (OP_AGET, [OPR_REGISTER dst; OPR_REGISTER a; OPR_INDEX i]) ->
      array_get (M8.int dst) (M8.int a) (M8.int i)
    | (OP_AGET_WIDE, [OPR_REGISTER dst; OPR_REGISTER a; OPR_INDEX i]) ->
      (* reads array element into register pair *)
      array_getw (M8.int dst) (M8.int a) (M8.int i)
    | (OP_AGET_OBJECT, [OPR_REGISTER dst; OPR_REGISTER a; OPR_INDEX i]) ->
      (* FIXME: figure out the size of the object first? *)
      array_get (M8.int dst) (M8.int a) (M8.int i)
    | (OP_AGET_BOOLEAN, [OPR_REGISTER dst; OPR_REGISTER a; OPR_INDEX i]) ->
      (* FIXME: what is the ACTUAL semantics of these instructions? *)
      array_get (M8.int dst) (M8.int a) (M8.int i)
    | (OP_AGET_BYTE, [OPR_REGISTER dst; OPR_REGISTER a; OPR_INDEX i]) ->
      array_get (M8.int dst) (M8.int a) (M8.int i)
    | (OP_AGET_CHAR, [OPR_REGISTER dst; OPR_REGISTER a; OPR_INDEX i]) ->
      array_get (M8.dst) (M8.int a) (M8.int i)
    | (OP_AGET_SHORT, [OPR_REGISTER dst; OPR_REGISTER a; OPR_INDEX i]) ->
      array_get (M8.int dst) (M8.int a) (M8.int i)
    | (OP_APUT, [OPR_REGISTER src; OPR_REGISTER a; OPR_INDEX i]) ->
      array_put (M8.int a) (M8.int src) (M8.int i)
    | (OP_APUT_WIDE, [OPR_REGISTER src; OPR_REGISTER a; OPR_INDEX i]) ->
      (* puts an array element from the register pair *)
      array_put (M8.int a) (M8.int src) (M8.int i)
    | (OP_APUT_OBJECT, [OPR_REGISTER src; OPR_REGISTER a; OPR_INDEX i]) ->
      (* FIXME: figure out the size of the object first? *)
      array_put (M8.int a) (M8.int src) (M8.int i)
    | (OP_APUT_BOOLEAN, [OPR_REGISTER src; OPR_REGISTER a; OPR_INDEX i]) ->
      array_put (M8.int a) (M8.int src) (M8.int i)
    | (OP_APUT_BYTE, [OPR_REGISTER src; OPR_REGISTER a; OPR_INDEX i]) ->
      array_put (M8.int a) (M8.int src) (M8.int i)
    | (OP_APUT_CHAR, [OPR_REGISTER src; OPR_REGISTER a; OPR_INDEX i]) ->
      array_put (M8.int a) (M8.int src) (M8.int i)
    | (OP_APUT_SHORT, [OPR_REGISTER src; OPR_REGISTER a; OPR_INDEX i]) ->
      array_put (M8.int a) (M8.int src) (M8.int i)

    (* Semantically these are identical to the array operations *)
    | (OP_IGET, [OPR_REGISTER dst; OPR_REGISTER o; OPR_INDEX i]) ->
        array_get (M4.int dst) (M4.int o) (M16.int i)
    | (OP_IGET_WIDE, [OPR_REGISTER dst; OPR_REGISTER o; OPR_INDEX i]) ->
        array_getw (M4.int dst) (M4.int o) (M16.int i)
    | (OP_IGET_OBJECT, [OPR_REGISTER dst; OPR_REGISTER o; OPR_INDEX i]) ->
      (* FIXME: figure out the size of the object first? *)
        array_get (M4.int dst) (M4.int o) (M16.int i)
      (* FIXME: what is the ACTUAL semantics of these instructions? *)
    | (OP_IGET_BOOLEAN, [OPR_REGISTER dst; OPR_REGISTER o; OPR_INDEX i]) ->
        array_get (M4.int dst) (M4.int o) (M16.int i)
    | (OP_IGET_BYTE, [OPR_REGISTER dst; OPR_REGISTER o; OPR_INDEX i]) ->
        array_get (M4.int dst) (M4.int o) (M16.int i)
    | (OP_IGET_CHAR, [OPR_REGISTER dst; OPR_REGISTER o; OPR_INDEX i]) ->
        array_get (M4.int dst) (M4.int o) (M16.int i)
    | (OP_IGET_SHORT, [OPR_REGISTER dst; OPR_REGISTER o; OPR_INDEX i]) ->
        array_get (M4.int dst) (M4.int o) (M16.int i)
    | (OP_IPUT, [OPR_REGISTER src; OPR_REGISTER o; OPR_INDEX i]) ->
        array_put (M4.int o) (M4.int src) (M16.int i)
    | (OP_IPUT_WIDE, [OPR_REGISTER src; OPR_REGISTER o; OPR_INDEX i]) ->
        array_putw (M4.int o) (M4.int src) (M16.int i)
    | (OP_IPUT_OBJECT, [OPR_REGISTER src; OPR_REGISTER o; OPR_INDEX i]) ->
      (* FIXME: figure out the size of the object first? *)
        array_put (M4.int o) (M4.int src) (M16.int i)
    | (OP_IPUT_BOOLEAN, [OPR_REGISTER src; OPR_REGISTER o; OPR_INDEX i]) ->
        array_put (M4.int o) (M4.int src) (M16.int i)
    | (OP_IPUT_BYTE, [OPR_REGISTER src; OPR_REGISTER o; OPR_INDEX i]) ->
        array_put (M4.int o) (M4.int src) (M16.int i)
    | (OP_IPUT_CHAR, [OPR_REGISTER src; OPR_REGISTER o; OPR_INDEX i]) ->
        array_put (M4.int o) (M4.int src) (M16.int i)
    | (OP_IPUT_SHORT, [OPR_REGISTER src; OPR_REGISTER o; OPR_INDEX i]) ->
        array_put (M4.int o) (M4.int src) (M16.int i)

    | (OP_SGET, [OPR_REGISTER dst; OPR_INDEX i]) ->
        array_get_static (M4.int dst) (M16.int i)
    | (OP_SGET_WIDE, [OPR_REGISTER dst; OPR_INDEX i]) ->
        array_get_static (M4.int dst) (M16.int i)
    | (OP_SGET_OBJECT, [OPR_REGISTER dst; OPR_INDEX i]) ->
        array_get_static (M4.int dst) (M16.int i)
    | (OP_SGET_BOOLEAN, [OPR_REGISTER dst; OPR_INDEX i]) ->
        array_get_static (M4.int dst) (M16.int i) 1
    | (OP_SGET_BYTE, [OPR_REGISTER x; OPR_INDEX i]) ->
        array_get_static (M4.int dst) (M16.int i) 1
    | (OP_SGET_CHAR, [OPR_REGISTER dst; OPR_INDEX i]) ->
        array_get_static (M4.int dst) (M16.int i) 1
    | (OP_SGET_SHORT, [OPR_REGISTER dst; OPR_INDEX i]) ->
        array_get_static (M4.int dst) (M16.int i) 2
    | (OP_SPUT, [OPR_REGISTER src; OPR_INDEX i]) ->
        array_put_static (M4.int src) (M16.int i)
    | (OP_SPUT_WIDE, [OPR_REGISTER src; OPR_INDEX i]) ->
        array_put_static (M4.int src) (M16.int i)
    | (OP_SPUT_OBJECT, [OPR_REGISTER src; OPR_INDEX i]) ->
        array_put_static (M4.int src) (M16.int i)
    | (OP_SPUT_BOOLEAN, [OPR_REGISTER x; OPR_INDEX i]) ->
        array_put_static (M4.int src) (M16.int i) 1
    | (OP_SPUT_BYTE, [OPR_REGISTER src; OPR_INDEX i]) ->
        array_put_static (M4.int src) (M16.int i) 1
    | (OP_SPUT_CHAR, [OPR_REGISTER src; OPR_INDEX i]) ->
        array_put_static (M4.int src) (M16.int i) 1
    | (OP_SPUT_SHORT, [OPR_REGISTER x; OPR_INDEX i]) ->
        array_put_static (M4.int src) (M16.int i) 2

    (* regs here is just a list of registers, amount is variadic *)
    | (OP_INVOKE_VIRTUAL, [OPR_CONST c; OPR_INDEX i; regs]) ->
        (* Call jump here? *)
    | (OP_INVOKE_SUPER, [OPR_CONST c; OPR_INDEX i; regs]) ->
    | (OP_INVOKE_DIRECT, [OPR_CONST c; OPR_INDEX i; regs]) ->
    | (OP_INVOKE_STATIC, [OPR_CONST c; OPR_INDEX i; regs]) ->
    | (OP_INVOKE_INTERFACE, [OPR_CONST c; OPR_INDEX i; regs]) ->

    | (OP_INVOKE_VIRTUAL_RANGE, [OPR_CONST c; OPR_INDEX i; OPR_REGISTER x]) ->
    | (OP_INVOKE_SUPER_RANGE, [OPR_CONST c; OPR_INDEX i; OPR_REGISTER x]) ->
    | (OP_INVOKE_DIRECT_RANGE, [OPR_CONST c; OPR_INDEX i; OPR_REGISTER x]) ->
    | (OP_INVOKE_STATIC_RANGE, [OPR_CONST c; OPR_INDEX i; OPR_REGISTER x]) ->
    | (OP_INVOKE_INTERFACE_RANGE, [OPR_CONST c; OPR_INDEX i; OPR_REGISTER x]) ->

    | (OP_NEG_INT, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply neg x y
    | (OP_NOT_INT, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply not x y
    | (OP_NEG_LONG, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply neg x y
    | (OP_NOT_LONG, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply not x y
    | (OP_NEG_FLOAT, [OPR_REGISTER x; OPR_REGISTER y]) ->
         failwith "floats are unimplemented"
    | (OP_NEG_DOUBLE, [OPR_REGISTER x; OPR_REGISTER y]) ->
         failwith "floats are unimplemented"
    | (OP_INT_TO_LONG, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_INT_TO_FLOAT, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_INT_TO_DOUBLE, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_LONG_TO_INT, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_LONG_TO_FLOAT, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_LONG_TO_DOUBLE, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_FLOAT_TO_INT, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_FLOAT_TO_LONG, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_FLOAT_TO_DOUBLE, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_DOUBLE_TO_INT, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_DOUBLE_TO_LONG, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_DOUBLE_TO_FLOAT, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_INT_TO_BYTE, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_INT_TO_CHAR, [OPR_REGISTER x; OPR_REGISTER y])
    | (OP_INT_TO_SHORT, [OPR_REGISTER x; OPR_REGISTER y]) ->
         failwith "unimplemented"

    | (OP_ADD_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 add dest x y
    | (OP_SUB_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 sub dest x y
    | (OP_MUL_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 mul dest x y
    | (OP_DIV_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 div dest x y
    | (OP_REM_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        (* is this "remnant"? *)
        apply2 modulo dest x y
    | (OP_AND_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logand dest x y
    | (OP_OR_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logor dest x y
    | (OP_XOR_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logxor dest x y
    | (OP_SHL_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftl true) dest x y
    | (OP_SHR_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftr true) dest x y
    | (OP_USHR_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftr true) dest x y

    | (OP_ADD_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 add dest x y
    | (OP_SUB_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 sub dest x y
    | (OP_MUL_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 mul dest x y
    | (OP_DIV_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 div dest x y
    | (OP_REM_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        (* is this "remnant"? *)
        apply2 modulo dest x y
    | (OP_AND_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logand dest x y
    | (OP_OR_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logor dest x y
    | (OP_XOR_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logxor dest x y
    | (OP_SHL_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftl true) dest x y
    | (OP_SHR_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftr true) dest x y
    | (OP_USHR_LONG, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftr true) dest x y

    | (OP_ADD_FLOAT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fadd dest x y
    | (OP_SUB_FLOAT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fsub dest x y
    | (OP_MUL_FLOAT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fmul dest x y
    | (OP_DIV_FLOAT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fdiv dest x y
    | (OP_REM_FLOAT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fmodulo dest x y
    | (OP_ADD_DOUBLE, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fadd dest x y
    | (OP_SUB_DOUBLE, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fsub dest x y
    | (OP_MUL_DOUBLE, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fmul dest x y
    | (OP_DIV_DOUBLE, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fdiv dest x y
    | (OP_REM_DOUBLE, [OPR_REGISTER dest; OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fmodulo dest x y

    | (OP_ADD_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 add x x y
    | (OP_SUB_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 sub x x y
    | (OP_MUL_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 mul x x y
    | (OP_DIV_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 div x x y
    | (OP_REM_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 modulo x x y
    | (OP_AND_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logand x x y
    | (OP_OR_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logor x x y
    | (OP_XOR_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logxor x x y
    | (OP_SHL_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftl true) x x y
    | (OP_SHR_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftr true) x x y
    | (OP_USHR_INT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftr true) x x y
    | (OP_ADD_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 add x x y
    | (OP_SUB_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 sub x x y
    | (OP_MUL_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 mul x x y
    | (OP_DIV_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 div x x y
    | (OP_REM_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 modulo x x y
    | (OP_AND_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logand x x y
    | (OP_OR_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logor x x y
    | (OP_XOR_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 logxor x x y
    | (OP_SHL_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftl true) x x y
    | (OP_SHR_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftr true) x x y
    | (OP_USHR_LONG_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        apply2 (shiftr true) x x y

    | (OP_ADD_FLOAT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fadd x x y
    | (OP_SUB_FLOAT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fsub x x y
    | (OP_MUL_FLOAT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fmul x x y
    | (OP_DIV_FLOAT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fdiv x x y
    | (OP_REM_FLOAT_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fmodulo x x y
    | (OP_ADD_DOUBLE_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fadd x x y
    | (OP_SUB_DOUBLE_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fsub x x y
    | (OP_MUL_DOUBLE_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fmul x x y
    | (OP_DIV_DOUBLE_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fdiv x x y
    | (OP_REM_DOUBLE_2ADDR, [OPR_REGISTER x; OPR_REGISTER y]) ->
        fapply2 fmodulo x x y

    | (OP_ADD_INT_LIT16, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c add dest x c
    | (OP_RSUB_INT, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c sub dest x c (* FIXME: reverse it! *)
    | (OP_MUL_INT_LIT16, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c mul dest x c
    | (OP_DIV_INT_LIT16, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c mul dest x c
    | (OP_REM_INT_LIT16, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c modulo dest x c
    | (OP_AND_INT_LIT16, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c modulo dest x c
    | (OP_OR_INT_LIT16, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c logor dest x c
    | (OP_XOR_INT_LIT16, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c logxor dest x c
    | (OP_ADD_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c add dest x c
    | (OP_RSUB_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c sub dest x c (* FIXME: reverse it! *)
    | (OP_MUL_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c mul dest x c
    | (OP_DIV_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c div dest x c
    | (OP_REM_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c modulo dest x c
    | (OP_AND_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c logand dest x c
    | (OP_OR_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c logor dest x c
    | (OP_XOR_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c logxor dest x c
    | (OP_SHL_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c (shiftl true) dest x c
    | (OP_SHR_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c (shiftr true) dest x c
    | (OP_USHR_INT_LIT8, [OPR_REGISTER dest; OPR_REGISTER x; OPR_CONST c]) ->
        apply2c (shiftr true) dest x c
      (* optimizer output -- these are never generated by "dx" *)

    | (OP_THROW_VERIFICATION_ERROR, _)
    | (OP_EXECUTE_INLINE, _)
    | (OP_EXECUTE_INLINE_RANGE, _)
    | (OP_INVOKE_DIRECT_EMPTY, _)
    | (OP_IGET_QUICK, _)
    | (OP_IGET_WIDE_QUICK, _)
    | (OP_IGET_OBJECT_QUICK, _)
    | (OP_IPUT_QUICK, _)
    | (OP_IPUT_WIDE_QUICK, _)
    | (OP_IPUT_OBJECT_QUICK, _)

    | (OP_INVOKE_VIRTUAL_QUICK, _)
    | (OP_INVOKE_VIRTUAL_QUICK_RANGE, _)
    | (OP_INVOKE_SUPER_QUICK, _)
    | (OP_INVOKE_SUPER_QUICK_RANGE, _) ->
        nop

    | _ -> failwith "not ready"

end

module Lifter = Dalvik(Theory.Manager)

let lift opcode =
  KB.Object.create Theory.Program.cls >>= fun insn ->
  Lifter.run opcode >>= fun sema ->
  KB.provide Theory.Program.Semantics.slot insn sema >>| fun () ->
  insn

let test opcode =
  match KB.run Theory.Program.cls (lift opcode) KB.empty with
  | Error _ -> failwith "Oops, we've got a conflict!"
  | Ok (code,_) ->
    Format.printf "%a@\n" KB.Value.pp code
