open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_arm_types
open Bap_disasm_arm_utils

module Arm = Bap_disasm_arm
module Env = Bap_disasm_arm_env

(** Memory access operations *)


(* Doug TODO check for misaligned access *)
(* Single-register memory access *)
let lift_r  ~(dst1 : Var.t) ?(dst2 : Var.t option) ~(base : Var.t)
    ~(offset : exp) mode sign size operation =
  let o_base   = Env.new_tmp "base" in
  (* If this load is a jump (only valid for 4-byte load)
   * We need to do the write_back before the load so we
   * Use the originals
   **)
  let address = match mode, operation, size, dst1 with
    | PostIndex, Ld, W, d when d = Env.pc -> Exp.var o_base
    | PreIndex, Ld, W, d when d = Env.pc  -> Exp.(var o_base + offset)
    | PostIndex, _,  _, _               -> Exp.var base
    | PreIndex, _, _, _ | Offset, _, _, _ -> Exp.(var base + offset) in

  (* Create temps for original if this is a jump *)
  let pre_write_back = match mode, operation, size, dst1 with
    | PreIndex,  Ld, W, d when d = Env.pc -> [
        Stmt.move o_base Exp.(var base);
        Stmt.move base  Exp.(var base + offset)
      ]
    | PostIndex, Ld, W, d when d = Env.pc -> [
        Stmt.move o_base  Exp.(var base);
        Stmt.move base Exp.(var base + offset)
      ]
    | Offset, _, _, _ -> []
    | _ -> [] in

  let write_back = match mode, operation, size, dst1 with
    | PreIndex,  Ld, W, d when d = Env.pc -> []
    | PostIndex, Ld, W, d when d = Env.pc -> []
    | Offset,    _,  _, _               -> []
    | _ -> [Stmt.move base Exp.(var base + offset)] in

  let typ = match size with
    | B -> `r8
    | H -> `r16
    | W | D -> `r32 in

  let store m n v = Exp.(store m n v LittleEndian typ) in
  let load  m n   = Exp.(load  m n LittleEndian typ) in

  let temp = match size with
    | B | H -> Env.new_tmp "t"
    | _ -> dst1 in

  let four = Exp.int (Word.of_int 4 ~width:32) in

  match operation with
  | Ld ->
    let rhs = cast_of_sign sign 32 Exp.(var temp) in
    let extend = match size with
      | B | H -> [Stmt.move dst1 rhs]
      | W | D -> [] in
    let loads =
      let mem = Exp.var (Env.mem) in
      if size = D then [
        Stmt.move dst1 (load mem address);
        Stmt.move (uw dst2) (load mem Exp.(address + four));
      ] else [
        assn temp (load mem address);
      ] in
    List.concat [
      pre_write_back;
      loads;
      extend;                  (* sign/zero extend if necessary *)
      write_back;
    ]
  | St ->
    (* truncate the value if necessary *)
    let trunc = match size with
      | B | H ->
        let n = if size = B then 8 else 16 in
        [Stmt.move temp Exp.(cast low n (var dst1))]
      | W | D -> [] in
    let stores =
      let m = Env.mem in
      let v = Exp.var m in
      match size with
      | D -> [
          Stmt.move m (store v address Exp.(var dst1));
          Stmt.move m (store v
                         Exp.(address + four) Exp.(var (uw dst2)));
        ]
      | B | H | W -> [
          Stmt.move m (store v address Exp.(var temp));
        ] in
    List.concat [
      trunc;                   (* truncate the value if necessary *)
      stores;
      write_back
    ]

let lift_m dest_list base mode update operation =
  let o_base = Env.new_tmp "base" in
  let calc_offset ith = match mode with
    | IB ->  4 * (ith + 1)
    | DB -> -4 * (ith + 1)
    | IA ->  4 * ith
    | DA -> -4 * ith in
  let writeback =
    let dest_len =
      Word.of_int ~width:32 (4 * List.length dest_list) in
    match update with
    | NoUpdate -> []
    | Update ->
      let (+-) = match mode with
        | IB | IA -> Exp.(+)
        | DB | DA -> Exp.(-)
      in [Stmt.move base Exp.(var base +-  int dest_len)] in
  let create_access i dest =
    let offset_e = Word.of_int ~width:32 (calc_offset i) in
    let mem = Exp.var Env.mem in
    let addr = Exp.(var o_base + int offset_e) in
    match operation with
    | Ld -> assn dest Exp.(load mem addr LittleEndian `r32)
    | St -> Stmt.move Env.mem
              Exp.(store Env.(var mem) addr (var dest) LittleEndian `r32) in
  (* Jmps should always be the last statement *)
  let rec move_jump_to_end l =
    match l with
      [] -> []
    | (stmt :: stmts) ->
      match stmt with
      | (Stmt.Jmp exp) -> stmts @ [Stmt.Jmp exp]
      |   _  -> stmt :: move_jump_to_end stmts in
  move_jump_to_end (List.concat [
      [Stmt.move o_base Exp.(var base)];
      List.mapi ~f:create_access dest_list;
      writeback
    ])
