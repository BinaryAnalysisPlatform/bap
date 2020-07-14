open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Armng_env.Env
module Defs = Armng_defs


module Mem_Multi(Core : Theory.Core) = struct
  open Core
  module DSL = Armng_dsl.Make(Core)
  module Flags = Armng_flags.Flags(Core)
  module Shift = Armng_shift.Shift(Core)
  module Cond = Armng_cond.Cond(Core)
  module Var = Theory.Var
  open Flags
  open Cond

  let mem_store address value = DSL.(Env.memory := storew b0 (var Env.memory) address value)

  let mem_load sort address = loadw sort b0 (var Env.memory) address

  let stmda base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            mem_store (!$base - !!4 * !!it) !$reg
          ])
      ]
    ]

  let stmda_upd base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            mem_store (!$base - !!4 * !!it) !$reg
          ]);
        !$$base := !$base - !!4 * !!(List.length dest_list)
      ]
    ]

  let ldmib base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            !$$reg := mem_load Env.value (!$base + !!4 * !!Int.(it + 1))
          ])
      ]
    ]

  let ldmib_upd base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            !$$reg := mem_load Env.value (!$base + !!4 * !!Int.(it + 1))
          ]);
        !$$base := !$base + !!4 * !!Int.(List.length dest_list + 1)
      ]
    ]

  let stmib base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            mem_store (!$base + !!4 * !!Int.(it + 1)) !$reg
          ])
      ]
    ]

  let stmib_upd base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            mem_store (!$base + !!4 * !!Int.(it + 1)) !$reg
          ]);
        !$$base := !$base + !!4 * !!Int.(List.length dest_list + 1)
      ]
    ]

  let ldmdb base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            !$$reg := mem_load Env.value (!$base - !!4 * !!Int.(it + 1))
          ])
      ]
    ]

  let ldmdb_upd base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            !$$reg := mem_load Env.value (!$base - !!4 * !!Int.(it + 1))
          ]);
        !$$base := !$base - !!4 * !!Int.(List.length dest_list + 1)
      ]
    ]

  let stmdb base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            mem_store (!$base - !!4 * !!Int.(it + 1)) !$reg
          ])
      ]
    ]

  let stmdb_upd base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            mem_store (!$base - !!4 * !!Int.(it + 1)) !$reg
          ]);
        !$$base := !$base - !!4 * !!Int.(List.length dest_list + 1)
      ]
    ]

  let ldmia base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            !$$reg := mem_load Env.value (!$base + !!4 * !!it)
          ])
      ]
    ]

  let ldmia_upd base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            !$$reg := mem_load Env.value (!$base + !!4 * !!it)
          ]);
        !$$base := !$base + !!4 * !!(List.length dest_list)
      ]
    ]

  let stmia base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            mem_store (!$base + !!4 * !!it) !$reg
          ])
      ]
    ]

  let stmia_upd base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            mem_store (!$base + !!4 * !!it) !$reg
          ]);
        !$$base := !$base + !!4 * !!(List.length dest_list)
      ]
    ]

  let ldmda base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            !$$reg := mem_load Env.value (!$base - !!4 * !!it)
          ])
      ]
    ]

  let ldmda_upd base cond _wr_flag dest_list =
    DSL.[
      if_ (resolve_cond cond) [
        foreach_ (List.rev dest_list) (fun reg it -> !%[
            !$$reg := mem_load Env.value (!$base - !!4 * !!it)
          ]);
        !$$base := !$base - !!4 * !!(List.length dest_list)
      ]
    ]

end

module Mem(Core : Theory.Core) = struct
  open Core
  module DSL = Armng_dsl.Make(Core)
  module Flags = Armng_flags.Flags(Core)
  module Shift = Armng_shift.Shift(Core)
  module Cond = Armng_cond.Cond(Core)
  module Var = Theory.Var
  open Flags
  open Cond

  let word x = Word.of_int x ~width:32


  module Z = Word.Int_exn
  let repair_imm (src : word) ~sign_mask ~imm_mask rtype =
    let open Defs in
    let bit_set =
      Word.(Z.(word sign_mask land src) = word sign_mask) in
    let negate =
      (bit_set && [%compare.equal: repair] rtype `NEG) ||
      (Base.not bit_set && [%compare.equal: repair] rtype `POS) in
    let offset = Z.(src land word imm_mask) in
    DSL.word_as_bitv (if negate then Z.neg offset else offset)

  let repair_reg reg imm ~sign_mask rtype =
    let open Defs in
    let bit_set =
      Word.(Z.(word sign_mask land imm) = word sign_mask) in
    let negate =
      (bit_set && [%compare.equal: repair] rtype `NEG) ||
      (Base.not bit_set && [%compare.equal: repair] rtype `POS)
    in
    let m_one = Word.(ones (bitwidth imm))  in
    if negate then DSL.(word_as_bitv m_one * reg) else reg

  let mem_offset_reg_or_imm_neg reg_off imm_off =
    let open Defs in
    match reg_off with
    | `Reg #nil_reg ->
      repair_imm imm_off ~sign_mask:0x100 ~imm_mask:0xff `NEG
    | `Reg #gpr_reg ->
      repair_reg DSL.(assert_val reg_off) imm_off ~sign_mask:0x100 `NEG
    | _ -> raise (Defs.Lift_Error "unexpected operand")

  let mem_offset_reg_or_imm_pos reg_off imm_off =
    let open Defs in
    match reg_off with
    | `Reg #nil_reg ->
      repair_imm imm_off ~sign_mask:0x100 ~imm_mask:0xff `POS
    | `Reg #gpr_reg ->
      repair_reg DSL.(assert_val reg_off) imm_off ~sign_mask:0x1 `POS
    | _ -> raise (Defs.Lift_Error "unexpected operand")

  let lift_offset ?shift offset =
    let offset = match offset with
      | `Reg _ -> DSL.assert_val offset
      | `Imm w ->
        let int32_min = Word.(one 32 lsl Word.of_int 31 32) in
        if Word.(w = int32_min)
        then DSL.imm 0
        else DSL.word_as_bitv w in
    match shift with
    | Some s -> Shift.mem_shift ~src:offset s
    | None -> offset

  let mem_store address value = DSL.(Env.memory := storew b0 (var Env.memory) address value)

  let mem_load sort address = loadw sort b0 (var Env.memory) address

  let strd dest1 dest2 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) !$dest1;
        mem_store (!$base + offset + !!4) !$dest2;
      ]
    ]

  let ldrd dest1 dest2 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value (!$base + offset);
        !$$dest2 := mem_load Env.value (!$base + offset + !!4);
      ]
    ]

  let strd_post dest1 dest2 base _unknown reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base !$dest1;
        mem_store (!$base + !!4) !$dest2;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  (** pc should be treated discriminately *)
  let ldrd_post dest1 dest2 base _unknown reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value !$base;
        !$$dest2 := mem_load Env.value (!$base + !!4);
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let strd_pre _unknown dest1 dest2 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) !$dest1;
        mem_store (!$base + offset + !!4) !$dest2;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrd_pre dest1 dest2 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value (!$base + offset);
        !$$dest2 := mem_load Env.value (!$base + offset + !!4);
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let strh dest1 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) (extend_to Env.half_word !$dest1);
      ]
    ]

  let ldrh dest1 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word (!$base + offset) |> extend;
      ]
    ]

  let strh_post _unknown dest1 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base (extend_to Env.half_word !$dest1);
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let strhtr _unknown dest1 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_pos reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base (extend_to Env.half_word !$dest1);
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  (** pc should be treated discriminately *)
  let ldrh_post dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word !$base |> extend;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrhtr dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_pos reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word !$base |> extend;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let strh_pre _unknown dest1 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) (extend_to Env.half_word !$dest1);
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrh_pre dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word (!$base + offset) |> extend;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrsh dest1 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word (!$base + offset) |> extend_signed;
      ]
    ]

  (** pc should be treated discriminately *)
  let ldrsh_post dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word !$base |> extend_signed;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrsh_pre dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word (!$base + offset) |> extend_signed;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrshtr dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_pos reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word !$base |> extend_signed;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrshti dest1 _unknown base imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_pos (`Reg `Nil) imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word !$base |> extend_signed;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrsb dest1 base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte (!$base + offset) |> extend_signed;
      ]
    ]

  (** pc should be treated discriminately *)
  let ldrsb_post dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte !$base |> extend_signed;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrsb_pre dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_neg reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte (!$base + offset) |> extend_signed;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrsbtr dest1 _unknown base reg_off imm_off cond =
    let imm_off = Defs.assert_imm imm_off in
    let offset = mem_offset_reg_or_imm_pos reg_off imm_off in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte !$base |> extend_signed;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let stri12 dest1 base offset cond =
    let offset = lift_offset offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) !$dest1
      ]
    ]

  let ldri12 dest1 base offset cond =
    let offset = lift_offset offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value (!$base + offset)
      ]
    ]

  let strbi12 dest1 base offset cond =
    let offset = lift_offset offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) (extend_to Env.byte !$dest1)
      ]
    ]

  let ldrbi12 dest1 base offset cond =
    let offset = lift_offset offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte (!$base + offset) |> extend
      ]
    ]

  let strrs dest1 base offset shift cond =
    let offset = lift_offset offset ~shift in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) !$dest1
      ]
    ]

  let ldrrs dest1 base offset shift cond =
    let offset = lift_offset offset ~shift in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value (!$base + offset)
      ]
    ]

  let strbrs dest1 base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) (extend_to Env.byte !$dest1)
      ]
    ]

  let ldrbrs dest1 base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte (!$base + offset) |> extend
      ]
    ]

  let str_post_imm _unknown dest1 base _invalid offset cond =
    let 
      offset = repair_imm (Defs.assert_imm offset) ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value !$base;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldr_post_imm dest1 _unknown base _invalid offset cond =
    let 
      offset = repair_imm (Defs.assert_imm offset) ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base !$dest1;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let strb_post_imm _unknown dest1 base _invalid offset cond =
    let 
      offset = repair_imm (Defs.assert_imm offset) ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte !$base |> extend;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let strbt_post_imm _unknown dest1 base _invalid offset cond =
    let 
      offset = repair_imm (Defs.assert_imm offset) ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte !$base |> extend;
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrb_post_imm _unknown dest1 base _invalid offset cond =
    let 
      offset = repair_imm (Defs.assert_imm offset) ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base (extend_to Env.byte !$dest1);
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let ldrbt_post_imm _unknown dest1 base _invalid offset cond =
    let 
      offset = repair_imm (Defs.assert_imm offset) ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base (extend_to Env.byte !$dest1);
        (* write back *)
        !$$base := !$base + offset
      ]
    ]

  let str_post_reg _unknown dest1 base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base !$dest1;
        !$$base := !$base + offset
      ]
    ]

  let strt_post_reg _unknown dest1 base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base !$dest1;
        !$$base := !$base + offset
      ]
    ]

  let ldr_post_reg dest1 _unknown base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value !$base;
        !$$base := !$base + offset
      ]
    ]

  let ldrt_post_reg dest1 _unknown base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value !$base;
        !$$base := !$base + offset
      ]
    ]

  let strb_post_reg _unknown dest1 base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base (extend_to Env.byte !$dest1);
        !$$base := !$base + offset
      ]
    ]

  let strbt_post_reg _unknown dest1 base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base (extend_to Env.byte !$dest1);
        !$$base := !$base + offset
      ]
    ]

  let ldrb_post_reg dest1 _unknown base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte !$base |> extend;
        !$$base := !$base + offset
      ]
    ]

  let ldrbt_post_reg dest1 _unknown base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte !$base |> extend;
        !$$base := !$base + offset
      ]
    ]

  let str_pre_imm _unknown dest1 base offset cond =
    let offset = lift_offset offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) !$dest1;
        !$$base := !$base + offset
      ]
    ]

  let ldr_pre_imm dest1 _unknown base offset cond =
    let offset = lift_offset offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value (!$base + offset);
        !$$base := !$base + offset
      ]
    ]

  let strb_pre_imm _unknown dest1 base offset cond =
    let offset = lift_offset offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) (extend_to Env.byte !$dest1);
        !$$base := !$base + offset
      ]
    ]

  let ldrb_pre_imm dest1 _unknown base offset cond =
    let offset = lift_offset offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte (!$base + offset) |> extend;
        !$$base := !$base + offset
      ]
    ]

  let str_pre_reg _unknown dest1 base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) !$dest1;
        !$$base := !$base + offset
      ]
    ]

  let ldr_pre_reg dest1 _unknown base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value (!$base + offset);
        !$$base := !$base + offset
      ]
    ]

  let strb_pre_reg _unknown dest1 base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        mem_store (!$base + offset) (extend_to Env.byte !$dest1);
        !$$base := !$base + offset
      ]
    ]

  let ldrb_pre_reg dest1 _unknown base offset shift cond =
    let offset = lift_offset ~shift offset in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte (!$base + offset) |> extend;
        !$$base := !$base + offset
      ]
    ]

  let ldrex dest1 base cond =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.value !$base;
      ]
    ]

  let ldrexb dest1 base cond =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.byte !$base |> extend;
      ]
    ]

  let ldrexh dest1 base cond =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest1 := mem_load Env.half_word !$base |> extend;
      ]
    ]

  let ldrexd multidest base cond =
    DSL.[
      if_ (resolve_cond cond) [
        !$$multidest := mem_load Env.value !$base;
      ]
    ]

  let strex dest1 src1 base cond =
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base !$src1;
        !$$dest1 := imm 0
      ]
    ]

  let strexb dest1 src1 base cond =
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base (extend_to Env.byte !$src1);
        !$$dest1 := imm 0
      ]
    ]

  let strexh dest1 src1 base cond =
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base (extend_to Env.half_word !$src1);
        !$$dest1 := imm 0
      ]
    ]

  let strexd dest1 multidest base cond =
    DSL.[
      if_ (resolve_cond cond) [
        mem_store !$base !$multidest;
        !$$dest1 := imm 0
      ]
    ]
end