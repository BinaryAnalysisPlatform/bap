(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)

module Bil = X86_legacy_bil

open Bil
open Bil.Ast
open Bil.Type
open Core_kernel
open X86_legacy_bil_convenience
open X86_legacy_bil_disasm_i386

module MC = Bap.Std.Disasm_expert.Basic
module Register = X86_legacy_bil_register
module Typecheck = X86_legacy_bil_typecheck
module Var_temp = X86_legacy_bil_var_temp
module Big_int_convenience = X86_legacy_bil_big_int_convenience
module Operands = X86_legacy_operands

exception Unsupported_opcode of string

let value_exn x = Option.value_exn x

module Fp_lifter = struct

  (** 8-bit low byte GPR registers *)
  type r8l = [
    | `AL | `BL | `CL | `DL
    | `SIL | `DIL | `BPL | `SPL
    | `R8B | `R9B | `R10B | `R11B
    | `R12B | `R13B | `R14B | `R15B
  ] [@@deriving sexp]

  (** 8-bit high-byte GPR registers *)
  type r8h = [`AH | `BH | `CH | `DH ] [@@deriving sexp]

  (** all 8 bit GPR registers *)
  type r8 = [r8l | r8h] [@@deriving sexp]

  (** 16-bit GPR registers *)
  type r16 = [
    |`AX | `BX | `CX | `DX
    | `DI | `SI | `BP | `SP
    | `R8W | `R9W | `R10W | `R11W
    | `R12W | `R13W | `R14W | `R15W
  ] [@@deriving sexp]

  (** 32-bit GPR registers *)
  type r32 = [
    | `EAX | `EBX | `ECX | `EDX
    | `EDI | `ESI | `EBP | `ESP
    | `R8D | `R9D | `R10D | `R11D
    | `R12D | `R13D | `R14D | `R15D
  ] [@@deriving sexp]

  type r64 = [
    | `RAX | `RBX | `RCX | `RDX
    | `RDI | `RSI | `RBP | `RSP
    | `R8 | `R9 | `R10 | `R11
    | `R12 | `R13 | `R14 | `R15
  ] [@@deriving sexp]

  type r128 = [
    | `XMM0  | `XMM1  | `XMM2  | `XMM3
    | `XMM4  | `XMM5  | `XMM6  | `XMM7
    | `XMM8  | `XMM9  | `XMM10 | `XMM11
    | `XMM12 | `XMM13 | `XMM14 | `XMM15
    | `XMM16 | `XMM17 | `XMM18 | `XMM19
    | `XMM20 | `XMM21 | `XMM22 | `XMM23
    | `XMM24 | `XMM25 | `XMM26 | `XMM27
    | `XMM28 | `XMM29 | `XMM30 | `XMM31
  ] [@@deriving sexp]

  type r256 = [
    | `YMM0  | `YMM1  | `YMM2  | `YMM3
    | `YMM4  | `YMM5  | `YMM6  | `YMM7
    | `YMM8  | `YMM9  | `YMM10 | `YMM11
    | `YMM12 | `YMM13 | `YMM14 | `YMM15
    | `YMM16 | `YMM17 | `YMM18 | `YMM19
    | `YMM20 | `YMM21 | `YMM22 | `YMM23
    | `YMM24 | `YMM25 | `YMM26 | `YMM27
    | `YMM28 | `YMM29 | `YMM30 | `YMM31
  ] [@@deriving sexp]

  type st = [
    | `ST0 | `ST1 | `ST2 | `ST3
    | `ST4 | `ST5 | `ST6 | `ST7
  ] [@@deriving sexp]

  type ip = [
    | `IP
    | `EIP
    | `RIP
  ] [@@deriving sexp]

  type segment = [
    | `CS
    | `DS
    | `ES
    | `FS
    | `GS
    | `SS
  ] [@@deriving sexp]

  type segment_base = [
    | `FS_BASE
    | `GS_BASE
  ] [@@deriving sexp]

  type gpr = [
    | r8
    | r16
    | r32
    | r64
    | r128
    | r256
  ] [@@deriving sexp]

  type reg_t = [
    | gpr
    | ip
    | st
    | segment
    | segment_base
  ] [@@deriving sexp]

  type spec = [`Nil | reg_t] [@@deriving sexp]

  let decode reg =
    let open Bap.Std in
    match Reg.name reg |> Sexp.of_string |> spec_of_sexp with
    | `Nil -> None
    | #reg_t as reg -> Some reg
    | exception exn -> failwithf "unknown register %s" (Reg.name reg) ()

  let var_of_reg_t mode = function
    | `EAX | `RAX -> Register.gax mode
    | `EBX | `RBX -> Register.gbx mode
    | `ECX | `RCX -> Register.gcx mode
    | `EDX | `RDX -> Register.gdx mode
    | `EBP | `RBP -> Register.gbp mode
    | `ESP | `RSP -> Register.gsp mode
    | `EDI | `RDI -> Register.gdi mode
    | `ESI | `RSI -> Register.gsi mode
    | `EIP | `RIP -> Register.gip mode
    (* XXX: Is r9d -> r* valid?
     * Is XMM* -> ymms.* valid?
    *)
    | `R8 | `R8D -> Register.r8
    | `R9 | `R9D -> Register.r9
    | `R10 | `R10D -> Register.r10
    | `R11 | `R11D -> Register.r11
    | `R12 | `R12D -> Register.r12
    | `R13 | `R13D -> Register.r13
    | `R14 | `R14D -> Register.r14
    | `R15 | `R15D -> Register.r15
    | `XMM0 | `YMM0 -> (Register.ymms mode).(0)
    | `XMM1 | `YMM1 -> (Register.ymms mode).(1)
    | `XMM2 | `YMM2 -> (Register.ymms mode).(2)
    | `XMM3 | `YMM3 -> (Register.ymms mode).(3)
    | `XMM4 | `YMM4 -> (Register.ymms mode).(4)
    | `XMM5 | `YMM5 -> (Register.ymms mode).(5)
    | `XMM6 | `YMM6 -> (Register.ymms mode).(6)
    | `XMM7 | `YMM7 -> (Register.ymms mode).(7)
    | `XMM8 | `YMM8 -> (Register.ymms mode).(8)
    | `XMM9 | `YMM9 -> (Register.ymms mode).(9)
    | `XMM10| `YMM10 -> (Register.ymms mode).(10)
    | `XMM11| `YMM11 -> (Register.ymms mode).(11)
    | `XMM12| `YMM12 -> (Register.ymms mode).(12)
    | `XMM13| `YMM13 -> (Register.ymms mode).(13)
    | `XMM14| `YMM14 -> (Register.ymms mode).(14)
    | `XMM15| `YMM15 -> (Register.ymms mode).(15)
    | `XMM16| `YMM16-> (Register.ymms mode).(0)
    | `XMM17| `YMM17-> (Register.ymms mode).(1)
    | `XMM18| `YMM18-> (Register.ymms mode).(2)
    | `XMM19| `YMM19-> (Register.ymms mode).(3)
    | `XMM20| `YMM20-> (Register.ymms mode).(4)
    | `XMM21| `YMM21-> (Register.ymms mode).(5)
    | `XMM22| `YMM22-> (Register.ymms mode).(6)
    | `XMM23| `YMM23-> (Register.ymms mode).(7)
    | `XMM24| `YMM24-> (Register.ymms mode).(8)
    | `XMM25| `YMM25-> (Register.ymms mode).(9)
    | `XMM26| `YMM26 -> (Register.ymms mode).(10)
    | `XMM27| `YMM27 -> (Register.ymms mode).(11)
    | `XMM28| `YMM28 -> (Register.ymms mode).(12)
    | `XMM29| `YMM29 -> (Register.ymms mode).(13)
    | `XMM30| `YMM30 -> (Register.ymms mode).(14)
    | `XMM31| `YMM31 -> (Register.ymms mode).(15)

    | `ST0 -> st.(0)
    | `ST1 -> st.(1)
    | `ST2 -> st.(2)
    | `ST3 -> st.(3)
    | `ST4 -> st.(4)
    | `ST5 -> st.(5)
    | `ST6 -> st.(6)
    | `ST7 -> st.(7)

    | reg -> failwithf "no var_of_reg_t translation yet %s"
               (Sexp.to_string @@ sexp_of_reg_t reg) ()

  let size_of_reg_t = function
    | #r8 -> 8
    | #r16 | `IP -> 16
    | #r32 | `EIP -> 32
    | #r64 | `RIP -> 64
    | #st -> 80
    | #r128 -> 128
    | #r256 -> 256
    | #segment -> 16
    | #segment_base -> 64

  let st_index_of_reg_t = function
    | `ST0 -> 0
    | `ST1 -> 1
    | `ST2 -> 2
    | `ST3 -> 3
    | `ST4 -> 4
    | `ST5 -> 5
    | `ST6 -> 6
    | `ST7 -> 7
    | reg -> failwithf "Input is not an ST register. %s"
               (Sexp.to_string @@ sexp_of_reg_t reg) ()

  let exp_of_reg_t arch reg =
    let var = var_of_reg_t arch reg in
    let exp = Var var in
    let reg_size = size_of_reg_t reg in
    match Var.typ var with
    | Reg n when n > reg_size -> extract (reg_size - 1) 0 exp
    | _ -> exp


  let get_type = Typecheck.infer_ast

  let mem_index ~arch ~typ ~seg ~base ~scale ~index ~disp =
    let open Bap.Std in
    let _seg = decode seg in
    let _scale = Imm.to_int scale in
    let index_typ = match arch with
      | `x86 -> reg_32
      | `x86_64 -> reg_64
    in
    let disp = Imm.to_int disp |> Option.map ~f:(fun i -> it i index_typ) in
    let base = decode base |> Option.map ~f:(exp_of_reg_t arch) in
    let index = decode index |> Option.map ~f:(exp_of_reg_t arch) in

    let seg = None
    and scale = None in

    (* seg + (base + scale * index + disp) *)
    let ( + ) op1 op2 = match op1, op2 with
      | Some op1, Some op2 -> op1 +* op2 |> Option.some
      | Some _ as op, None -> op
      | None, (Some _ as op) -> op
      | None, None -> None
    in

    let ( * ) op1 op2 = match op1, op2 with
      | Some op1, Some op2 -> op2 <<* op1 |> Option.some
      | None, (Some _ as op) -> op
      | _, None -> None
    in

    match seg + (base + scale * index + disp) with
    | None -> failwith "unsupported memory expression"
    | Some exp -> exp

  type fp_inst = [
    | `LD_F0
    | `LD_F1
    | `LD_Frr
    | `LD_F32m
    | `LD_F64m
    | `LD_F80m
    | `ILD_F16m
    | `ILD_F32m (* FILD: Load Integer *)
    | `ILD_F64m

    | `ST_FPrr
    | `ST_FP32m
    | `ST_F32m
    | `ST_FP64m
    | `ST_F64m
    | `ST_FP80m
    | `IST_FP16m (* FISTP *)
    | `IST_FP32m
    | `IST_FP64m

    | `ABS_F
    | `SQRT_F
    | `SQRTSDr
    | `CHS_F

    | `CVTSI2SDrr
    | `CVTSI2SDrm
    | `CVTSI2SSrr
    | `CVTSI2SS64rr
    | `CVTSI2SSrm
    | `CVTSS2SDrr
    | `CVTSS2SDrm
    | `CVTSI2SD64rr
    | `CVTTSD2SIrm
    | `CVTSD2SSrr

    | `ADDSDrr
    | `ADDSDrm | `ADDSDrm_Int
    | `ADDSSrr
    | `ADDSSrm
    | `ADD_FrST0
    | `ADD_FPrST0
    | `ADD_F32m
    | `ADD_F64m
    | `ADD_FST0r

    | `SUBSDrr
    | `SUBSDrm
    | `SUBSSrr
    | `SUBSSrm
    | `SUB_FrST0
    | `SUB_FPrST0
    | `SUBR_FPrST0
    | `SUB_F32m
    | `SUB_F64m

    | `MULSDrr
    | `MULSDrm
    | `MULSSrr
    | `MULSSrm
    | `MUL_FST0r
    | `MUL_FrST0
    | `MUL_FPrST0
    | `MUL_F32m
    | `MUL_F64m

    | `DIVSDrr | `DIVSDrr_Int
    | `DIVSDrm
    | `DIVSSrr
    | `DIVSSrm
    | `DIV_FST0r
    | `DIV_FrST0
    | `DIV_FPrST0
    | `DIVR_FPrST0
    | `DIV_F32m
    | `DIV_F64m
    | `DIV_FI32m
    | `DIV_FI16m

    | `MAXSDrr

    | `UCOMISDrr
    | `UCOMISDrm
    | `UCOMISSrr
    | `UCOMISSrm
    | `UCOM_FIr
    | `UCOM_FIPr
    | `UCOM_Fr
    | `UCOM_FPr
    | `FCOMr
    | `FCOMPr
    | `FCOM32m
    | `FCOM64m
    | `FCOMP32m
    | `FCOMP64m
    | `UNPCKLPSrr

    | `XCH_F

    | `WAIT

    (* YMM instructions under fp_inst for now. *)
    | `VZEROUPPER
    | `VPBROADCASTBrr
    | `VPBROADCASTBYrr
    | `VINSERTI128rr
    | `PACKUSWBrr

    (* other instructions currently unsupported *)
    | `STD
    | `FNSTCW16m
    | `FLDCW16m
    | `FNSTSW16m
    | `FNSTSW16r
    | `FXAM
    | `FRNDINT
  ] [@@deriving bin_io, sexp, compare, enumerate]

  let sse_bv_to_fp = ieeebvtof
  let fp_to_sse_bv = ftoieeebv
  let convert_int64_to_single_precision_fp integer =
    bvtosf ~float_size:fp_64_bits integer
    |> ftof ~float_size:fp_32_bits

  (* TODO: should these all be the same call? We're passing in a certain
     integer size not a certain float size, it's a little bit of code smell
     here. *)
  let convert_int16_to_double_precision_fp integer =
    bvtosf ~float_size:fp_16_bits integer
  let convert_int32_to_double_precision_fp integer =
    bvtosf ~float_size:fp_32_bits integer
  let convert_int64_to_double_precision_fp integer =
    bvtosf ~float_size:fp_64_bits integer

  let convert_single_precision_fp_to_double_precision_fp fp =
    ftof ~float_size:fp_64_bits fp
  let convert_double_precision_fp_to_single_precision_fp fp =
    ftof ~float_size:fp_32_bits fp
  let convert_double_precision_fp_to_int32 fp =
    ftosbv ~bv_size:32 fp
  let x87_bv_to_fp_pc = ToIR.x87_bv_to_fp_pc

  let load_fp ~index = ToIR.get_x87_stack ~index |> x87_bv_to_fp_pc

  let lift arch mem insn =

    (* FIXME: we need a way to go from int to float *)
    let ld_f0 () =
      let zero = bvtosf ~float_size:fp_80_bits
          (Ast.Int ((Big_int_Z.big_int_of_int 0), Type.reg_64))
      in
      [ToIR.dec_x87_stack] @ ToIR.set_x87_stack ~index:0 zero
    in

    let ld_f1 () =
      let one = bvtosf ~float_size:fp_80_bits
          (Ast.Int ((Big_int_Z.big_int_of_int 1), Type.reg_64))
      in
      [ToIR.dec_x87_stack] @ ToIR.set_x87_stack ~index:0 one
    in

    let ld_fm mem ~typ ~seg ~base ~scale ~index ~disp =
      let exp_typ = match typ with
        | `reg_16 -> reg_16
        | `reg_32 -> reg_32
        | `reg_64 -> reg_64
        | `reg_80 -> Type.Reg 80
      in
      let mem_index = mem_index ~typ:exp_typ ~arch ~seg ~base ~scale ~index ~disp in
      let mem = load_s arch None exp_typ mem_index in
      let value = ToIR.bv_to_fp80 (typ :> ToIR.convertable_bv) mem in
      let stmts =
        [ToIR.dec_x87_stack;]
        @ ToIR.set_x87_stack ~index:0 value
      in
      Core_kernel.Result.Ok stmts
    in

    let ild_fm mem ~typ ~seg ~base ~scale ~index ~disp =
      let exp_typ = match typ with
        | `reg_16 -> reg_16
        | `reg_32 -> reg_32
        | `reg_64 -> reg_64
      in
      let mem_index = mem_index ~typ:exp_typ ~arch ~seg ~base ~scale ~index ~disp in
      let mem : Ast.exp = load_s arch None exp_typ mem_index in
      let value : Ast.exp = match typ with
        | `reg_16 -> convert_int16_to_double_precision_fp mem
        | `reg_32 -> convert_int32_to_double_precision_fp mem
        | `reg_64 -> convert_int64_to_double_precision_fp mem
      in
      let stmts =
        [ToIR.dec_x87_stack;]
        @ ToIR.set_x87_stack ~index:0 value
      in
      Core_kernel.Result.Ok stmts
    in

    let ld_fprr mem src =
      let index = decode src |> value_exn |> st_index_of_reg_t in
      load_fp ~index |> ToIR.push_x87_stack |> fun x -> Result.Ok x
    in

    (** FSTP: Copy ST(0) to ST(i) and pop register stack. *)
    let st_fprr mem dst =
      let dst_index = decode dst |> value_exn |> st_index_of_reg_t in
      let src = load_fp ~index:0 in
      let stmts = ToIR.set_x87_stack ~index:dst_index src @ ToIR.pop_x87_stack in
      Result.Ok stmts
    in

    let st_fm mem ~typ ~pop ~seg ~base ~scale ~index ~disp =
      let exp_typ = match typ with
        | `reg_16 -> reg_16
        | `reg_32 -> reg_32
        | `reg_64 -> reg_64
        | `reg_80 -> Type.Reg 80
      in
      let mem_index = mem_index ~typ:exp_typ ~arch ~seg ~base ~scale ~index ~disp in
      let st0 = load_fp ~index:0 in
      let bv = ToIR.fp80_to_bv (typ :> ToIR.convertable_bv) st0 in
      let stmts = [
        ToIR.store_s arch None exp_typ mem_index bv;
      ] @
        (if pop then ToIR.pop_x87_stack else [])
      in
      Core_kernel.Result.Ok stmts
    in

    let ist_fm mem ~typ ~pop ~seg ~base ~scale ~index ~disp =
      let exp_typ = match typ with
        | `reg_16 -> reg_16
        | `reg_32 -> reg_32
        | `reg_64 -> reg_64
      in
      let mem_index = mem_index ~typ:exp_typ ~arch ~seg ~base ~scale ~index ~disp in
      let st0 = load_fp ~index:0 in
      (* FIXME: convert float bv to signed integer *)
      let bv = ToIR.fp80_to_bv (typ :> ToIR.convertable_bv) st0 in
      let stmts = [
        ToIR.store_s arch None exp_typ mem_index bv;
      ] @
        (if pop then ToIR.pop_x87_stack else [])
      in
      Core_kernel.Result.Ok stmts
    in

    (* Converting integers to double precision FP *)
    let cvtsi2sd dst exp =
      let exp = cast CAST_UNSIGNED (Reg 64) exp in
      let double = convert_int64_to_double_precision_fp exp in
      let max_vl = Typecheck.bits_of_width (Var.typ dst) in
      let bv_size = 64 in
      let upper_part = extract (max_vl-1) bv_size (Var dst) in
      Core_kernel.Result.Ok [
        ToIR.move dst (upper_part ++* fp_to_sse_bv ~bv_size double)
      ]
    in
    let cvtsi2sd_rr ~typ arch mem dst src =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src = decode src |> value_exn |> exp_of_reg_t arch in
      assert (Poly.(=) typ @@ get_type src);
      cvtsi2sd dst src
    in
    let cvtsi2sd_rm ~typ arch mem dst ~seg ~base ~scale ~index ~disp  =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let mem_index = mem_index ~typ ~arch ~seg ~base ~scale ~index ~disp in
      let src = load_s arch None typ mem_index in
      cvtsi2sd dst src
    in

    (* Converting integers to single precision FP  *)
    let cvtsi2ss dst exp =
      let exp = cast CAST_UNSIGNED reg_64 exp in
      let double = convert_int64_to_single_precision_fp exp in
      let max_vl = Typecheck.bits_of_width (Var.typ dst) in
      let bv_size = 32 in
      let upper_part = extract (max_vl-1) bv_size (Var dst) in
      Core_kernel.Result.Ok [
        ToIR.move dst (upper_part ++* fp_to_sse_bv ~bv_size double)
      ]
    in
    let cvtsi2ss_rm ~typ arch mem dst ~seg ~base ~scale ~index ~disp =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let mem_index = mem_index ~typ ~arch ~seg ~base ~scale ~index ~disp in
      let src = load_s arch None typ mem_index in
      cvtsi2ss dst src
    in
    let cvtsi2ss_rr ~typ arch mem dst src =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src = decode src |> value_exn |> var_of_reg_t arch in
      cvtsi2ss dst (cast CAST_UNSIGNED typ (Var src))
    in


    (* Converting single precision FP to double precision FP *)
    let cvtss2sd dst exp =
      let double = convert_single_precision_fp_to_double_precision_fp exp in
      let max_vl = Typecheck.bits_of_width (Var.typ dst) in
      let bv_size = 64 in
      let upper_part = extract (max_vl-1) bv_size (Var dst) in
      Core_kernel.Result.Ok [
        ToIR.move dst (upper_part ++* fp_to_sse_bv ~bv_size double)
      ]
    in
    let cvtss2sd_rm ~typ arch mem dst ~seg ~base ~scale ~index ~disp =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let mem_index = mem_index ~typ ~arch ~seg ~base ~scale ~index ~disp in
      let src = load_s arch None typ mem_index in
      cvtss2sd dst (sse_bv_to_fp ~float_size:fp_32_bits src)
    in
    let cvtss2sd_rr ~typ arch mem dst src =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src = decode src |> value_exn |> var_of_reg_t arch in
      let src = extract 31 0 (Var src) in
      cvtss2sd dst (sse_bv_to_fp ~float_size:fp_32_bits src)
    in

    (* Converting double precision FP to signed integers *)
    let cvttsd2si dst exp =
      let integer = convert_double_precision_fp_to_int32 exp in
      Core_kernel.Result.Ok [
        ToIR.move dst integer
      ]
    in
    let cvttsd2si_rm arch mem dst ~seg ~base ~scale ~index ~disp =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let mem_index = mem_index ~typ:reg_64 ~arch ~seg ~base ~scale ~index ~disp in
      let src = load_s arch None reg_64 mem_index in
      cvttsd2si dst (sse_bv_to_fp ~float_size:fp_64_bits src)
    in

    (* Converting double precision FP to single precision *)
    let cvtsd2ss upper_part dst exp =
      let double = convert_double_precision_fp_to_single_precision_fp exp in
      Core_kernel.Result.Ok [
        ToIR.move dst (upper_part ++* fp_to_sse_bv ~bv_size:32 double)
      ]
    in
    let cvtsd2ss_rr arch mem dst src =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src = decode src |> value_exn |> var_of_reg_t arch in
      let max_vl = Typecheck.bits_of_width (Var.typ dst) in
      let upper_part = extract (max_vl-1) 32 (Var dst) in
      let src = extract 63 0 (Var src) in
      cvtsd2ss upper_part dst (sse_bv_to_fp ~float_size:fp_64_bits src)
    in


    let sse_unopsd_r ~unop arch mem dst src =
      let src = decode src |> value_exn |> exp_of_reg_t arch in
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let double =
        unop (sse_bv_to_fp ~float_size:fp_64_bits (extract 63 0 src))
        |> fp_to_sse_bv ~bv_size:64
      in
      let max_vl = Typecheck.bits_of_width (Var.typ dst) in
      let upper_part = extract (max_vl-1) 64 (Var dst) in
      let stmts =
        [ToIR.move dst (upper_part ++* double)]
      in
      Core_kernel.Result.Ok stmts
    in

    let sse_binopsd_rrr ~typ ~binop arch mem dst src1 src2 =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src1 = decode src1 |> value_exn |> exp_of_reg_t arch in
      let src2 = decode src2 |> value_exn |> exp_of_reg_t arch in
      let float_size, bv_size = match typ with
        | `reg_64 -> fp_64_bits, 64
        | `reg_32 -> fp_32_bits, 32
        | `reg_16 -> fp_16_bits, 16
      in
      let double =
        binop (sse_bv_to_fp ~float_size (extract (bv_size-1) 0 src1))
          (sse_bv_to_fp ~float_size (extract (bv_size-1) 0 src2))
        |> fp_to_sse_bv ~bv_size
      in
      let max_vl = Typecheck.bits_of_width (Var.typ dst) in
      let upper_part = extract (max_vl-1) bv_size (Var dst) in
      let stmts =
        [ToIR.move dst (upper_part ++* double)]
      in
      Core_kernel.Result.Ok stmts
    in

    let sse_binopsd_rrm ~typ ~binop arch mem dst src1
        ~seg ~base ~scale ~index ~disp =

      let float_size, typ, bv_size = match typ with
        | `reg_64 -> fp_64_bits, reg_64, 64
        | `reg_32 -> fp_32_bits, reg_32, 32
        | `reg_16 -> fp_16_bits, reg_16, 16
      in

      let mem_index = mem_index ~typ:reg_64 ~arch ~seg ~base ~scale ~index ~disp in
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src1 = decode src1 |> value_exn |> exp_of_reg_t arch in
      let src2 = load_s arch None typ mem_index in
      let double =
        binop (sse_bv_to_fp ~float_size (extract (bv_size-1) 0 src1))
          (sse_bv_to_fp ~float_size (extract (bv_size-1) 0 src2))
        |> fp_to_sse_bv ~bv_size
      in
      let max_vl = Typecheck.bits_of_width (Var.typ dst) in
      let upper_part = extract (max_vl-1) bv_size (Var dst) in
      let stmts =
        [ToIR.move dst (upper_part ++* double)]
      in
      Core_kernel.Result.Ok stmts
    in

    let x87_binop style ~binop ?(reversed=false) mem sti =
      (* TODO: We don't explicitly handle edge cases here. Z3 may handle them
         but if we see incoherence in the binops this is a possible cause.
         See the manual for the intended behavior for +0/+inf, NaN+NaN, etc.
         This TODO affects the other binop handlers too. *)
      let sti_index = decode sti |> value_exn |> st_index_of_reg_t in
      let st0 = load_fp ~index:0 in
      let sti = load_fp ~index:sti_index in
      let stmts = match style with
        |    `FST0r -> ToIR.set_x87_stack ~index:0         (binop st0 sti)
        |    `FrST0 -> ToIR.set_x87_stack ~index:sti_index (binop sti st0)
        |   `FPrST0 -> ToIR.set_x87_stack ~index:sti_index (binop sti st0) @ ToIR.pop_x87_stack
        | `R_FPrST0 -> ToIR.set_x87_stack ~index:sti_index (binop st0 sti) @ ToIR.pop_x87_stack
      in Result.Ok stmts
    in

    let x87_binop_mem ~typ:atyp ~binop mem ~seg ~base ~scale ~index ~disp =
      (* x87 binops with a single memory arg use st0 as the destination *)
      let dst = load_fp ~index:0 in
      let float_size, typ = match atyp with
        | `reg_64 -> fp_64_bits, reg_64
        | `reg_32 -> fp_32_bits, reg_32
        | `reg_16 -> fp_16_bits, reg_16
      in
      let mem_index = mem_index ~typ ~arch ~seg ~base ~scale ~index ~disp in
      let src = load_s arch None typ mem_index in
      let src = ToIR.bv_to_fp80 (atyp :> ToIR.convertable_bv) src in
      let value = binop dst src in
      let stmts =
        ToIR.set_x87_stack ~index:0 value
      in
      Result.Ok stmts
    in

    let ucomisx_rr ~typ arch mem src1 src2 =
      let src1 = decode src1 |> value_exn |> exp_of_reg_t arch in
      let src2 = decode src2 |> value_exn |> exp_of_reg_t arch in
      let float_size, highbit = match typ with
        | `reg_16 -> fp_16_bits, 15
        | `reg_32 -> fp_32_bits, 31
        | `reg_64 -> fp_64_bits, 63
      in
      let src1 = sse_bv_to_fp ~float_size (extract highbit 0 src1) in
      let src2 = (sse_bv_to_fp ~float_size (extract highbit 0 src2)) in
      let double binop = binop src1 src2 in
      let isnan = Var_temp.nt "isnan" reg_1 in
      let stmts =
        [
          ToIR.move isnan @@ exp_or (fisnan src1) (fisnan src2);
          ToIR.move zf @@ exp_or (double feq) (Var isnan);
          ToIR.move pf (Var isnan);
          ToIR.move cf @@ exp_or (double flt) (Var isnan);
          ToIR.move oF exp_false;
          ToIR.move af exp_false;
          ToIR.move sf exp_false;
        ]
      in
      Core_kernel.Result.Ok stmts
    in
    let ucomisx_rm ~typ arch mem src1 ~seg ~base ~scale ~index ~disp =
      let src1 = decode src1 |> value_exn |> exp_of_reg_t arch in
      let float_size, highbit, exp_typ = match typ with
        | `reg_16 -> fp_16_bits, 15, reg_16
        | `reg_32 -> fp_32_bits, 31, reg_32
        | `reg_64 -> fp_64_bits, 63, reg_64
      in
      let mem_index = mem_index ~typ:exp_typ ~arch ~seg ~base ~scale ~index ~disp in
      let src2 = load_s arch None exp_typ mem_index in
      let src1 = sse_bv_to_fp ~float_size (extract highbit 0 src1) in
      let src2 = (sse_bv_to_fp ~float_size (extract highbit 0 src2)) in
      let double binop = binop src1 src2 in
      let isnan = Var_temp.nt "isnan" reg_1 in
      let stmts =
        [
          ToIR.move isnan @@ exp_or (fisnan src1) (fisnan src2);
          ToIR.move zf @@ exp_or (double feq) (Var isnan);
          ToIR.move pf (Var isnan);
          ToIR.move cf @@ exp_or (double flt) (Var isnan);
          ToIR.move oF exp_false;
          ToIR.move af exp_false;
          ToIR.move sf exp_false;
        ]
      in
      Core_kernel.Result.Ok stmts
    in

    let ucomfi_r ~pop mem sti =
      let sti_index = decode sti |> value_exn |> st_index_of_reg_t in
      let sti = load_fp ~index:sti_index in
      let st0 = load_fp ~index:0 in
      (* FIXME: handle QNaN vs. SNaN and check for invalid operands:
       * https://en.wikipedia.org/wiki/Extended_precision#x86_extended_precision_format
      *)
      let i0 = Int(Big_int_convenience.bi0, reg_1) in
      let i1 = Int(Big_int_convenience.bi1, reg_1) in
      let has_nan = binop OR (fisnan st0) (fisnan sti) in
      let stmts = [
        ToIR.move x87_c1 i0;
        ToIR.move zf @@ exp_ite has_nan i1 (feq st0 sti);
        ToIR.move pf @@ exp_ite has_nan i1 i0;
        ToIR.move cf @@ exp_ite has_nan i1 (flt st0 sti);
      ] @ (if pop then ToIR.pop_x87_stack else [])
      in
      Core_kernel.Result.Ok stmts
    in

    let fcom_r ~pop ?(pop2=false) ~unordered mem sti =
      let sti_index = decode sti |> value_exn |> st_index_of_reg_t in
      let sti = load_fp ~index:sti_index in
      let st0 = load_fp ~index:0 in
      (* FIXME: SNaN, QNaN stuff isn't handled;
       * the difference between fcom and fucom is that fcom signals
       * for any kind of NaNs, and fucom signals only for SNaNs. *)
      let i0 = Int(Big_int_convenience.bi0, reg_1) in
      let i1 = Int(Big_int_convenience.bi1, reg_1) in
      let has_nan = binop OR (fisnan st0) (fisnan sti) in
      let has_empty = binop OR (ToIR.get_x87_tag ~index:0 ==* it 3 (Reg 2)) (ToIR.get_x87_tag ~index:sti_index ==* it 3 (Reg 2)) in
      let stmts = [
        ToIR.move x87_c1 @@ exp_ite has_empty i1 i0;
        ToIR.move x87_c3 @@ exp_ite has_nan i1 (feq st0 sti);
        ToIR.move x87_c2 @@ exp_ite has_nan i1 i0;
        ToIR.move x87_c0 @@ exp_ite has_nan i1 (flt st0 sti);
      ] @ (match pop, pop2 with
          | false, false -> []
          | true, false -> ToIR.pop_x87_stack
          | true, true -> ToIR.pop_x87_stack @ ToIR.pop_x87_stack
          | false, true -> assert false
        )
      in
      Core_kernel.Result.Ok stmts
    in

    let fcom_m mem ~typ ~pop ~seg ~base ~scale ~index ~disp =
      let float_size, exp_typ = match typ with
        | `reg_64 -> fp_64_bits, reg_64
        | `reg_32 -> fp_32_bits, reg_32
        | `reg_16 -> fp_16_bits, reg_16
      in
      let mem_index = mem_index ~typ:exp_typ ~arch ~seg ~base ~scale ~index ~disp in
      let op = load_s arch None exp_typ mem_index in
      let op = ToIR.bv_to_fp80 (typ :> ToIR.convertable_bv) op in

      let st0 = load_fp ~index:0 in
      let i0 = Int(Big_int_convenience.bi0, reg_1) in
      let i1 = Int(Big_int_convenience.bi1, reg_1) in
      let has_nan = binop OR (fisnan st0) (fisnan op) in
      let stmts = [
        ToIR.move x87_c1 @@ i0;
        ToIR.move x87_c0 @@ exp_ite has_nan i1 (flt st0 op);
        ToIR.move x87_c2 @@ exp_ite has_nan i1 i0;
        ToIR.move x87_c3 @@ exp_ite has_nan i1 (feq st0 op);
      ] @ (if pop then ToIR.pop_x87_stack else [])
      in
      Core_kernel.Result.Ok stmts
    in

    let fidiv mem ~typ ~seg ~base ~scale ~index ~disp =
      let exp_typ = match typ with
        | `reg_32 -> reg_32
        | `reg_16 -> reg_16
      in
      let mem_index = mem_index ~typ:exp_typ ~arch ~seg ~base ~scale ~index ~disp in
      let mem : Ast.exp = load_s arch None exp_typ mem_index in
      let value : Ast.exp = match typ with
        | `reg_16 -> bvtosf ~float_size:fp_80_bits mem
        | `reg_32 -> bvtosf ~float_size:fp_80_bits mem
      in
      let st0 = load_fp ~index:0 in
      let stmts = ToIR.set_x87_stack ~index:0 (fdiv st0 value) in
      Result.Ok stmts
    in

    (** Unpack and Interleave Low Packed Single-Precision Floating-Point Values *)
    let unpcklps_rr arch mem dest src1 src2 =
      let src1 = decode src1 |> value_exn |> exp_of_reg_t arch in
      let src2 = decode src2 |> value_exn |> exp_of_reg_t arch in
      let dest = decode dest |> value_exn |> var_of_reg_t arch in

      let x0 = extract 31 0  src1 in
      let y0 = extract 31 0  src2 in
      let x1 = extract 63 32 src1 in
      let y1 = extract 63 32 src2 in
      let stmts =
        [ ToIR.move dest (y1 ++* x1 ++* y0 ++* x0) ]
      in
      Core_kernel.Result.Ok stmts
    in

    (** FXCH ST(i): Exchange the contents of ST(0) and ST(i). *)
    let xch_f mem sti =
      let i = decode sti |> value_exn |> st_index_of_reg_t in
      let st0 = ToIR.get_x87_stack ~index:0 in
      let sti = load_fp ~index:i in
      let temp = Var_temp.nt "xchvar" (Reg 80) in
      let stmts =
        [ToIR.move x87_c1 @@ Int(Big_int_convenience.bi0, reg_1);
         ToIR.move temp st0] @
        ToIR.set_x87_stack ~index:0 sti @
        ToIR.set_x87_stack ~index:i (Var temp |> ToIR.x87_bv_to_fp_pc)
      in
      Result.Ok stmts
    in

    let abs_f () =
      (* Unsoundness note: fabs operates only on the physical sign bit,
       * which may change the sign of a NaN; but we only support one NaN. *)
      let st0 = load_fp ~index:0 in
      ToIR.set_x87_stack ~index:0 (fabs st0)
    in

    let sqrt_f () =
      let st0 = load_fp ~index:0 in
      ToIR.set_x87_stack ~index:0 (fsqrt st0)
    in

    let chs_f () =
      (* Unsoundness note: fchs always flips the physical sign bit,
       * actually changing the sign of NaNs; but we only support one NaN. *)
      let st0 = load_fp ~index:0 in
      ToIR.set_x87_stack ~index:0 (fneg st0)
    in

    (* Keep only lower 128 bits. *)
    let vzeroupper arch =
      let f reg =
        let open Big_int_convenience in
        let mask128 = Int (bimask128, Reg 128) in
        let cast_t = Reg (Typecheck.bits_of_width (Var.typ reg)) in
        ToIR.move reg @@ exp_and (Var reg) (Cast (Type.CAST_UNSIGNED, cast_t, mask128))
      in
      List.map ~f (Register.ymm_list arch)
    in

    let vpbroadcast_rr dest_width src_width arch mem dst src =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src = decode src |> value_exn |> exp_of_reg_t arch in
      let max_vl = Typecheck.bits_of_width (Var.typ dst) in
      let src_size = match src_width with `B -> 8 | `W -> 16 | `D -> 32 | `Q -> 64 in
      let dst_size = match dest_width with `YMM -> 256 | `XMM -> 128 | `ZMM -> 512 in

      let src_exp = cast CAST_LOW (Reg src_size) src in
      let acc = ref src_exp in
      for _ = 2 to (dst_size / src_size) do
        acc := !acc ++* src_exp
      done;
      let acc =
        if max_vl = dst_size then !acc
        else Int(Big_int_convenience.bi0, Reg (max_vl - dst_size)) ++* !acc
      in
      Core_kernel.Result.Ok [ ToIR.move dst acc ]
    in

    let vinserti128_rrri arch mem dst src1 src2 imm =
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src1 = decode src1 |> value_exn |> exp_of_reg_t arch in
      let src2 = decode src2 |> value_exn |> exp_of_reg_t arch in
      let imm = Bap.Std.Imm.to_int imm |> value_exn in
      let src1_low = extract 127 0 src1 in
      let src1_high = extract 255 128 src1 in
      let packed = extract 127 0 src2 in
      let keep_src1_low = (Int.bit_and imm 1) = 1 in
      let final_value =
        if keep_src1_low then
          concat packed src1_low
        else
          concat src1_high packed
      in
      let stmts =
        [
          ToIR.move dst final_value
        ]
      in
      Core_kernel.Result.Ok stmts
    in

    (* Pack with Unsigned Saturation *)
    let packuswb_rr arch mem dst src1 src2 =
      let dst_size = decode dst |> value_exn |> size_of_reg_t in
      let dst = decode dst |> value_exn |> var_of_reg_t arch in
      let src1 = decode src1 |> value_exn |> var_of_reg_t arch in
      let src2 = decode src2 |> value_exn |> var_of_reg_t arch in
      (* Cast 16-bit signed to 8-bit unsigned, saturating on boundaries. *)
      let is_saturated_negative exp =
        binop SLT exp (Int(Big_int_Z.zero_big_int, reg_16))
      in
      let is_saturated_positive exp =
        binop LT (Int(Big_int_Z.big_int_of_int 0xFF, reg_16)) exp
      in
      let min_byte = Int(Big_int_Z.zero_big_int, reg_8) in
      let max_byte = Int(Big_int_Z.big_int_of_int 0xFF, reg_8) in
      let saturate_signed_word_to_unsigned_byte exp =
        exp_ite (is_saturated_negative exp) min_byte (
          exp_ite (is_saturated_positive exp) max_byte (
            extract 7 0 exp
          )
        )
      in
      (* Two cases: 64-bit operands and 128-bit operands. *)
      let byte_of_idx idx =
        let extract_src, idx =
          (* First half of bits are take from src1. *)
          if idx < (dst_size / 2) then src1, idx
          (* The second half are taken from src2. *)
          else src2, (idx - (dst_size / 2))
        in
        extract (idx+15) idx (Var extract_src)
        |> saturate_signed_word_to_unsigned_byte
      in
      let real_reg_size = Typecheck.bits_of_width (Var.typ dst) in
      (* If the real register we're writing to is larger, make sure to
       * preserve its original upper bits when writing to it.
      *)
      let upper_bytes =
        if (dst_size / 2) < real_reg_size then
          Sequence.singleton (extract (real_reg_size-1) (dst_size / 2) (Var src1))
        else
          Sequence.empty
      in
      let packed_bytes =
        Sequence.range ~stride:16 ~start:`inclusive ~stop:`exclusive 0 dst_size
        |> Sequence.map ~f:byte_of_idx
      in
      let bytes = Sequence.append packed_bytes upper_bytes in
      let low_byte, remaining_bytes = Sequence.next bytes |> value_exn in
      let final_value =
        Sequence.fold ~init:low_byte ~f:(fun acc byte -> concat byte acc)
          remaining_bytes
      in
      Result.Ok [ToIR.move dst final_value]
    in

    let std () =
      [ToIR.move df exp_true]
    in

    let fnstcw mem ~seg ~base ~scale ~index ~disp  =
      let mem_index = mem_index ~typ:reg_16 ~arch ~seg ~base ~scale ~index ~disp in
      let control_word =
        concat (Var x87_15) @@
        concat (Var x87_14) @@
        concat (Var x87_13) @@
        concat (Var x87_x)  @@
        concat (Var x87_rc) @@
        concat (Var x87_pc) @@
        concat (Var x87_7)  @@
        concat (Var x87_6)  @@
        concat (Var x87_pm) @@
        concat (Var x87_um) @@
        concat (Var x87_om) @@
        concat (Var x87_zm) @@
        concat (Var x87_dm) @@
        (Var x87_im)
      in
      Result.Ok [ToIR.store_s arch None reg_16 mem_index control_word]
    in

    let fldcw mem ~seg ~base ~scale ~index ~disp =
      let mem_index = mem_index ~typ:reg_16 ~arch ~seg ~base ~scale ~index ~disp in
      let mem = load_s arch None reg_16 mem_index in
      Result.Ok [
        ToIR.move x87_im @@ extract 0 0 mem;
        ToIR.move x87_dm @@ extract 1 1 mem;
        ToIR.move x87_zm @@ extract 2 2 mem;
        ToIR.move x87_om @@ extract 3 3 mem;
        ToIR.move x87_um @@ extract 4 4 mem;
        ToIR.move x87_pm @@ extract 5 5 mem;
        ToIR.move x87_6 @@ extract 6 6 mem;
        ToIR.move x87_7 @@ extract 7 7 mem;
        ToIR.move x87_pc @@ extract 9 8 mem;
        ToIR.move x87_rc @@ extract 11 10 mem;
        ToIR.move x87_x @@ extract 12 12 mem;
        ToIR.move x87_13 @@ extract 13 13 mem;
        ToIR.move x87_14 @@ extract 14 14 mem;
        ToIR.move x87_15 @@ extract 15 15 mem;
      ]
    in

    let fnstsw_m mem ~seg ~base ~scale ~index ~disp  =
      let mem_index = mem_index ~typ:reg_16 ~arch ~seg ~base ~scale ~index ~disp in
      Result.Ok [ToIR.store_s arch None reg_16 mem_index x87_status_word]
    in

    let fnstsw_ax arch =
      let gax = Register.gax arch in
      let gax_high = match arch with
        | `x86 -> 31
        | `x86_64 -> 63 in (* was Target.Arch.addr_width - 1 *)
      [ToIR.move gax @@ concat (extract gax_high 16 (Var gax)) x87_status_word]
    in

    let fxam () =
      let st0 = load_fp ~index:0 in
      let switch x ~def ~nan ~norm ~inf ~zero ~empty ~denorm =
        exp_ite (ToIR.get_x87_tag ~index:0 ==* it 3 (Reg 2)) empty @@
        exp_ite (fisnan x) nan @@
        exp_ite (fisnorm x) norm @@
        exp_ite (fisinf x) inf @@
        exp_ite (fiszero x) zero @@
        exp_ite (fissub x) denorm @@
        def
      in
      let _0 = it 0 (Reg 1) in
      let _1 = it 1 (Reg 1) in
      [
        ToIR.move x87_c0 @@ switch st0 ~def:_0 ~nan:_1 ~norm:_0 ~inf:_1 ~zero:_0 ~empty:_1 ~denorm:_0;
        ToIR.move x87_c1 @@ fisneg st0;
        ToIR.move x87_c2 @@ switch st0 ~def:_0 ~nan:_0 ~norm:_1 ~inf:_1 ~zero:_0 ~empty:_0 ~denorm:_1;
        ToIR.move x87_c3 @@ switch st0 ~def:_0 ~nan:_0 ~norm:_0 ~inf:_0 ~zero:_1 ~empty:_1 ~denorm:_1;
      ]
    in

    let frndint () =
      ToIR.set_x87_stack ~index:0 @@ fround @@ load_fp ~index:0
    in

    let decode_name insn =
      let inst_name = MC.Insn.name insn in
      try (Sexp.of_string inst_name |> fp_inst_of_sexp : [< fp_inst])
      with exn -> raise (Unsupported_opcode inst_name) in
    match decode_name insn with
    | `LD_F0 ->
      ld_f0 ()
    | `LD_F1 ->
      ld_f1 ()
    | `LD_Frr ->
      Operands.r ~f:ld_fprr mem insn |> ok_exn
    | `LD_F32m ->
      Operands.m ~f:(ld_fm ~typ:`reg_32) mem insn |> ok_exn
    | `LD_F64m ->
      Operands.m ~f:(ld_fm ~typ:`reg_64) mem insn |> ok_exn
    | `LD_F80m ->
      Operands.m ~f:(ld_fm ~typ:`reg_80) mem insn |> ok_exn
    | `ILD_F16m ->
      Operands.m ~f:(ild_fm ~typ:`reg_16) mem insn |> ok_exn
    | `ILD_F32m ->
      Operands.m ~f:(ild_fm ~typ:`reg_32) mem insn |> ok_exn
    | `ILD_F64m ->
      Operands.m ~f:(ild_fm ~typ:`reg_64) mem insn |> ok_exn

    | `ABS_F ->
      abs_f ()
    | `SQRT_F ->
      sqrt_f ()
    | `SQRTSDr ->
      Operands.rr ~f:(sse_unopsd_r ~unop:fsqrt arch) mem insn |> ok_exn
    | `CHS_F ->
      chs_f ()

    | `ST_FPrr ->
      Operands.r ~f:st_fprr mem insn |> ok_exn
    | `ST_FP32m ->
      Operands.m ~f:(st_fm ~pop:true ~typ:`reg_32) mem insn |> ok_exn
    | `ST_F32m ->
      Operands.m ~f:(st_fm ~pop:false ~typ:`reg_32) mem insn |> ok_exn
    | `ST_FP64m ->
      Operands.m ~f:(st_fm ~pop:true ~typ:`reg_64) mem insn |> ok_exn
    | `ST_F64m ->
      Operands.m ~f:(st_fm ~pop:false ~typ:`reg_64) mem insn |> ok_exn
    | `ST_FP80m ->
      Operands.m ~f:(st_fm ~pop:true ~typ:`reg_80) mem insn |> ok_exn
    | `IST_FP16m ->
      Operands.m ~f:(ist_fm ~pop:true ~typ:`reg_16) mem insn |> ok_exn
    | `IST_FP32m ->
      Operands.m ~f:(ist_fm ~pop:true ~typ:`reg_32) mem insn |> ok_exn
    | `IST_FP64m ->
      Operands.m ~f:(ist_fm ~pop:true ~typ:`reg_64) mem insn |> ok_exn

    | `CVTSI2SDrr ->
      Operands.rr ~f:(cvtsi2sd_rr ~typ:reg_32 arch) mem insn |> ok_exn
    | `CVTSI2SDrm ->
      Operands.rm ~f:(cvtsi2sd_rm ~typ:reg_32 arch) mem insn |> ok_exn
    | `CVTSI2SD64rr ->
      Operands.rr ~f:(cvtsi2sd_rr ~typ:reg_64 arch) mem insn |> ok_exn
    | `CVTSI2SSrr ->
      Operands.rr ~f:(cvtsi2ss_rr ~typ:reg_32 arch) mem insn |> ok_exn
    | `CVTSI2SS64rr ->
      Operands.rr ~f:(cvtsi2ss_rr ~typ:reg_64 arch) mem insn |> ok_exn
    | `CVTSI2SSrm ->
      Operands.rm ~f:(cvtsi2ss_rm ~typ:reg_32 arch) mem insn |> ok_exn
    | `CVTSS2SDrr ->
      Operands.rr ~f:(cvtss2sd_rr ~typ:reg_32 arch) mem insn |> ok_exn
    | `CVTSS2SDrm ->
      Operands.rm ~f:(cvtss2sd_rm ~typ:reg_32 arch) mem insn |> ok_exn
    | `CVTTSD2SIrm ->
      Operands.rm ~f:(cvttsd2si_rm arch) mem insn |> ok_exn
    | `CVTSD2SSrr ->
      Operands.rr ~f:(cvtsd2ss_rr arch) mem insn |> ok_exn

    | `ADDSDrr ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_64 ~binop:fadd arch) mem insn |> ok_exn
    | `ADDSDrm | `ADDSDrm_Int ->
      Operands.rrm ~f:(sse_binopsd_rrm ~typ:`reg_64 ~binop:fadd arch) mem insn |> ok_exn
    | `ADDSSrr ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_32 ~binop:fadd arch) mem insn |> ok_exn
    | `ADDSSrm ->
      Operands.rrm ~f:(sse_binopsd_rrm ~typ:`reg_32 ~binop:fadd arch) mem insn |> ok_exn
    | `ADD_FrST0 ->
      Operands.r ~f:(x87_binop `FrST0 ~binop:fadd) mem insn |> ok_exn
    | `ADD_FST0r ->
      Operands.r ~f:(x87_binop `FST0r ~binop:fadd) mem insn |> ok_exn
    | `ADD_FPrST0 ->
      Operands.r ~f:(x87_binop `FPrST0 ~binop:fadd) mem insn |> ok_exn
    | `ADD_F32m ->
      Operands.m ~f:(x87_binop_mem ~typ:`reg_32 ~binop:fadd) mem insn |> ok_exn
    | `ADD_F64m ->
      Operands.m ~f:(x87_binop_mem ~typ:`reg_64 ~binop:fadd) mem insn |> ok_exn
    | `SUBSDrr ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_64 ~binop:fsub arch) mem insn |> ok_exn
    | `SUBSDrm ->
      Operands.rrm ~f:(sse_binopsd_rrm ~typ:`reg_64 ~binop:fsub arch) mem insn |> ok_exn
    | `SUBSSrr ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_32 ~binop:fsub arch) mem insn |> ok_exn
    | `SUBSSrm ->
      Operands.rrm ~f:(sse_binopsd_rrm ~typ:`reg_32 ~binop:fsub arch) mem insn |> ok_exn
    | `SUB_FrST0 ->
      Operands.r ~f:(x87_binop `FrST0 ~binop:fsub) mem insn |> ok_exn
    | `SUB_FPrST0 ->
      Operands.r ~f:(x87_binop `FPrST0 ~binop:fsub) mem insn |> ok_exn
    | `SUBR_FPrST0 ->
      Operands.r ~f:(x87_binop `R_FPrST0 ~binop:fsub) mem insn |> ok_exn
    | `SUB_F32m ->
      Operands.m ~f:(x87_binop_mem ~typ:`reg_32 ~binop:fsub) mem insn |> ok_exn
    | `SUB_F64m ->
      Operands.m ~f:(x87_binop_mem ~typ:`reg_64 ~binop:fsub) mem insn |> ok_exn

    | `MULSDrr ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_64 ~binop:fmul arch) mem insn |> ok_exn
    | `MULSDrm ->
      Operands.rrm ~f:(sse_binopsd_rrm ~typ:`reg_64 ~binop:fmul arch) mem insn |> ok_exn
    | `MULSSrr ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_32 ~binop:fmul arch) mem insn |> ok_exn
    | `MULSSrm ->
      Operands.rrm ~f:(sse_binopsd_rrm ~typ:`reg_32 ~binop:fmul arch) mem insn |> ok_exn
    | `MUL_FrST0 ->
      Operands.r ~f:(x87_binop `FrST0 ~binop:fmul) mem insn |> ok_exn
    | `MUL_FST0r ->
      Operands.r ~f:(x87_binop `FST0r ~binop:fmul) mem insn |> ok_exn
    | `MUL_FPrST0 ->
      Operands.r ~f:(x87_binop `FPrST0 ~binop:fmul) mem insn |> ok_exn
    | `MUL_F32m ->
      Operands.m ~f:(x87_binop_mem ~typ:`reg_32 ~binop:fmul) mem insn |> ok_exn
    | `MUL_F64m ->
      Operands.m ~f:(x87_binop_mem ~typ:`reg_64 ~binop:fmul) mem insn |> ok_exn

    | `DIVSDrr | `DIVSDrr_Int ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_64 ~binop:fdiv arch) mem insn |> ok_exn
    | `DIVSDrm ->
      Operands.rrm ~f:(sse_binopsd_rrm ~typ:`reg_64 ~binop:fdiv arch) mem insn |> ok_exn
    | `DIVSSrr ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_32 ~binop:fdiv arch) mem insn |> ok_exn
    | `DIVSSrm ->
      Operands.rrm ~f:(sse_binopsd_rrm ~typ:`reg_32 ~binop:fdiv arch) mem insn |> ok_exn
    | `DIV_FrST0 ->
      Operands.r ~f:(x87_binop `FrST0 ~binop:fdiv) mem insn |> ok_exn
    | `DIV_FPrST0 ->
      Operands.r ~f:(x87_binop `FPrST0 ~binop:fdiv) mem insn |> ok_exn
    | `DIV_FST0r ->
      Operands.r ~f:(x87_binop `FST0r ~binop:fdiv) mem insn |> ok_exn
    | `DIVR_FPrST0 ->
      Operands.r ~f:(x87_binop `R_FPrST0 ~binop:fdiv) mem insn |> ok_exn
    | `DIV_F32m ->
      Operands.m ~f:(x87_binop_mem ~typ:`reg_32 ~binop:fdiv) mem insn |> ok_exn
    | `DIV_F64m ->
      Operands.m ~f:(x87_binop_mem ~typ:`reg_64 ~binop:fdiv) mem insn |> ok_exn
    | `DIV_FI32m -> Operands.m ~f:(fidiv ~typ:`reg_32) mem insn |> ok_exn
    | `DIV_FI16m -> Operands.m ~f:(fidiv ~typ:`reg_16) mem insn |> ok_exn

    | `MAXSDrr ->
      Operands.rrr ~f:(sse_binopsd_rrr ~typ:`reg_64 ~binop:fmax arch) mem insn |> ok_exn

    | `UCOMISDrr ->
      Operands.rr ~f:(ucomisx_rr ~typ:`reg_64 arch) mem insn |> ok_exn
    | `UCOMISDrm ->
      Operands.rm ~f:(ucomisx_rm ~typ:`reg_64 arch) mem insn |> ok_exn
    | `UCOMISSrr ->
      Operands.rr ~f:(ucomisx_rr ~typ:`reg_32 arch) mem insn |> ok_exn
    | `UCOMISSrm ->
      Operands.rm ~f:(ucomisx_rm ~typ:`reg_32 arch) mem insn |> ok_exn
    | `UCOM_FIr ->
      Operands.r ~f:(ucomfi_r ~pop:false) mem insn |> ok_exn
    | `UCOM_FIPr ->
      Operands.r ~f:(ucomfi_r ~pop:true) mem insn |> ok_exn

    | `UCOM_Fr ->
      Operands.r ~f:(fcom_r ~pop:false ~unordered:true) mem insn |> ok_exn
    | `UCOM_FPr ->
      Operands.r ~f:(fcom_r ~pop:true ~unordered:true) mem insn |> ok_exn
    | `FCOMr ->
      Operands.r ~f:(fcom_r ~pop:false ~unordered:false) mem insn |> ok_exn
    | `FCOMPr ->
      Operands.r ~f:(fcom_r ~pop:true ~unordered:false) mem insn |> ok_exn

    | `FCOM32m ->
      Operands.m ~f:(fcom_m ~pop:false ~typ:`reg_32) mem insn |> ok_exn
    | `FCOM64m ->
      Operands.m ~f:(fcom_m ~pop:false ~typ:`reg_64) mem insn |> ok_exn
    | `FCOMP32m ->
      Operands.m ~f:(fcom_m ~pop:true ~typ:`reg_32) mem insn |> ok_exn
    | `FCOMP64m ->
      Operands.m ~f:(fcom_m ~pop:true ~typ:`reg_64) mem insn |> ok_exn
    | `UNPCKLPSrr ->
      Operands.rrr ~f:(unpcklps_rr arch) mem insn |> ok_exn

    | `XCH_F ->
      Operands.r ~f:xch_f mem insn |> ok_exn

    (* FWAIT: NOP *)
    | `WAIT -> []

    (* YMMs *)
    | `VZEROUPPER ->
      vzeroupper arch
    | `VPBROADCASTBYrr ->
      Operands.rr ~f:(vpbroadcast_rr `YMM `B arch) mem insn |> ok_exn
    | `VPBROADCASTBrr ->
      Operands.rr ~f:(vpbroadcast_rr `XMM `B arch) mem insn |> ok_exn
    | `VINSERTI128rr ->
      Operands.rrri ~f:(vinserti128_rrri arch) mem insn |> ok_exn
    | `PACKUSWBrr ->
      Operands.rrr ~f:(packuswb_rr arch) mem insn |> ok_exn

    (* Others *)
    | `STD ->
      std ()
    | `FNSTCW16m ->
      Operands.m ~f:fnstcw mem insn |> ok_exn
    | `FLDCW16m ->
      Operands.m ~f:fldcw mem insn |> ok_exn
    | `FNSTSW16m ->
      Operands.m ~f:fnstsw_m mem insn |> ok_exn
    | `FNSTSW16r ->
      fnstsw_ax arch
    | `FXAM ->
      fxam ()
    | `FRNDINT ->
      frndint ()

end

let run arch mem insn =
  try Ok (Fp_lifter.lift arch mem insn) with
  | Typecheck.TypeError msg  ->
    Or_error.errorf "ill-typed code: %s" msg
  | Unsupported_opcode name ->
    Or_error.errorf "unsupported opcode: %s" name
