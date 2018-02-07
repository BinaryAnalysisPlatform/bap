open Core_kernel.Std
open Bap.Std
open Mips_rtl

module type Model = sig
  type t
  val gpr : t String.Map.t
  val gpri : t Int.Map.t
  val fpr : t String.Map.t
  val fpri : t Int.Map.t
  val hi : t
  val lo : t
end

module type Model_exp = sig
  include Model with type t := exp
end

module type MIPS = sig
  module E : Model_exp
  include Model with type t := var
  val mem : var
  val gpr_bitwidth : int
  val fpr_bitwidth : int
end

let range x = List.range 0 x
let range32 = List.range 0 32
let range64 = List.range 0 64

let make_name prefix i = sprintf "%s%d" prefix i

let make_var_i typ prefix i = Var.create (make_name prefix i) typ

let make_regs_list typ prefix size =
  Array.init ~f:(fun i -> make_var_i typ prefix i) size

let reglist_to_map regs =
  List.fold ~init:String.Map.empty ~f:(fun regs var ->
      String.Map.add regs (Var.name var) var) regs

let make_regs typ ?alias prefix range =
  List.fold ~init:String.Map.empty ~f:(fun regs i ->
      let var = make_var_i typ prefix i in
      let name = Var.name var in
      let regs = String.Map.add regs name var in
      match alias with
      | None -> regs
      | Some a ->
        let name = make_name a i in
        String.Map.add regs name var) range

let make_regs_i typ prefix range =
  List.fold ~init:Int.Map.empty ~f:(fun regs i ->
      Int.Map.add regs i (make_var_i typ prefix i)) range

let make_reg typ name = Var.create name typ
let flag name = Var.create name (Type.imm 1)

module Bitwidth = struct
  let gpr_bitwidth = 32
  let fpr_bitwidth = 64
  let cr_bitwidth = 32
  let lr_bitwidth = 32
end

module Vars = struct
  open Bitwidth

  let fpr = make_regs (Type.imm fpr_bitwidth) "f" range32
  let fpri = make_regs_i (Type.imm fpr_bitwidth) "f" range32
end

let of_vars vars =
  String.Map.map vars ~f:(fun v -> Exp.of_var v)

let of_vars_i vars =
  Int.Map.map vars ~f:(fun v -> Exp.of_var v)

module Exps = struct
  open Vars

  let fpri = of_vars_i fpri
  let fpr = of_vars fpr

end

module type Spec = sig
  val gpr_bitwidth : int
  val addr_size : addr_size
end

module Make_MIPS(S: Spec) : MIPS = struct
  include Bitwidth
  let gpr_bitwidth = S.gpr_bitwidth

  include Vars

  let make_gpr = make_reg (Type.imm gpr_bitwidth)

  let gprs = [
    make_gpr "ZERO", "R0";
    make_gpr "AT", "R1";
    make_gpr "V0", "R2";
    make_gpr "V1", "R3";
    make_gpr "A0", "R4";
    make_gpr "A1", "R5";
    make_gpr "A2", "R6";
    make_gpr "A3", "R7";
    make_gpr "T0", "R8";
    make_gpr "T1", "R9";
    make_gpr "T2", "R10";
    make_gpr "T3", "R11";
    make_gpr "T4", "R12";
    make_gpr "T5", "R13";
    make_gpr "T6", "R14";
    make_gpr "T7", "R15";
    make_gpr "S0", "R16";
    make_gpr "S1", "R17";
    make_gpr "S2", "R18";
    make_gpr "S3", "R19";
    make_gpr "S4", "R20";
    make_gpr "S5", "R21";
    make_gpr "S6", "R22";
    make_gpr "S7", "R23";
    make_gpr "T8", "R24";
    make_gpr "T9", "R25";
    make_gpr "K0", "R26";
    make_gpr "K1", "R27";
    make_gpr "GP", "R28";
    make_gpr "SP", "R29";
    make_gpr "FP", "R30";
    make_gpr "RA", "R31";
  ]

  let gpr =
    List.fold gprs ~init:String.Map.empty ~f:(fun init (reg, alias) ->
        let name = Var.name reg in
        let names = [name; alias] in
        let names =
          if gpr_bitwidth = 64 then (name ^ "_64") :: names
          else names in
        List.fold names ~init ~f:(fun regs name -> Map.add regs name reg))

  let gpri = List.foldi ~init:Int.Map.empty ~f:(fun n regs (reg,_) ->
      Map.add regs n reg) gprs

  let hi = make_reg (Type.imm gpr_bitwidth) "HI"
  let lo = make_reg (Type.imm gpr_bitwidth) "LO"

  module E = struct
    include Exps
    let gpri = of_vars_i gpri
    let gpr = of_vars gpr
    let hi = Exp.of_var hi
    let lo = Exp.of_var lo
  end

  let mem = Var.create "mem" (Type.mem S.addr_size `r8)
end

module Spec32 = struct
  let gpr_bitwidth = 32
  let addr_size = `r32
end

module Spec64 = struct
  let gpr_bitwidth = 64
  let addr_size = `r64
end

module MIPS_32 = Make_MIPS(Spec32)
module MIPS_64 = Make_MIPS(Spec64)

module Make_cpu(M : MIPS) : CPU = struct
  open M

  let mem = M.mem

  let gpr =
    let data = Map.data gpr in
    List.fold data ~init:Var.Set.empty
      ~f:(fun regs v -> Var.Set.add regs v)

  let sp = Var.Set.find_exn gpr ~f:(fun v -> String.is_prefix ~prefix:"SP" (Var.name v))
  let fp = Var.Set.find_exn gpr ~f:(fun v -> String.is_prefix ~prefix:"FP" (Var.name v))

  (* MIPS doesn't have flags, but structure requires them
   * We just make a stubs here *)
  let flag n = Var.create n bool_t
  let zf = flag "ZF"
  let cf = flag "CF"
  let vf = flag "VF"
  let nf = flag "NF"

  let flags = Var.Set.of_list [
      vf; cf; nf; zf;
    ]

  let is = Var.same
  let is_reg r = Set.mem gpr (Var.base r)
  let is_mem = is mem
  let is_sp = is sp
  let is_bp = is fp
  let is_flag _ = false
  let is_zf _ = false
  let is_cf _ = false
  let is_vf _ = false
  let is_nf _ = false
end

module MIPS_32_cpu = Make_cpu(MIPS_32)
module MIPS_64_cpu = Make_cpu(MIPS_64)
