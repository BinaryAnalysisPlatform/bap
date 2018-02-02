open Core_kernel.Std
open Bap.Std
open Mips_rtl

module type Model = sig
    type t
    val gpr : t String.Map.t
    val gpri : t Int.Map.t
    val fpr : t String.Map.t
    val fpri : t Int.Map.t
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

    (* Define "standard" aliases *)
    let zero = make_reg (Type.imm gpr_bitwidth) "ZERO"
    let at = make_reg (Type.imm gpr_bitwidth) "AT"
    let v = make_regs_list (Type.imm gpr_bitwidth) "V" 4
    let a = make_regs_list (Type.imm gpr_bitwidth) "A" 4
    let t = make_regs_list (Type.imm gpr_bitwidth) "T" 10
    let s = make_regs_list (Type.imm gpr_bitwidth) "S" 8
    let k = make_regs_list (Type.imm gpr_bitwidth) "K" 2
    let gp = make_reg (Type.imm gpr_bitwidth) "GP"
    let sp = make_reg (Type.imm gpr_bitwidth) "SP"
    let fp = make_reg (Type.imm gpr_bitwidth) "FP"
    let ra = make_reg (Type.imm gpr_bitwidth) "RA"

    (* TODO: Here I am not sure if HI and LO belongs to GPRs though *)
    let gprs = Array.concat [
        v; a; t; s; k;
        [|zero; at; gp; sp; fp; ra;|];
    ]

    let gpr = Array.to_list gprs |> reglist_to_map
    (* FIXME: better representation *)
    let gpri = make_regs_i (Type.imm fpr_bitwidth) "R" range32

    module E = struct
        include Exps
        let gpri = of_vars_i gpri
        let gpr = of_vars gpr
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

    let sp = Var.Set.find_exn gpr ~f:(fun v -> Var.name v = "SP")
    let fp = Var.Set.find_exn gpr ~f:(fun v -> Var.name v = "FP")

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

