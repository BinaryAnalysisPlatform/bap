open Core_kernel
open Bap.Std

open Powerpc_rtl

module type Model = sig
  type t
  val gpr  : t String.Map.t
  val gpri : t Int.Map.t
  val fpr : t String.Map.t
  val fpri : t Int.Map.t
  val vr : t String.Map.t
  val vri : t Int.Map.t
  val ctr : t
  val lr : t
  val tar : t
  val cri : t Int.Map.t
  val crn : t String.Map.t
  val so : t
  val ca : t
  val ov : t
  val ca32 : t
  val ov32 : t
end

module type Model_exp = sig
  include Model with type t := exp
  (** condition register  *)
  val cr : exp

  (** condition register fields *)
  val cr_fields  : exp String.Map.t
  val cri_fields : exp Int.Map.t
end

module type Bitwidth = sig
  val gpr_bitwidth : int
  val fpr_bitwidth : int
  val lr_bitwidth  : int
  val ctr_bitwidth : int
  val tar_bitwidth : int
  val cr_bitwidth  : int
  val vr_bitwidth  : int
end

module type PowerPC = sig
  module E : Model_exp
  include Model with type t := var
  include Bitwidth
  val mem : var
  val flags : Var.Set.t
end

let range32 = List.range 0 32
let range64 = List.range 0 64

let make_name prefix i = sprintf "%s%d" prefix i

let make_var_i typ prefix i = Var.create (make_name prefix i) typ

let make_regs typ ?alias prefix range =
  List.fold ~init:String.Map.empty ~f:(fun regs i ->
      let var = make_var_i typ prefix i in
      let name = Var.name var in
      let regs = Map.set regs name var in
      match alias with
      | None -> regs
      | Some a ->
        let name = make_name a i in
        Map.set regs name var) range


let make_regs_i typ prefix range =
  List.fold ~init:Int.Map.empty ~f:(fun regs i ->
      Map.set regs i (make_var_i typ prefix i)) range

let flag name = Var.create name (Type.imm 1)

module Vars (B : Bitwidth) = struct
  open B

  (** general purpose registers  *)
  let gpr = make_regs (Type.imm gpr_bitwidth) "R" ~alias:"X" range32
  let gpri = make_regs_i (Type.imm gpr_bitwidth) "R" range32

  (** floating point registers *)
  let fpr = make_regs (Type.imm fpr_bitwidth) "F" range32
  let fpri = make_regs_i (Type.imm fpr_bitwidth) "F" range32

  (** vector registers *)
  let vr = make_regs (Type.imm vr_bitwidth) "VR" range32
  let vri = make_regs_i (Type.imm vr_bitwidth) "VR" range32

  (** count register  *)
  let ctr = Var.create "CTR" (Type.imm ctr_bitwidth)

  (** link register  *)
  let lr = Var.create "LR" (Type.imm lr_bitwidth)

  (** target register  *)
  let tar = Var.create "TAR" (Type.imm tar_bitwidth)

  (** FPRF floating point result flags  *)
  let float_c = flag "C"          (** Result Class Descriptor        *)
  let float_less = flag "FL"      (** Less Than or Negative           *)
  let float_equal = flag "FE"     (** Greater Than or Positive        *)
  let float_greater = flag "FG"   (** Floating-Point Equal or Zero    *)
  let float_unordered = flag "FU" (** Floating-Point Unordered or NaN *)

  (** condition register bits  *)
  let cr0  = flag "CR7UN"
  let cr1  = flag "CR7EQ"
  let cr2  = flag "CR7GT"
  let cr3  = flag "CR7LT"
  let cr4  = flag "CR6UN"
  let cr5  = flag "CR6EQ"
  let cr6  = flag "CR6GT"
  let cr7  = flag "CR6LT"
  let cr8  = flag "CR5UN"
  let cr9  = flag "CR5EQ"
  let cr10 = flag "CR5GT"
  let cr11 = flag "CR5LT"
  let cr12 = flag "CR4UN"
  let cr13 = flag "CR4EQ"
  let cr14 = flag "CR4GT"
  let cr15 = flag "CR4LT"
  let cr16 = flag "CR3UN"
  let cr17 = flag "CR3EQ"
  let cr18 = flag "CR3GT"
  let cr19 = flag "CR3LT"
  let cr20 = flag "CR2UN"
  let cr21 = flag "CR2EQ"
  let cr22 = flag "CR2GT"
  let cr23 = flag "CR2LT"
  let cr24 = flag "CR1UN"
  let cr25 = flag "CR1EQ"
  let cr26 = flag "CR1GT"
  let cr27 = flag "CR1LT"
  let cr28 = flag "CR0UN"
  let cr29 = flag "CR0EQ"
  let cr30 = flag "CR0GT"
  let cr31 = flag "CR0LT"

  let cr_bits = [
    cr0;  cr1;  cr2;  cr3;  cr4;  cr5;  cr6;  cr7;
    cr8;  cr9;  cr10; cr11; cr12; cr13; cr14; cr15;
    cr16; cr17; cr18; cr19; cr20; cr21; cr22; cr23;
    cr24; cr25; cr26; cr27; cr28; cr29; cr30; cr31;
  ]

  let cri =
    let _, bits =
      List.fold (List.rev cr_bits) ~init:(0,Int.Map.empty)
        ~f:(fun (num, bits) bit ->
            num + 1, Map.set bits ~key:num ~data:bit) in
    bits

  let crn =
    Int.Map.fold cri ~init:String.Map.empty
      ~f:(fun ~key:_ ~data:var acc ->
          Map.set acc (Var.name var) var)

  let fields = [
    "CR0", 0, (cr28, cr29, cr30, cr31);
    "CR1", 1, (cr24, cr25, cr26, cr27);
    "CR2", 2, (cr20, cr21, cr22, cr23);
    "CR3", 3, (cr16, cr17, cr18, cr19);
    "CR4", 4, (cr12, cr13, cr14, cr15);
    "CR5", 5, (cr8,  cr9,  cr10, cr11);
    "CR6", 6, (cr4,  cr5,  cr6,  cr7);
    "CR7", 7, (cr0,  cr1,  cr2,  cr3);
  ]

  let cr_fields =
    List.fold fields ~init:String.Map.empty ~f:(fun fs (name, _, fd) ->
        Map.set fs name fd)

  let cri_fields =
    List.fold fields ~init:Int.Map.empty ~f:(fun fs (_, index, fd) ->
        Map.set fs index fd)

  (** fixed precision flags  *)
  let so = flag "SO" (** summary overflow *)
  let ca = flag "CA"
  let ov = flag "OV"
  let ca32 = flag "CA32" (** carry of low-order 32 bit result *)
  let ov32 = flag "OV32" (** overflow of low-order 32 bit result *)

end

let of_vars vars =
  Map.map vars ~f:(fun v -> Exp.of_var v)

let of_vars_i vars =
  Map.map vars ~f:(fun v -> Exp.of_var v)

module Exps(B : Bitwidth) = struct
  module Vars = Vars(B)
  open Vars

  let gpr  = of_vars gpr
  let gpri = of_vars_i gpri
  let fpr  = of_vars fpr
  let fpri = of_vars_i fpri
  let vr   = of_vars vr
  let vri  = of_vars_i vri
  let ctr  = Exp.of_var ctr
  let lr   = Exp.of_var lr
  let tar  = Exp.of_var tar

  let cri = Map.map cri ~f:(fun v -> Exp.of_var v)

  let crn =
    Int.Map.fold Vars.cri ~init:String.Map.empty
      ~f:(fun ~key:_ ~data:var acc ->
          Map.set acc (Var.name var) (Exp.of_var var))

  let so  = Exp.of_var so
  let ca  = Exp.of_var ca
  let ov  = Exp.of_var ov
  let ca32 = Exp.of_var ca32
  let ov32 = Exp.of_var ov32

  let cr = Exp.of_vars (List.rev cr_bits)

  let cr_fields =
    Map.map cr_fields ~f:(fun (b3,b2,b1,b0) -> Exp.of_vars [b0;b1;b2;b3])

  let cri_fields =
    Map.map cri_fields ~f:(fun (b3,b2,b1,b0) -> Exp.of_vars [b0;b1;b2;b3])

end

module type Spec = sig
  val gpr_bitwidth : int
  val addr_size : addr_size
end

module Make_ppc(S : Spec) : PowerPC = struct

  module Bitwidth = struct
    let gpr_bitwidth = S.gpr_bitwidth
    let fpr_bitwidth = 64
    let lr_bitwidth  = S.gpr_bitwidth
    let ctr_bitwidth = S.gpr_bitwidth
    let tar_bitwidth = S.gpr_bitwidth
    let cr_bitwidth  = 32
    let vr_bitwidth  = 128
  end

  module Vars = Vars(Bitwidth)
  module E = Exps(Bitwidth)

  include Vars
  include Bitwidth

  let mem = Var.create "mem" (Type.mem S.addr_size `r8)

  let flags = Var.Set.of_list [
      so; ca; ca32; ov; ov32;
      Map.find_exn cri 0;
      Map.find_exn cri 1;
      Map.find_exn cri 2;
    ]

end

module Spec32 = struct
  let gpr_bitwidth = 32
  let addr_size = `r32
end

module Spec64 = struct
  let gpr_bitwidth = 64
  let addr_size = `r64
end

module PowerPC_32 = Make_ppc(Spec32)
module PowerPC_64 = Make_ppc(Spec64)

module Make_cpu(P : PowerPC) : CPU = struct
  open P

  let mem = P.mem

  let gpr =
    let data = Map.data gpr in
    List.fold data ~init:Var.Set.empty
      ~f:(fun regs v -> Var.Set.add regs v)

  let sp = Var.Set.find_exn gpr ~f:(fun v -> String.equal (Var.name v) "R1")
  let vf = ov
  let cf = ca
  let nf = Map.find_exn cri 0
  let zf = Map.find_exn cri 1

  let flags = Var.Set.of_list [
      so; ca; ov; cf; nf; zf; ca32; ov32;
    ]

  let is = Var.same
  let is_reg r = Set.mem gpr (Var.base r)
  let is_flag r = Set.mem flags (Var.base r)
  let is_zf = is zf
  let is_cf = is ca
  let is_vf = is vf
  let is_nf = is nf
  let is_mem = is mem
  let is_sp = is sp
  let is_bp _ = false
end

module PowerPC_32_cpu = Make_cpu(PowerPC_32)
module PowerPC_64_cpu = Make_cpu(PowerPC_64)
