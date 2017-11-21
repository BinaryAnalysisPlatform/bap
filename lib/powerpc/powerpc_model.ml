open Core_kernel.Std
open Bap.Std

let range32 = List.range 0 32
let range64 = List.range 0 64

let make_var_i typ prefix i = Var.create (sprintf "%s%d" prefix i) typ

let make_regs typ prefix range =
  List.fold ~init:Var.Set.empty ~f:(fun regs i ->
      Var.Set.add regs (make_var_i typ prefix i)) range

let flag name = Var.create name (Type.imm 1)

module Hardware = struct

  let gpr_bitwidth = 64
  let fpr_bitwidth = 64
  let vr_bitwidth  = 128
  let cr_bitwidth  = 32
  let xer_bitwidth = 64
  let lr_bitwidth  = 64
  let ctr_bitwidth = 64
  let tar_bitwidth = 64

  let gpr = make_regs (Type.imm gpr_bitwidth) "R" range32

  (** floating point registers *)
  let fpr = make_regs (Type.imm fpr_bitwidth) "F" range32

  (** vector registers *)
  let vr = make_regs (Type.imm vr_bitwidth) "VR" range32

  (** count register  *)
  let ctr = Var.create "CTR" (Type.imm ctr_bitwidth)

  (** link register  *)
  let lr = Var.create "LR" (Type.imm lr_bitwidth)

  (** target register  *)
  let tar = Var.create "TAR" (Type.imm tar_bitwidth)

  (** fixed point exception register  *)
  let xer = Var.create "TAR" (Type.imm xer_bitwidth)

  (** fixed precision flags  *)
  let so = flag "SO" (** summary overflow *)
  let ca = flag "CA"
  let ov = flag "OV"
  let ca32 = flag "CA32" (** carry of low-order 32 bit result *)
  let ov32 = flag "OV32" (** overflow of low-order 32 bit result *)

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

  let cr =
    let bits = [
      cr0;  cr1;  cr2;  cr3;  cr4;  cr5;  cr6;  cr7;
      cr8;  cr9;  cr10; cr11; cr12; cr13; cr14; cr15;
      cr16; cr17; cr18; cr19; cr20; cr21; cr22; cr23;
      cr24; cr25; cr26; cr27; cr28; cr29; cr30; cr31; ] in
    let _, bits =
      List.fold bits ~init:(0,Int.Map.empty)
        ~f:(fun (num, bits) bit ->
            num + 1, Int.Map.add bits ~key:num ~data:bit) in
    bits

  let cr_fields =
    let fields = [
      "CR0", (cr28, cr29, cr30, cr31);
      "CR1", (cr24, cr25, cr26, cr27);
      "CR2", (cr20, cr21, cr22, cr23);
      "CR3", (cr16, cr17, cr18, cr19);
      "CR4", (cr12, cr13, cr14, cr15);
      "CR5", (cr8,  cr9,  cr10, cr11);
      "CR6", (cr4,  cr5,  cr6,  cr7);
      "CR7", (cr0,  cr1,  cr2,  cr3);
    ] in
    List.fold fields ~init:String.Map.empty ~f:(fun fs (name, fd) ->
        String.Map.add fs name fd)


end

module PowerPC_32 = struct
  include Hardware
  let mem = Var.create "mem" (Type.mem `r32 `r8)
end

module PowerPC_64 = struct
  include Hardware
  let mem = Var.create "mem" (Type.mem `r64 `r8)
end

module type PowerPC_cpu = sig
  include module type of Hardware
  val mem : var
end

module Make_cpu(P : PowerPC_cpu) : CPU = struct
  include P

  let sp = Var.Set.find_exn gpr ~f:(fun v -> Var.name v = "R1")
  let vf = ov
  let cf = ca
  let nf = cr0
  let zf = cr2

  let flags = Var.Set.of_list [
      so; ca; ov; cf; nf; zf; ca32; ov32;
      float_c; float_less; float_equal;
      float_greater; float_unordered
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
