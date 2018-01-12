open Core_kernel.Std
open Bap.Std

open Bap_powerpc_rtl
open Bap_powerpc_dsl
open Bap_powerpc_utils
open Bap_powerpc_model

type cpu = {
  load      : exp -> bitwidth -> exp;
  store     : exp -> exp -> bitwidth -> rtl;
  jmp       : exp -> rtl;
  cia       : exp;
  addr_size : bitwidth;
  gpr_width : bitwidth;
  reg       : (op -> exp) ec;
  gpr       : int -> exp;
  fpr       : int -> exp;
  vr        : int -> exp;
  ctr       : exp;
  lr        : exp;
  tar       : exp;
  cr        : exp;
  cr0       : exp;
  cr1       : exp;
  cr2       : exp;
  cr3       : exp;
  cr4       : exp;
  cr5       : exp;
  cr6       : exp;
  cr7       : exp;
  so        : exp;
  ca        : exp;
  ov        : exp;
  ca32      : exp;
  ov32      : exp;
}

let size_of_width x =
  let x = int_of_bitwidth x in
  match Size.of_int x with
  | Ok s -> s
  | Error _ -> ppc_fail "invalid size: %d" x

let find m reg =
  let (module M : PowerPC) = m in
  let open M in
  let open E in
  let find_reg regs reg = String.Map.find regs (Reg.name reg) in
  let find_gpr = find_reg gpr in
  let find_vr  = find_reg vr in
  let find_fpr = find_reg fpr in
  let find_cr_bit = find_reg crn in
  let find_cr_field = find_reg cr_fields in
  let reg_searches =
    [find_gpr; find_cr_bit; find_cr_field; find_fpr; find_vr] in
  List.filter_map reg_searches ~f:(fun f -> f reg) |> function
  | [] -> Exp.of_word (Word.zero gpr_bitwidth)
  | hd :: [] -> hd
  | _ -> ppc_fail "Register name %s is ambigous!!!" (Reg.name reg)

let make_cpu addr_size endian memory =
  let (module M) = match addr_size with
    | `r32 -> (module PowerPC_32 : PowerPC)
    | `r64 -> (module PowerPC_64) in
  let open M in
  let open E in
  let reg = reg (find (module M)) in
  let load addr width =
    let size = size_of_width width in
    Exp.load mem addr endian size in
  let store addr data width =
    let size = size_of_width width in
    store mem addr data endian size in
  let cia = Memory.min_addr memory |>
            Exp.of_word |>
            Exp.signed in
  let jmp e = match addr_size with
    | `r32 -> jmp (low word e)
    | `r64 -> jmp e in
  let find name regs n =
    try
      Int.Map.find_exn regs n
    with _ ->
      ppc_fail "%s with number %d not found" name n in
  let gpr n = find "GPR" gpri n in
  let fpr n = find "FPR" fpri n in
  let vr  n = find "VR"  vri  n in
  let cr0 = Int.Map.find_exn cri_fields 0 in
  let cr1 = Int.Map.find_exn cri_fields 1 in
  let cr2 = Int.Map.find_exn cri_fields 2 in
  let cr3 = Int.Map.find_exn cri_fields 3 in
  let cr4 = Int.Map.find_exn cri_fields 4 in
  let cr5 = Int.Map.find_exn cri_fields 5 in
  let cr6 = Int.Map.find_exn cri_fields 6 in
  let cr7 = Int.Map.find_exn cri_fields 7 in
  let addr_size = match addr_size with
    | `r32 -> word
    | `r64 -> doubleword in
  let gpr_width = addr_size in
  { load; store; jmp; cia; addr_size; gpr_width;
    reg; gpr; fpr; vr;
    cr; cr0; cr1; cr2; cr3; cr4; cr5; cr6; cr7;
    ctr; lr; tar;
    so; ca; ov; ca32; ov32;}
