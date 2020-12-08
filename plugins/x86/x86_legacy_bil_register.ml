(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(**

   A wrapper module for BAP registers

   @author Thanassis Avgerinos, Sang Kil Cha, Alexandre Rebert

*)

(* !@ARM: The register module needs to be functorized to allow for
   multiple architectures. Similarly, the uses to Disasm_i386.* and
   Register.* need to be updated with the generic functions. *)

module Disasm_i386 = X86_legacy_bil_disasm_i386

let gax mode = Disasm_i386.gax mode
let gbx mode = Disasm_i386.gbx mode
let gcx mode = Disasm_i386.gcx mode
let gdx mode = Disasm_i386.gdx mode
let gsi mode = Disasm_i386.gsi mode
let gdi mode = Disasm_i386.gdi mode
let gsp mode = Disasm_i386.gsp mode
let gbp mode = Disasm_i386.gbp mode
let gs_base mode = Disasm_i386.ggs_base mode
let fs_base mode = Disasm_i386.gfs_base mode
let gip mode = Disasm_i386.gip mode

let r8  = Disasm_i386.R64.r8
let r9  = Disasm_i386.R64.r9
let r10 = Disasm_i386.R64.r10
let r11 = Disasm_i386.R64.r11
let r12 = Disasm_i386.R64.r12
let r13 = Disasm_i386.R64.r13
let r14 = Disasm_i386.R64.r14
let r15 = Disasm_i386.R64.r15

let gpr_list mode =
  [gax mode; gcx mode; gdx mode; gbx mode; gsp mode; gbp mode; gsi mode; gdi mode]
  @ if mode = `x86_64 then
    [r8; r9; r10; r11; r12; r13; r14; r15]
  else
    []

let ymms mode = Disasm_i386.gymms mode

let ymm_list mode =
  ymms mode |> Array.to_list

let flag_offset_list =
  [Disasm_i386.cf, 0;
   Disasm_i386.pf, 2;
   Disasm_i386.af, 4;
   Disasm_i386.zf, 6;
   Disasm_i386.sf, 7;
   Disasm_i386.oF, 11]

let flag_list =
  List.map fst flag_offset_list

let float_list = [
  Disasm_i386.x87_ie;
  Disasm_i386.x87_de;
  Disasm_i386.x87_ze;
  Disasm_i386.x87_oe;
  Disasm_i386.x87_ue;
  Disasm_i386.x87_pe;
  Disasm_i386.x87_sf;
  Disasm_i386.x87_es;
  Disasm_i386.x87_c0;
  Disasm_i386.x87_c1;
  Disasm_i386.x87_c2;
  Disasm_i386.x87_top;
  Disasm_i386.x87_c3;
  Disasm_i386.x87_b;
  Disasm_i386.x87_im;
  Disasm_i386.x87_dm;
  Disasm_i386.x87_zm;
  Disasm_i386.x87_om;
  Disasm_i386.x87_um;
  Disasm_i386.x87_pm;
  Disasm_i386.x87_6;
  Disasm_i386.x87_7;
  Disasm_i386.x87_pc;
  Disasm_i386.x87_rc;
  Disasm_i386.x87_x;
  Disasm_i386.x87_13;
  Disasm_i386.x87_14;
  Disasm_i386.x87_15;
] @ (Array.to_list Disasm_i386.st)

let gmem mode = Disasm_i386.gmem mode
