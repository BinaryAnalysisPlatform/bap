(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(** Native lifter of x86 instructions to the BAP IL *)
module Bil = X86_legacy_bil

open Big_int_Z
open Bil
open Bil.Ast
open Type

module Big_int_convenience = X86_legacy_bil_big_int_convenience
module Var_temp = X86_legacy_bil_var_temp

open Big_int_convenience
open X86_legacy_bil_convenience

(* Note: remove these if we open Core_kernel here. *)
module List = Core_kernel.List
module Int = Core_kernel.Int
module Option = Core_kernel.Option

let compute_segment_bases = ref false

(* Note: In general, the function g is the get memory function.  The
   variable na refers to the next address or next instruction.

   To help understand this file, please refer to the Intel Instruction
   Set Reference. For consistency, any section numbers here are wrt
   Order Number: 253666-035US June 2010 and 253667-035US.


   The x86 instruction format is as follows:
   Instruction Prefixes: 0-4bytes (1 byte per prefix)
   Optional Rex Prefix: 1 byte
   Opcode: 1 - 3 bytes.
   ModR/M: 1 optional byte
   SIB: 1 optional byte
   Displacement: 0,1,2, or 4 bytes.
   Immediate: 0,1,2, or 4 bytes

   ModR/M has the following format:
   7:6 Mod
   5:3 Reg or extra opcode bits
   2:0 R/M

   SIB:
   7:6 Scale
   5:3 Index
   2:0 Base


   In order to get the most common unsupported opcodes, you can run something like:
   for f in bin/*; do BAP_DEBUG_MODULES=AsmirV ~/bap/trunk/utils/iltrans -bin $f ; done 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n

   To optimize for number of programs disassembled:
   for f in bin/*; do echo -n "$f "; BAP_DEBUG_MODULES=AsmirV iltrans -bin $f 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n  | wc -l; done | sort -n -k 2

*)

(* type segment = CS | SS | DS | ES | FS | GS *)

exception Disasm_i386_exception of string

type binopf = Ast.exp -> Ast.exp -> Ast.exp

type mode = [ `x86 | `x86_64 ]
let type_of_mode = function
  | `x86 -> Reg 32
  | `x86_64 -> Reg 64

type order = Low | High

type direction = Forward | Backward

type operand =
  | Oreg of int
  | Ovec of int
  | Oseg of int
  | Oaddr of Ast.exp
  | Oimm of big_int

type jumptarget =
  | Jabs of operand
  | Jrel of Type.addr * Type.addr (* next ins address, offset *)

(* See section 4.1 of the Intel® 64 and IA-32 Architectures Software
   Developer’s Manual, Volumes 2A & 2B: Instruction Set Reference
   (order numbers 253666 and 253667) *)
module Pcmpstr = struct

  type ssize = Bytes | Words
  let ssize_to_string = function
    | Bytes -> "Bytes"
    | Words -> "Words"
  type ssign = Signed | Unsigned
  let ssign_to_string = function
    | Signed -> "Signed"
    | Unsigned -> "Unsigned"
  type agg = EqualAny | Ranges | EqualEach | EqualOrdered
  let agg_to_string = function
    | EqualAny -> "EqualAny"
    | Ranges -> "Ranges"
    | EqualEach -> "EqualEach"
    | EqualOrdered -> "EqualOrdered"
  type outselectsig = LSB | MSB (* For PCMPESTRI/PCMPISTRI, choosees LSB or MSB.  *)
  let outselectsig_to_string = function
    | LSB -> "LSB"
    | MSB -> "MSB"
  type outselectmask = Bitmask | Bytemask (* For PCMPESTRM/PCMPISTRM, represents bit mask/word mask. *)
  let outselectmask_to_string = function
    | Bitmask -> "Bitmask"
    | Bytemask -> "Bytemask"

  let sig_to_mask = function
    | LSB -> Bitmask
    | MSB -> Bytemask

  (* See Section 4.1 of Intel manual for more
     information on the immediate control byte.

     i[0]:
     0 = 16 packed bytes
     1 =  8 packed words
     i[1]:
     0 = packed elements are unsigned
     1 = packed elements are signed
     i[3:2]:
     00 = "equal any"
     01 = "ranges"
     10 = "each each"
     11 = "equal ordered"
     i[4]:
     0 = IntRes1 unmodified
     1 = IntRes1 is negated (1's complement)
     i[5]:
     0 = Negation of IntRes1 is for all 16 (8) bits
     1 = Negation of IntRes1 is masked by reg/mem validity
     i[6]:
     0 = Use least significant bit for IntRes2
     1 = Use most significant bit for IntRes2
     i[7]: Undefined, set to 0.
  *)
  type imm8cb = {
    ssize : ssize;
    ssign : ssign;
    agg : agg;
    negintres1 : bool;
    maskintres1 : bool;
    outselectsig : outselectsig;
    outselectmask : outselectmask;
  }

  type out = Index | Mask
  type len = Implicit | Explicit

  (** Information about the type of pcmp instruction. *)
  type pcmpinfo = {
    out : out;
    len : len;
  }
end

type offsetinfo = {
  offlen : typ;
  offtyp : typ;
  offop : operand;
  offsrcoffset : int;
  offdstoffset : int;
}


(* prefix names *)
let _pref_lock = 0xf0
and repnz = 0xf2
and repz = 0xf3
and hint_bnt = 0x2e
and hint_bt = 0x3e
and pref_cs = 0x2e
and pref_ss = 0x36
and pref_ds = 0x3e
and pref_es = 0x26
and pref_fs = 0x64
and pref_gs = 0x65
and pref_opsize = 0x66
and pref_addrsize = 0x67

(* Prefixes that we can usually handle automatically *)
let _standard_prefs = [pref_opsize; pref_addrsize; hint_bnt; hint_bt; pref_cs; pref_ss; pref_ds; pref_es; pref_fs; pref_gs]

(* See Table 2-4: REX Prefix Fields. *)
type rex = {
  rex_w : bool; (* Bit 3: 1 = 64-bit operand size *)
  rex_r : bool; (* Bit 2: Extension of ModR/M reg field *)
  rex_x : bool; (* Bit 1: Extension of SIB index field *)
  rex_b : bool; (* Bit 0: Extension of ModR/M r/m field, SIB base
                   field, or opcode reg field *)
}

type vex = {
  vex_nr : bool; (* inverted rex_r bit *)
  vex_nx : bool; (* inverted rex_x bit *)
  vex_nb : bool; (* inverted rex_b bit *)
  vex_map_select : int; (* Specifies the opcode map to use *)
  vex_we : bool; (* For int instructions, equivalent to rex.w. For non-int instructions, opcode extension bit. *)
  vex_v : int; (* additional instruction operand (XMM or YMM register) *)
  vex_l : bool; (* 0 = 128-bit operands (xmm), 1 = 256-bit vector operands (ymm) *)
  vex_pp : int; (* Specifies mandatory prefix (0=none, 1=pref_opsize 2=repz 3=repnz) *)
}

type prefix = {
  addrsize : typ;
  opsize   : typ; (* General operand size *)
  bopsize  : typ; (* Operand size that defaults to machine size
                     (e.g. for pop) *)
  mopsize  : typ; (* Multi-scalar operand size *)
  repeat   : bool;
  nrepeat  : bool;
  addrsize_override : bool;
  opsize_override : bool;
  rex : rex option;
  vex : vex option;
  r_extend : int; (* extended r bit *)
  rm_extend : int; (* extended rm bit or sib base *)
  sib_extend : int; (* extended sib index bit *)
  (* add more as needed *)
}

(** disfailwith is a non-fatal disassembly exception. *)
let disfailwith s = raise (Disasm_i386_exception s)

let unimplemented s  = disfailwith ("disasm_i386: unimplemented feature: "^s)

let (&) = (land)
and (>>) = (lsr)
let ite t b e1 e2 =
  X86_legacy_bil_convenience.exp_ite ~t b e1 e2

(* register widths *)
let r1 = reg_1
let _r4 = Reg 4
let r8 = reg_8
let r16 = reg_16
let r32 = reg_32
let r64 = reg_64
let r128 = reg_128
let r256 = reg_256
let _xmm_t = r128
let ymm_t = r256
let st_t = Reg 80

(** Only use this for registers, not temporaries *)
let nv = Var.newvar
let nt = Var_temp.nt

type multimodereg = { v32: Var.t; v64: Var.t }
(* new multi-mode variable *)
let nmv n32 t32 n64 t64 = { v32=nv n32 t32; v64=nv n64 t64; }

let gv mode { v32; v64 } = match mode with
  | `x86 -> v32
  | `x86_64 -> v64

let ge mode mv = Var (gv mode mv)

(* registers *)

let rbp = nmv "R_EBP" r32 "R_RBP" r64
and rsp = nmv "R_ESP" r32 "R_RSP" r64
and rsi = nmv "R_ESI" r32 "R_RSI" r64
and rdi = nmv "R_EDI" r32 "R_RDI" r64
and rip = nmv "R_EIP" r32 "R_RIP" r64 (* XXX why is eip in here? *)
and rax = nmv "R_EAX" r32 "R_RAX" r64
and rbx = nmv "R_EBX" r32 "R_RBX" r64
and rcx = nmv "R_ECX" r32 "R_RCX" r64
and rdx = nmv "R_EDX" r32 "R_RDX" r64
and rflags = nmv "R_EFLAGS" r32 "R_RFLAGS" r64 (* XXX why is eflags in here? *)
(* condition flag bits *)
and cf = nv "R_CF" r1
and pf = nv "R_PF" r1
and af = nv "R_AF" r1
and zf = nv "R_ZF" r1
and sf = nv "R_SF" r1
and oF = nv "R_OF" r1
and df = nv "R_DF" r1

(* segment registers and bases *)
and fs_base = nmv "R_FS_BASE" r32 "R_FS_BASE" r64
and gs_base = nmv "R_GS_BASE" r32 "R_GS_BASE" r64

and cs = nv "R_CS" r16
and ds = nv "R_DS" r16
and es = nv "R_ES" r16
and fs = nv "R_FS" r16
and gs = nv "R_GS" r16
and ss = nv "R_SS" r16

and gdt = nmv "R_GDTR" r32 "R_GDTR" r64
and ldt = nmv "R_LDTR" r32 "R_LDTR" r64

and x87_ie = nv "R_X87_IE" r1
and x87_de = nv "R_X87_DE" r1
and x87_ze = nv "R_X87_ZE" r1
and x87_oe = nv "R_X87_OE" r1
and x87_ue = nv "R_X87_UE" r1
and x87_pe = nv "R_X87_PE" r1
and x87_sf = nv "R_X87_SF" r1
and x87_es = nv "R_X87_ES" r1
and x87_c0 = nv "R_X87_C0" r1
and x87_c1 = nv "R_X87_C1" r1
and x87_c2 = nv "R_X87_C2" r1
and x87_top = nv "R_X87_TOP" (Reg 3)
and x87_c3 = nv "R_X87_C3" r1
and x87_b = nv "R_X87_B" r1

and x87_im = nv "R_X87_IM" r1
and x87_dm = nv "R_X87_DM" r1
and x87_zm = nv "R_X87_ZM" r1
and x87_om = nv "R_X87_OM" r1
and x87_um = nv "R_X87_UM" r1
and x87_pm = nv "R_X87_PM" r1
and x87_6 = nv "R_X87_6" r1
and x87_7 = nv "R_X87_7" r1
and x87_pc = nv "R_X87_PC" (Reg 2)
and x87_rc = nv "R_X87_RC" (Reg 2)
and x87_x = nv "R_X87_X" r1
and x87_13 = nv "R_X87_13" r1
and x87_14 = nv "R_X87_14" r1
and x87_15 = nv "R_X87_15" r1

and mxcsr = nv "R_MXCSR" r32

let gax mode = gv mode rax
let gcx mode = gv mode rcx
let gdx mode = gv mode rdx
let gbx mode = gv mode rbx
let gsp mode = gv mode rsp
let gbp mode = gv mode rbp
let gsi mode = gv mode rsi
let gdi mode = gv mode rdi
let gip mode = gv mode rip
let gflags mode = gv mode rflags
let gfs_base mode = gv mode fs_base
let ggs_base mode = gv mode gs_base

(* r8 -> r15 *)
let nums = Array.init 8 (fun i -> nmv "ERROR" (Reg 0) (Printf.sprintf "R_R%d" (i+8)) r64)

let _fpu_status = x87_ie::x87_de::x87_ze::x87_oe::x87_ue::x87_pe::x87_sf::
                  x87_es::x87_c0::x87_c1::x87_c2::x87_top::x87_c3::x87_b::[]
let _fpu_control = x87_im::x87_dm::x87_zm::x87_om::x87_um::x87_pm::x87_6::
                   x87_7::x87_pc::x87_rc::x87_x::x87_13::x87_14::x87_15::[]

let _concat_le_vars_into_exp l =
  let f acc x = X86_legacy_bil_convenience.concat (Var x) acc in
  List.fold_left ~f ~init:(Var (List.hd_exn l)) @@ List.tl_exn l

let x87_status_word = _concat_le_vars_into_exp _fpu_status
let x87_control_word = _concat_le_vars_into_exp _fpu_control

let x87_top_exp = Var x87_top

(*
let xmms = Array.init 8 (fun i -> nv (Printf.sprintf "R_XMM%d" i) xmm_t)
*)

let ymms = Array.init 32 (fun i -> nv (Printf.sprintf "R_YMM%d" i) ymm_t)

let gymms mode =
  let regs = match mode with
    | `x86 -> 8
    | `x86_64 -> 16
  in
  Array.sub ymms 0 regs

(* floating point registers *)
let st = Array.init 8 (fun i -> nv (Printf.sprintf "R_X87_ST%d" i) st_t)
let st_tag = Array.init 8 (fun i -> nv (Printf.sprintf "R_X87_TAG%d" i) (Reg 2))
let st_exps = Array.map (fun st -> Var st) st
let st_tag_exps = Array.map (fun st_tag -> Var st_tag) st_tag


(* Tag values for x87 registers defined in Secion 8.1.7 of Volume 1 in
   the Intel Manual *)
let fnorm = X86_legacy_bil_convenience.it 0 (Reg 2)
let fzero = X86_legacy_bil_convenience.it 1 (Reg 2)
let fspecial = X86_legacy_bil_convenience.it 2 (Reg 2)
let fempty = X86_legacy_bil_convenience.it 3 (Reg 2)

let _mvs {v64; v32} = [v64; v32]

let shared_regs =
  cf::pf::af::zf::sf::oF::df::cs::ds::es::fs::gs::ss::mxcsr::[]
  @ Array.to_list st @ _fpu_control

let shared_multi_regs =
  rbp::rsp::rsi::rdi::rip::rax::rbx::rcx::rdx::rflags::fs_base::gs_base::[]

let regs_x86 : var list =
  shared_regs
  @ List.map ~f:(fun {v64; v32} -> v32) shared_multi_regs
  @ Array.to_list (Array.sub ymms 0 8)

let (r_8, r_9, r_10, r_11, r_12, r_13, r_14, r_15) = match Array.to_list nums with
  | (r_8::r_9::r_10::r_11::r_12::r_13::r_14::r_15::[]) -> (r_8, r_9, r_10, r_11, r_12, r_13, r_14, r_15)
  | _ -> failwith "Impossible, matching against a list of known size"

let regs_x86_64 : var list =
  shared_regs
  @ List.map ~f:(fun {v64; v32} -> v64) (shared_multi_regs @ (Array.to_list nums))
  @ Array.to_list ymms

let regs_full : var list =
  shared_regs
  @ List.concat (List.map ~f:(fun {v64; v32} -> [v32; v64]) shared_multi_regs)
  @ List.map ~f:(fun {v64; v32} -> v64) (Array.to_list nums)
  @ Array.to_list ymms

let o_rax = Oreg 0
and o_rcx = Oreg 1
and o_rdx = Oreg 2
and o_rbx = Oreg 3
and o_rsp = Oreg 4
and o_rbp = Oreg 5
and _o_rsi = Oreg 6
and _o_rdi = Oreg 7

let _o_es = Oseg 0
and _o_cs = Oseg 1
and _o_ss = Oseg 2
and _o_ds = Oseg 3
and o_fs = Oseg 4
and o_gs = Oseg 5

(* let esp_e = Var esp *)
(* and ebp_e = Var ebp *)
(* and esi_e = Var esi *)
(* and edi_e = Var edi *)
(* and ecx_e = Var ecx *)
(* and eax_e = Var eax *)
(* and edx_e = Var edx *)

let mem = nmv "mem32" (TMem (r32, r8)) "mem64" (TMem (r64, r8))

let gmem mode = gv mode mem

(* 64-bit registers *)
module R64 = struct
  let r8 = r_8.v64
  let r9 = r_9.v64
  let r10 = r_10.v64
  let r11 = r_11.v64
  let r12 = r_12.v64
  let r13 = r_13.v64
  let r14 = r_14.v64
  let r15 = r_15.v64
end


let cf_e = Var cf
and pf_e = Var pf
and af_e = Var af
and zf_e = Var zf
and sf_e = Var sf
and of_e = Var oF

and df_e = Var df

let seg_cs = None
and seg_ss = None
and seg_ds = None
and seg_es = None
and seg_fs = Some fs_base
and seg_gs = Some gs_base

let load_s mode s t a =
  let mem_e = ge mode mem in
  match s with
  | None -> Load(mem_e, a, little_endian, t)
  | Some v -> Load(mem_e, Var v +* a, little_endian, t)

module ToIR = struct

  (* stmt helpers *)

  let move v e =
    Move(v, e, [])

  (** Converts from Intel 80-bit bitvector to IEEE 15,64 floating point.
      Ported from Ryan's ssemantics. *)
  let intel80tof ~rm exp =
    let sign = extract 79 79 exp in
    let expon = extract 78 64 exp in
    let integer = extract 63 63 exp in
    let frac = extract 62 0 exp in
    let bitvec15_zero = Int(Big_int_convenience.bi0, Reg 15) in
    let bitvec15_one  = Int(Big_int_convenience.bi1, Reg 15) in
    let bitvec1 = Int(Big_int_convenience.bi1, reg_1) in
    let expon_is_0 = (binop EQ expon bitvec15_zero) in
    let integer_is_1 = (binop EQ integer bitvec1) in
    let ispseudodenorm = (binop AND expon_is_0 integer_is_1) in
    let isvalid = (binop OR expon_is_0 integer_is_1) in
    let xregular = ieeebvtof ~float_size:fp_80_bits @@ X86_legacy_bil_convenience.concat sign (X86_legacy_bil_convenience.concat expon frac) in
    let xinvalid = fnan ~rm ~float_size:fp_80_bits in
    let xpseudo = ieeebvtof ~float_size:fp_80_bits @@ X86_legacy_bil_convenience.concat sign (X86_legacy_bil_convenience.concat bitvec15_one frac) in
    X86_legacy_bil_convenience.exp_ite isvalid (X86_legacy_bil_convenience.exp_ite ispseudodenorm xpseudo xregular) xinvalid

  (** Converts from IEEE 15,64 floating point to Intel 80-bit bitvector. *)
  let ftointel80 ~rm exp =
    let bv = ftoieeebv ~bv_size:79 exp in
    let sign_exp = extract 78 63 bv in
    let frac = extract 62 0 bv in
    let iszero = fiszero ~rm exp in
    let isdenormal = fissub ~rm exp in
    let intzero = binop OR iszero isdenormal in
    let bitvec0 = Int(Big_int_convenience.bi0, reg_1) in
    let bitvec1 = Int(Big_int_convenience.bi1, reg_1) in
    let integer = X86_legacy_bil_convenience.exp_ite intzero bitvec0 bitvec1 in
    X86_legacy_bil_convenience.concat sign_exp (X86_legacy_bil_convenience.concat integer frac)

  (** Convert X87 bitvector (80-bit) to reduced precision version
      based on the contents of the precision-control field. This
      function also does the necessary rounding. *)
  let x87_bv_to_fp_pc ?(rm=RNE) exp =
    let cmp_pc i = (binop EQ (X86_legacy_bil_convenience.it i (Reg 2)) (Var x87_pc)) in
    let convert_00 = intel80tof ~rm exp |> ftof ~rm ~float_size:fp_pc_32_bits |> ftof ~rm ~float_size:fp_80_bits in
    let convert_10 = intel80tof ~rm exp |> ftof ~rm ~float_size:fp_pc_64_bits |> ftof ~rm ~float_size:fp_80_bits in
    let convert_11 = intel80tof ~rm exp in
    (* TODO: should we detect when 0b01 occurs and raise an error? *)
    X86_legacy_bil_convenience.exp_ite (cmp_pc 0b00) convert_00
      (X86_legacy_bil_convenience.exp_ite (cmp_pc 0b10) convert_10 convert_11)

  (** Convert X87 float to 80-bit double-extended precision bitvector. *)
  let x87_fp_to_bv_pc ?(rm=RNE) exp =
    let cmp_pc i = (binop EQ (X86_legacy_bil_convenience.it i (Reg 2)) (Var x87_pc)) in
    let convert_00 = ftof ~rm ~float_size:fp_pc_32_bits exp |> ftof ~rm ~float_size:fp_80_bits |> ftointel80 ~rm in
    let convert_10 = ftof ~rm ~float_size:fp_pc_64_bits exp |> ftof ~rm ~float_size:fp_80_bits |> ftointel80 ~rm in
    let convert_11 = ftof ~rm ~float_size:fp_pc_79_bits exp |> ftointel80 ~rm in
    X86_legacy_bil_convenience.exp_ite (cmp_pc 0b00) convert_00
      (X86_legacy_bil_convenience.exp_ite (cmp_pc 0b10) convert_10 convert_11)

  let get_x87_stack ~index =
    Core_kernel.Array.foldi ~f:(
      fun i acc st_exp ->
        X86_legacy_bil_convenience.exp_ite (x87_top_exp ==* X86_legacy_bil_convenience.it ((i - index) mod 8) (Reg 3)) st_exp acc
    ) ~init:st_exps.(0) st_exps

  let get_x87_tag ~index =
    Core_kernel.Array.foldi ~f:(
      fun i acc st_tag ->
        X86_legacy_bil_convenience.exp_ite (x87_top_exp ==* X86_legacy_bil_convenience.it ((i - index) mod 8) (Reg 3)) st_tag acc
    ) ~init:st_tag_exps.(0) st_tag_exps

  let set_x87_stack ~index value =
    Core_kernel.Array.mapi ~f:(
      fun i st_var ->
        let selector = x87_top_exp ==* X86_legacy_bil_convenience.it ((i - index) mod 8) (Reg 3) in
        let tag =
          X86_legacy_bil_convenience.exp_ite (fisnorm value) fnorm
            (X86_legacy_bil_convenience.exp_ite (fiszero value) fzero fspecial)
        in
        let bv_value = x87_fp_to_bv_pc value in
        [
          move st_tag.(i) (X86_legacy_bil_convenience.exp_ite selector tag st_tag_exps.(i));
          move st_var (X86_legacy_bil_convenience.exp_ite selector bv_value st_exps.(i));
        ]
    ) st |> Array.to_list |> Core_kernel.List.concat

  let empty_x87_stack ~index =
    Core_kernel.Array.mapi ~f:(
      fun i st_tag_var ->
        let selector = x87_top_exp ==* X86_legacy_bil_convenience.it i (Reg 3) in
        move st_tag_var (X86_legacy_bil_convenience.exp_ite selector fempty st_tag_exps.(i))
    ) st_tag |> Array.to_list

  let inc_x87_stack =
    move x87_top (x87_top_exp +* X86_legacy_bil_convenience.it 1 (Reg 3))

  let dec_x87_stack =
    move x87_top (x87_top_exp -* X86_legacy_bil_convenience.it 1 (Reg 3))

  let pop_x87_stack =
    [inc_x87_stack]
    @ empty_x87_stack ~index:0

  let push_x87_stack x =
    set_x87_stack ~index:7 x
    @ [dec_x87_stack]

  type convertable_bv = [ `reg_16 | `reg_32 | `reg_64 | `reg_80 ]

  let bv_to_fp80 typ exp =
    let fp = match typ with
      | `reg_16 -> ieeebvtof ~float_size:fp_16_bits
      | `reg_32 -> ieeebvtof ~float_size:fp_32_bits
      | `reg_64 -> ieeebvtof ~float_size:fp_64_bits
      | `reg_80 -> (fun ?(rm = RNE) -> intel80tof ~rm)
    in
    if typ = `reg_80 then fp exp else
      ftof ~float_size:fp_80_bits (fp exp)

  let fp80_to_bv typ exp =
    match typ with
    | `reg_16 -> ftof ~float_size:fp_16_bits exp |> ftoieeebv ~bv_size:16
    | `reg_32 -> ftof ~float_size:fp_32_bits exp |> ftoieeebv ~bv_size:32
    | `reg_64 -> ftof ~float_size:fp_64_bits exp |> ftoieeebv ~bv_size:64
    | `reg_80 -> ftointel80 ~rm:(RNE) exp


  let store_s mode s t a e =
    let mem = gv mode mem in
    match s with
    | None -> move mem (Store(Var mem, a, e, little_endian, t))
    | Some v -> move mem (Store(Var mem, Var v +* a, e, little_endian, t))

  let _storem mode t a e =
    move mode (Store(Var mode, a, e, little_endian, t))

  (* Double width operands, as used by multiplication and division *)
  let op_dbl = function
    | Reg 8 -> [r16, o_rax]
    | Reg 16 -> [r16, o_rdx; r16, o_rax]
    | Reg 32 -> [r32, o_rdx; r32, o_rax]
    | Reg 64 -> [r64, o_rdx; r64, o_rax]
    | _ -> disfailwith "op_dbl only defined for Reg 8, 16, 32, and 64"


  let reta = [StrAttr "ret"]
  and calla = [StrAttr "call"]

  let compute_sf result = cast_high r1 result
  let compute_zf t result = Int(bi0, t) ==* result
  let compute_pf t r =
    let acc = nt "acc" t in
    let var_acc = Var acc in
    (* extra parens do not change semantics but do make it pretty print nicer *)
    exp_not (cast_low r1
               (Let(acc, (( >>* ) r (it 4 t)) ^* r, Let(acc, (( >>* ) var_acc (it 2 t)) ^* var_acc, (( >>* ) var_acc (it 1 t)) ^* var_acc))))

  let set_sf r = move sf (compute_sf r)
  let set_zf t r = move zf (compute_zf t r)
  let set_pf t r = move pf (compute_pf t r)

  let set_pszf t r =
    [set_pf t r;
     set_sf r;
     set_zf t r]

  (* Adjust flag

     AF is set when there is a carry to or borrow from bit 4 (starting
     at 0), when considering unsigned operands. Let X_i denote bit i of
     value X.  Note that in addition, r_4 = c + [(op1_4 + op2_4) mod 2],
     where c is the carry bit from the lower four bits. Since AF = c,
     and we want to know the value of AF, we can rewrite as AF = c = r_4
     - [(op1_4 + op2_4) mod 2]. Noting that addition and subtraction mod
       2 is just xor, we can simplify to AF = r_4 xor op1_4 xor op2_4.
  *)

  let set_apszf t s1 s2 r =
    let bit4 = it (1 lsl 4) t in
    move af (bit4 ==* (bit4 &* ((r ^* s1) ^* s2)))
    ::set_pszf t r

  (* Helper functions to set flags for adding *)
  let set_aopszf_add t s1 s2 r =
    move oF (cast_high r1 ((s1 =* s2) &* (s1 ^* r)))
    ::set_apszf t s1 s2 r

  let set_flags_add t s1 s2 r =
    move cf (r <* s1)
    ::set_aopszf_add t s1 s2 r

  (* Helper functions to set flags for subtracting *)
  let set_apszf_sub t s1 s2 r = set_apszf t s1 s2 r

  let set_aopszf_sub t s1 s2 r =
    move oF (cast_high r1 ((s1 ^* s2) &* (s1 ^* r)))
    ::set_apszf_sub t s1 s2 r

  let set_flags_sub t s1 s2 r =
    move cf (s2 >* s1)
    ::set_aopszf_sub t s1 s2 r
end (* ToIR *)
