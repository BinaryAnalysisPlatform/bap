open Core_kernel.Std
open Bap_types.Std

module BV = Bitvector
module Memory = Bap_memory

type binopf = Exp.t -> Exp.t -> Exp.t

type mode = X86 | X8664

type order = Low | High

type direction = Forward | Backward

type operand =
  | Oreg of int
  | Ovec of int
  | Oseg of int
  | Oaddr of Exp.t
  | Oimm of addr

type jumptarget =
  | Jabs of operand
  | Jrel of addr * addr (* next ins address, offset *)

(* See section 4.1 of the Intel® 64 and IA-32 Architectures Software
   Developer’s Manual, Volumes 2A & 2B: Instruction Set Reference
   (order numbers 253666 and 253667) *)
module Pcmpstr = struct

  type ssize = Bytes | Words

  type ssign = Signed | Unsigned

  type agg = EqualAny | Ranges | EqualEach | EqualOrdered

  type outselectsig = LSB | MSB (* For PCMPESTRI/PCMPISTRI, choosees LSB or MSB.  *)

  type outselectmask = Bitmask | Bytemask (* For PCMPESTRM/PCMPISTRM, represents bit mask/word mask. *)

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

type cast_type = Bap_types.Std.cast

type opcode =
  | Bswap of (typ * operand)
  | Retn of ((typ * operand) option) * bool (* bytes to release, far/near ret *)
  | Nop
  | Mov of typ * operand * operand * (Exp.t option) (* dst, src, condition *)
  | Movs of typ
  | Movzx of typ * operand * typ * operand (* dsttyp, dst, srctyp, src *)
  | Movsx of typ * operand * typ * operand (* dsttyp, dst, srctyp, src *)
  | Movdq of typ * operand * typ * operand * bool (* dst type, dst op, src type, src op, aligned *)
  | Movoffset of (typ * operand) * offsetinfo list
  (* dest type, dest, (src copy length, src type, src, src src offset, src dest offset)* *)
  | Lea of typ * operand * Exp.t
  | Call of operand * addr (* addr is RA *)
  | Shift of binop * typ * operand * operand
  | Shiftd of binop * typ * operand * operand * operand
  | Rotate of binop * typ * operand * operand * bool (* left or right, type, src/dest op, shift op, use carry flag *)
  | Bt of typ * operand * operand
  | Bs of typ * operand * operand * direction
  | Jump of jumptarget
  | Jcc of jumptarget * Exp.t
  | Setcc of typ * operand * Exp.t
  | Hlt
  | Cmps of typ
  | Scas of typ
  | Stos of typ
  | Push of typ * operand
  | Pop of typ * operand
  | Pushf of typ
  | Popf of typ
  | Popcnt of typ * operand * operand (* size, src, dest *)
  | Sahf
  | Lahf
  | Add of (typ * operand * operand)
  | Adc of (typ * operand * operand)
  | Inc of typ * operand
  | Dec of typ * operand
  | Sub of (typ * operand * operand)
  | Sbb of (typ * operand * operand)
  | Cmp of (typ * operand * operand)
  | Cmpxchg of (typ * operand * operand)
  | Cmpxchg8b of operand
  | Xadd of (typ * operand * operand)
  | Xchg of (typ * operand * operand)
  | And of (typ * operand * operand)
  | Or of (typ * operand * operand)
  | Xor of (typ * operand * operand)
  | Test of (typ * operand * operand)
  | Ptest of (typ * operand * operand)
  | Not of (typ * operand)
  | Neg of (typ * operand)
  | Mul of (typ * operand) (* typ, src *)
  | Imul of typ * (bool * operand) * operand * operand (* typ, (true if one operand form, dst operand), src1, src2 *)
  | Div of typ * operand (* typ, src *)
  | Idiv of typ * operand (* typ, src *)
  | Cld
  | Rdtsc
  | Cpuid
  | Xgetbv
  | Stmxcsr of operand
  | Ldmxcsr of operand
  | Fnstcw of operand
  | Fldcw of operand
  | Fld of operand
  | Fst of (operand * bool)
  | Punpck of (typ * typ * order * operand * operand * operand option) (* dest size, element size, low/high elements, dest, src, optional VEX src *)
  | Ppackedbinop of (typ * typ * binopf * string * operand * operand * operand option) (* Perform a generic packed binary operation. dest size, element size, binop, assembly string, dest, src, optional VEX src *)
  | Pbinop of (typ * binopf * string * operand * operand * operand option)
  | Pmov of (typ * typ * typ * operand * operand * cast_type * string) (* Packed move. dest size, dest elt size, src elt size, dest, src, ext(signed/zero), name *)
  | Pmovmskb of (typ * operand * operand)
  | Pcmp of (typ * typ * binop * string * operand * operand * operand option)
  | Palignr of (typ * operand * operand * operand option * operand)
  | Pcmpstr of (typ * operand * operand * operand * Pcmpstr.imm8cb * Pcmpstr.pcmpinfo)
  | Pshufb of typ * operand * operand * operand option
  | Pshufd of typ * operand * operand * operand option * operand
  | Leave of typ
  | Interrupt of operand
  | Interrupt3 (* Trap to debugger *)
  | Sysenter
  | Syscall

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
