(** Basic register types of all supported X86 registers. *)

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

(** 64-bit GPR registers *)
type r64 = [
  | `RAX | `RBX | `RCX | `RDX
  | `RDI | `RSI | `RBP | `RSP
  | `R8 | `R9 | `R10 | `R11
  | `R12 | `R13 | `R14 | `R15
] [@@deriving sexp]

(** SSE register types *)
type r128 = [
  | `XMM0
  | `XMM1
  | `XMM2
  | `XMM3
  | `XMM4
  | `XMM5
  | `XMM6
  | `XMM7
  | `XMM8
  | `XMM9
  | `XMM10
  | `XMM11
  | `XMM12
  | `XMM13
  | `XMM14
  | `XMM15
] [@@deriving sexp]

type r256 = [
  | `YMM0
  | `YMM1
  | `YMM2
  | `YMM3
  | `YMM4
  | `YMM5
  | `YMM6
  | `YMM7
  | `YMM8
  | `YMM9
  | `YMM10
  | `YMM11
  | `YMM12
  | `YMM13
  | `YMM14
  | `YMM15
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

type t = [
  | gpr
  | ip
  | segment
  | segment_base
] [@@deriving sexp]
