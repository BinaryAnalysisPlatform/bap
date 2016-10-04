open Bap.Std

type t = [
  | `DE (* Divide-by-Zero-Error *)
  | `DB (* Debug *)
  | `NMI (* Non-Maskable-Interrupt *)
  | `BP (* Breakpoint *)
  | `OF (* Overflow *)
  | `BR (* Bound-Range *)
  | `UD (* Invalid-Opcode *)
  | `NM (* Device-Not-Available *)
  | `DF (* Double-Fault *)
  | `TS (* Invalid-TSS *)
  | `NP (* Segment-Not-Present *)
  | `SS (* Stack *)
  | `GP (* General-Protection *)
  | `PF (* Page-Fault *)
  | `MF (* x87 Floating-Point Exception-Pending *)
  | `AC (* Alignment-Check *)
  | `MC (* Machine-Check *)
  | `XF (* SIMD Floating-Point *)
  | `SX (* Security Exception *)
  | `INTR of int (* External Interrupts (Maskable) *)
  | `SOFTWARE of int (* Software Interrupts *)
]

let to_int = function
  | `DE -> 0
  | `DB -> 1
  | `NMI -> 2
  | `BP -> 3
  | `OF -> 4
  | `BR -> 5
  | `UD -> 6
  | `NM -> 7
  | `DF -> 8
  | `TS -> 10
  | `NP -> 11
  | `SS -> 12
  | `GP -> 13
  | `PF -> 14
  | `MF -> 16
  | `AC -> 17
  | `MC -> 18
  | `XF -> 19
  | `SX -> 30
  | `INTR v -> v
  | `SOFTWARE v -> v

