open Bap.Std

type t = X86_tools_types.interrupt_vector

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

