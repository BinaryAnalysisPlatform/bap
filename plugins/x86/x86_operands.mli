open Core_kernel.Std
open Bap.Std

val r : f:(mem -> reg -> bil Or_error.t) -> lifter
val i : f:(mem -> imm -> bil Or_error.t) -> lifter
val m : f:(mem -> seg:reg -> base:reg -> scale:imm -> index:reg -> disp:imm -> bil Or_error.t) -> lifter

val rr : f:(mem -> reg -> reg -> bil Or_error.t) -> lifter
val ri : f:(mem -> reg -> imm -> bil Or_error.t ) -> lifter
val rm : f:(mem -> reg -> seg:reg -> base:reg -> scale:imm -> index:reg -> disp:imm -> bil Or_error.t) -> lifter
val mr : f:(mem -> seg:reg -> base:reg -> scale:imm -> index:reg -> disp:imm -> reg -> bil Or_error.t) -> lifter
val mi : f:(mem -> seg:reg -> base:reg -> scale:imm -> index:reg -> disp:imm -> imm -> bil Or_error.t) -> lifter

val rrr : f:(mem -> reg -> reg -> reg -> bil Or_error.t) -> lifter
val rri : f:(mem -> reg -> reg -> imm -> bil Or_error.t) -> lifter
val rrm : f:(mem -> reg -> reg -> seg:reg -> base:reg -> scale:imm -> index:reg -> disp:imm -> bil Or_error.t) -> lifter
