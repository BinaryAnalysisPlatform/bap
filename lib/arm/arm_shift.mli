open Core_kernel.Std
open Bap.Std
open Arm_types

(** Need the operand and the carry flag value src - the source, if you
    intend to use the carry bit, this must not be the destination of the
    shift expression.  This means it must be a temp that contains the
    value of the source not the actual source itself shift_type - the type
    of shift shift - must be a exp that is the amount of the shift
    (ignored for rrx)
*)
val lift_c : src:exp -> shift -> shift:exp -> typ -> exp * exp


(** decodes a register shifted operand
 * src - the operand to be shifted, cannot be the destination
 *        in practice this means it must be a temp variable.
 * shift_type - an int64, bits 2 through 0 represent the shift type
 *              valid shift types are number 1 through 5
 * shift - the value to shift by
 * t - the type
 **)

val lift_r : src:exp -> op -> shift:exp -> typ -> exp * exp


(** decodes an immediate shifted operand
 * src - the operand to be shifted, cannot be the destination
 *        in practice this means it must be a temp variable.
 * shift_type - an int64, bits 2 through 0 represent the shift type
 *              valid shift types are number 1 through 5
 *              bits 3 and higher represent the shift amount if this is an
 *              immediate shift. For register shifts these upper bits are 0.
 *              If the shift type is RRX, a shift amount of 1 is implied.
 * t - the type
 **)
val lift_i : src:exp -> op -> typ -> exp * exp


(** decodes a shifted operand for a memory operation
 * src - the operand to be shifted
 * shift - an int64,
 *            bits 11 through 0 represent the shift amount
 *            bits 12 represents whether the expression is added or subtracted
 *            bits 15 through 13 represent the shift type, valid shift types
 *              are number 1 through 5
 * typ - the type
 **)
val lift_mem : src:exp -> op -> typ -> exp
