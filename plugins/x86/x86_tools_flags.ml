open Core_kernel
open Bap.Std
open X86_tools_types
  
module Make(CPU : X86CPU) : FR = struct
  type t = cpu_flag 

  let var = function
    | `CF -> CPU.cf
    | `ZF -> CPU.zf
    | `SF -> CPU.sf
    | `OF -> CPU.oF
    | `PF -> CPU.pf
    | `AF -> CPU.af
    | `DF -> CPU.df

  let get t = var t |> Bil.var
  let set t exp =
    let f = var t in
    Bil.(f := exp)

  let set_unknown t s = var t |> Var.typ |> Bil.unknown s |> set t

  let parity width result =
    let i value = Word.of_int ~width value |> Bil.int in
    let v2 = Type.imm width |>
             Var.create ~is_virtual:true ~fresh:true "v" in
    let open Bil in
    lnot (cast low 1 (let_ v2 (result lsr (i 4) lxor result)
                        (let_ v2 (var v2 lsr (i 2) lxor var v2)
                           (var v2 lsr (i 1) lxor var v2))))

  let auxiliary_carry result op1 op2 =
    let open Bil in
    extract ~hi:4 ~lo:4 (result lxor op1 lxor op2)

  let sub =
    let carry _diff op1 op2 =
      let open Bil in
      op1 < op2 in
    let overflow diff op1 op2 =
      let open Bil in
      cast high 1 ((op1 lxor op2) land (op1 lxor diff)) in
    carry, overflow
    
  let add =
    let carry sum op1 op2 =
      let open Bil in
      sum < op2 in
    let overflow sum op1 op2 =
      let open Bil in
      cast high 1 (lnot (op1 lxor op2) land (op2 lxor sum)) in
    carry, overflow

  let after_insn (carry, overflow)  ~result size ~op1 ~op2 =
    let size = Size.in_bits size in
    List.map ~f:(fun (t, exp) -> set t exp)
      [ `CF, carry result op1 op2;
        `OF, overflow result op1 op2;
        `AF, auxiliary_carry result op1 op2;
        `PF, parity size result;
        `SF, Bil.(cast high 1 result);
        `ZF, Bil.(int (Word.zero size) = result)]

  let after_sub ~diff = after_insn sub ~result:diff
  let after_add ~sum = after_insn add ~result:sum
end
