open Core_kernel.Std
open Bap.Std
open Bap_c_type

type ctype = t

let is_const p = p.Spec.qualifier.Qualifier.const
let is_mutable p = not (is_const p)


let rec lvalue (t : ctype) = match t with
  | `Void -> true
  | `Basic t -> is_mutable t
  | `Pointer ({Spec.t} as p) -> is_mutable p || lvalue t
  | `Array ({Spec.t=(t,_)} as p) -> is_mutable p || lvalue t
  | `Function _ -> false
  | `Structure {Spec.t=fs} | `Union {Spec.t=fs} ->
    List.exists fs ~f:(fun (_,t) -> lvalue t)

let arg_intent : ctype -> intent = function
  | `Void -> In
  | `Basic _ -> In
  | `Pointer {Spec.t} when lvalue t -> Both
  | `Array {Spec.t=(e,_)} when lvalue e -> Both
  | `Pointer _ | `Array _ -> In
  | `Function _ -> In
  | `Union _
  | `Structure _ -> In

module Stack = struct
  type direction = [`up | `down]
  let create ?(direction=`down) arch =
    let module Target = (val target_of_arch arch) in
    let sz = (Arch.addr_size arch :> Size.t) in
    let width = Size.in_bits sz in
    let endian = Arch.endian arch in
    let mem = Bil.var Target.CPU.mem in
    let sp = Target.CPU.sp in
    fun off ->
      let off = Word.of_int ~width (off * Size.in_bytes sz) in
      let addr = if Word.is_zero off
        then Bil.(var sp)
        else Bil.(var sp + int off) in
      Bil.load ~mem ~addr endian sz

end
