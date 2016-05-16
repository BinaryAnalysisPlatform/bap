open Core_kernel.Std
open Bap.Std
open Bap_c.Std

let size = object(self)
  inherit C.Size.base `ILP32
  method! enum s = self#integer `uint
end


(**
   Result Return
   -------------

   The manner in which a result is returned from a function is
   determined by the type of that result.

   * A Half-precision Floating Point Type is converted to single
     precision and returned in r0.

   * A Fundamental Data Type that is smaller than 4 bytes is zero- or
     sign-extended to a word and returned in r0.

   * A word-sized Fundamental Data Type (e.g., int, float) is returned
     in r0.

   * A double-word sized Fundamental Data Type (e.g., long long,
   double and 64-bit containerized vectors) is returned in r0 and r1.

   * A 128-bit containerized vector is returned in r0-r3.

   * A Composite Type not larger than 4 bytes is returned in r0. The
   format is as if the result had been stored in memory at a
   word-aligned address and then loaded into r0 with an LDR
   instruction. Any bits in r0 that lie outside the bounds of the
   result have unspecified values.

   * A Composite Type larger than 4 bytes, or whose size cannot be
   determined statically by both caller and callee, is stored in
   memory at an address passed as an extra argument when the function
   was called.


   Parameter Passing
   -----------------

   Stage A – Initialization
   ========================

   This stage is performed exactly once, before processing of the
   arguments commences.

   A.1 The Next Core Register Number (NCRN) is set to r0.

   A.2.cp Co-processor argument register initialization is performed.

   A.3 The next stacked argument address (NSAA) is set to the current
   stack-pointer value (SP).

   A.4 If the subroutine is a function that returns a result in
   memory, then the address for the result is placed in r0 and the
   NCRN is set to r1.

   Stage B – Pre-padding and extension of arguments
   ================================================

   For each argument in the list the first matching rule from the
   following list is applied.

   B.1 If the argument is a Composite Type whose size cannot be
   statically determined by both the caller and callee, the argument
   is copied to memory and the argument is replaced by a pointer to
   the copy.

   B.2 If the argument is an integral Fundamental Data Type that is
   smaller than a word, then it is zero- or sign-extended to a full
   word and its size is set to 4 bytes. If the argument is a
   Half-precision Floating Point Type it is converted to Single
   Precision.

   B.3.cp If the argument is a CPRC then any preparation rules for
   that co-processor register class are applied.

   B.4 If the argument is a Composite Type whose size is not a multiple
   of 4 bytes, then its size is rounded up to the nearest multiple of
   4.



   Stage C – Assignment of arguments to registers and stack
   ========================================================

   For each argument in the list the following rules are applied in
   turn until the argument has been allocated.

   C.1.cp If the argument is a CPRC and there are sufficient
   unallocated co-processor registers of the appropriate class, the
   argument is allocated to co-processor registers.

   C.2.cp If the argument is a CPRC then any co-processor registers in
   that class that are unallocated are marked as unavailable. The NSAA
   is adjusted upwards until it is correctly aligned for the argument
   and the argument is copied to the memory at the adjusted NSAA. The
   NSAA is further incremented by the size of the argument. The
   argument has now been allocated.

   C.3 If the argument requires double-word alignment (8-byte), the
   NCRN is rounded up to the next even register number.

   C.4 If the size in words of the argument is not more than r4 minus
   NCRN, the argument is copied into core registers, starting at the
   NCRN. The NCRN is incremented by the number of registers used.
   Successive registers hold the parts of the argument they would hold
   if its value were loaded into those registers from memory using an
   LDM instruction. The argument has now been allocated.

   C.5 If the NCRN is less than r4 and the NSAA is equal to the SP,
   the argument is split between core registers and the stack. The
   first part of the argument is copied into the core registers
   starting at the NCRN up to and including r3. The remainder of the
   argument is copied onto the stack, starting at the NSAA. The NCRN
   is set to r4 and the NSAA is incremented by the size of the
   argument minus the amount passed in registers. The argument has now
   been allocated.

   C.6 The NCRN is set to r4.

   C.7 If the argument required double-word alignment (8-byte), then
   the NSAA is rounded up to the next double-word address.

   C.8 The argument is copied to memory at the NSAA. The NSAA is
   incremented by the size of the argument.
*)

let nats = Seq.unfold ~init:0 ~f:(fun n -> Some (n,n+1))
let regs = ARM.CPU.[r0;r1;r2;r3] |> List.map ~f:Bil.var
let mems = Seq.map nats ~f:(C.Abi.Stack.create `armv7)

let align ncrn t =
  if size#alignment t = 64 then match ncrn with
    | [_;_;_;_] -> ncrn
    | [_;r2;r3] -> [r2;r3]
    | _ -> []
  else ncrn


let arg sub n int exps =
  let exp = List.reduce_exn exps ~f:Bil.concat in
  let typ = Type.imm (List.length exps * 32) in
  let var = Var.create (sub ^ "_" ^ n) typ in
  Arg.create ~intent:int var exp

let retregs = function
  | #C.Type.scalar as t ->
    List.take regs (Size.in_bytes (size#scalar t) / 4), Out, regs
  | non_fundamental -> match size#bits non_fundamental with
    | Some sz when sz <= 32 -> List.take regs 1,Out,regs
    | _ -> List.take regs 1, Both, List.tl_exn regs

let ret sub = function
  | `Void -> [],regs
  | other as t ->
    let exps,int,regs = retregs t in
    [arg sub "result" int exps], regs

let args sub {C.Type.Proto.return; args=ps} =
  let this,regs = ret sub return in
  let _,_,args =
    List.fold ps ~init:(regs,mems,[]) ~f:(fun (regs,mems,args) (n,t) ->
        let words = Option.value (size#bits t) ~default:32 / 32 in
        let exps,regs = List.split_n (align regs t) words in
        let rest,mems = Seq.split_n mems (words - List.length exps) in
        regs,mems,arg sub n (C.Abi.arg_intent t) (exps@rest)::args) in
  List.rev (this@args)
