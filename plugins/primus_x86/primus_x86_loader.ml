open Core_kernel
open Bap.Std
open Bap_primus.Std
open X86_cpu

module Component(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Interpreter = Primus.Interpreter.Make(Machine)

  let zero = Primus.Generator.static 0

  let initialize_flags flags =
    Machine.Seq.iter (Set.to_sequence flags) ~f:(fun reg ->
        Env.add reg zero)

  let correct_sp sp addend _ =
    Env.get sp >>= fun x ->
    Interpreter.binop Bil.plus x addend >>= Env.set sp

  let correct_sp sp addend =
    match Var.typ sp with
    | Mem _ -> Machine.return ()
    | Imm width ->
       Value.of_int ~width addend >>= fun addend ->
       Primus.Linker.Trace.lisp_call_return >>> correct_sp sp addend

  let seq = Machine.sequence

  let init () =
    Machine.get () >>= fun proj ->
    match Project.arch proj with
    | `x86 ->
       seq [initialize_flags IA32.flags; correct_sp IA32.sp 4]
    | `x86_64 ->
       seq [initialize_flags AMD64.flags; correct_sp AMD64.sp 8]
    | _ -> Machine.return ()

end
