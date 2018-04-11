open Core_kernel
open Bap.Std
open Bap_primus.Std

module Component(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Env = Primus.Env.Make(Machine)

  let zero = Primus.Generator.static 0

  let initialize_flags flags =
    Machine.Seq.iter (Set.to_sequence flags) ~f:(fun reg ->
        Env.add reg zero)

  let init () =
    Machine.get () >>= fun proj ->
    match Project.arch proj with
    | `x86 -> initialize_flags (X86_cpu.IA32.flags)
    | `x86_64 -> initialize_flags (X86_cpu.AMD64.flags)
    | _ -> Machine.return ()

end
