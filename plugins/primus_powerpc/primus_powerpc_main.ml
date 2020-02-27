open Core_kernel
open Bap.Std
open Bap_primus.Std

include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P
      "Performs the PowerPC target specific setup. So far it just initializes
  all flags and CTR register to zero."
  ]

let () = Config.when_ready @@ fun _ ->
  let module Component(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Env = Primus.Env.Make(Machine)
    module Value = Primus.Value.Make(Machine)

    let initialize_regs regs =
      let zero = Primus.Generator.static 0 in
      Machine.List.iter regs ~f:(fun (r,s) ->
          let r = Var.create r (Type.imm s) in
          Env.add r zero)

    let init32 vars =
      initialize_regs vars

    let init () =
      Machine.get () >>= fun proj ->
      match Project.arch proj with
      | `ppc ->
        let regs = [
          "CTR", 32;
          "SO", 1;
          "CA", 1;
          "OV", 1;
          "CR0LT", 1;
          "CR0GT", 1;
          "CR0EQ", 1;
          "CA32", 1;
          "OV32", 1;
        ] in
        init32 regs
      | _ -> Machine.return ()

  end in
  Primus.Components.register_generic "init" (module Component)
    ~package:"powerpc"
    ~desc:"initializes PPC registers"
