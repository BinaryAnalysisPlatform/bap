open Bap_main
open Bap_core_theory

module AT = Bap_avr_target

module Abi = struct
  open Bap_c.Std

  module Arg = C.Abi.Arg
  open Arg.Let
  open Arg.Syntax

  let model = new C.Size.base `LP32

  let define t =
    C.Abi.define t model @@ fun _ {C.Type.Proto.return=r; args} ->
    let* iregs = Arg.Arena.create AT.Gcc.args in
    let* irets = Arg.Arena.create AT.Gcc.rets in
    let pass_via regs (_,t) =
      let open Arg in
      choice [
        sequence [
          align_even regs;
          registers regs t;
        ];
        sequence [
          deplet regs;
          memory t;
        ]
      ] in
    let args = Arg.List.iter args ~f:(pass_via iregs) in
    let return = match r with
      | `Void -> None
      | r -> Some (pass_via irets ("",r)) in
    Arg.define ?return args

  let setup () =
    List.iter define AT.Gcc.[tiny; mega; xmega]
end

let main _ctxt =
  AT.load ();
  Abi.setup ();
  Ok ()

let () = Bap_main.Extension.declare main
    ~provides:["avr"; "lifter"; "disassembler"]
