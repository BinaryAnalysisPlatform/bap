open Core_kernel[@@warning "-D"]
open Bap_main
open Bap.Std
open Bap_core_theory
open KB.Syntax
module CT = Theory

include Bap_main.Loggers()

module Target = Bap_msp430_target
module Dis = Disasm_expert.Basic

type Extension.Error.t += Unknown_backend of string

let provides = [
  "semantics";
  "lifter";
  "msp430";
]

let use_llvm_decoding () =
  KB.promise CT.Label.encoding @@ fun label ->
  CT.Label.target label >>| fun t ->
  if CT.Target.belongs Target.msp430 t
  then Target.llvm16
  else CT.Language.unknown


let enable_llvm () =
  Dis.register Target.llvm16 @@ fun _ ->
  Dis.create (*TODO - find these out ~attrs:"+a,+c,+d,+m"*) ~backend:"llvm" "msp430"

(*
let pcode = Theory.Language.declare ~package:"bap" "pcode-msp430"

let enable_pcode () =
  Dis.register pcode @@begin fun t ->
    Dis.create ~backend:"ghidra" @@ sprintf "msp430:LE:%d:default"
      (Theory.Target.bits t)
  end;
  KB.promise Theory.Label.encoding @@begin fun label ->
    Theory.Label.target label >>| fun t ->
    if Theory.Target.belongs Target.parent t
    then pcode
    else Theory.Language.unknown
  end
*)

let enable_loader () =
  let request_arch doc =
    let open Ogre.Syntax in
    match Ogre.eval (Ogre.request Image.Scheme.arch) doc with
    | Error _ -> assert false
    | Ok arch -> arch in
  KB.promise CT.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>| request_arch >>| function
  | Some "msp430" -> Target.msp430
  | _ -> CT.Target.unknown

module Abi = struct
  open Bap_c.Std
  open Bap.Std

  module Arg = C.Abi.Arg
  open Arg.Let
  open Arg.Syntax

  let is_floating = function
    | `Basic {C.Type.Spec.t=#C.Type.real} -> true
    | _ -> false

  let data_model t =
    let bits = Theory.Target.bits t in
    new C.Size.base (if bits = 32 then `ILP32 else `LP64)

  let define t =
    let model = data_model t in
    C.Abi.define t model @@ fun _ {C.Type.Proto.return=r; args} ->
    let* iargs = Arg.Arena.iargs t in
    let* irets = Arg.Arena.irets t in
    let* fargs = Arg.Arena.fargs t in
    let* frets = Arg.Arena.frets t in

    (* integer calling convention *)
    let integer regs t =
      Arg.count regs t >>= function
      | None -> Arg.reject ()
      | Some 1 -> Arg.choice [
          Arg.register regs t;
          Arg.memory t;
        ]
      | Some 2 -> Arg.choice [
          Arg.sequence [
            Arg.align_even regs;
            Arg.registers ~limit:2 regs t;
          ];
          Arg.split_with_memory regs t;
          Arg.memory t;
        ]
      | Some _ -> Arg.reference regs t in

    (* floating-point calling convention *)
    let float iregs fregs t =
      Arg.count fregs t >>= function
      | Some 1 -> Arg.choice [
          Arg.register fregs t;
          Arg.register iregs t;
          Arg.memory t;
        ]
      | _ -> integer iregs t in

    let arg iregs fregs r =
      if is_floating r
      then float iregs fregs r
      else integer iregs r in

    Arg.define ?return:(match r with
        | `Void -> None
        | r -> Some (arg irets frets r))
      (Arg.List.iter args ~f:(fun (_,t) ->
           arg iargs fargs t));
end


let backend =
  let open Extension in
  Configuration.parameter Type.(some string) "backend"

let main ctxt =
  enable_loader ();
  Abi.define Target.msp430;
  match Extension.Configuration.get ctxt backend with
  | Some "llvm" | None ->
    use_llvm_decoding ();
    enable_llvm ();
    Ok ()
(*| Some "ghidra" ->
    enable_pcode ();
    Ok ()*)
  | Some s -> Error (Unknown_backend s)

let () = Bap_main.Extension.declare main
    ~doc:"provides msp430 semantics"
    ~provides