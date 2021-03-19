open Base
open Bap_main
open Bap.Std
open Bap_core_theory
open KB.Syntax
module CT = Theory

include Bap_main.Loggers()

module Target = Bap_riscv_target
module Dis = Disasm_expert.Basic

let provides = [
  "riscv";
  "riscv64";
  "riscv32";
]


let provide_decoding () =
  KB.promise CT.Label.encoding @@ fun label ->
  CT.Label.target label >>| fun t ->
  if CT.Target.belongs Target.parent t
  then if Theory.Target.belongs Target.riscv64 t
    then Target.llvm64
    else Target.llvm32
  else CT.Language.unknown


let enable_llvm encoding triple =
  Dis.register encoding @@ fun _ ->
  Dis.create ~attrs:"+a,+c,+d,+m" ~backend:"llvm" triple

let enable_loader () =
  let request_arch doc =
    let open Ogre.Syntax in
    match Ogre.eval (Ogre.request Image.Scheme.arch) doc with
    | Error _ -> assert false
    | Ok arch -> arch in
  KB.promise CT.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>| request_arch >>| function
  | Some "riscv64" -> Target.riscv64
  | Some "riscv32" -> Target.riscv32
  | _ -> CT.Target.unknown

module Abi = struct
  open Bap_c.Std
  open Bap.Std
  open Monads.Std
  open Monad.Option.Syntax
  open Monad.Option.Let

  let name = "riscv"            (* is there an official name? *)

  let (.:()) file num = match Set.nth file num with
    | None -> failwith "a wrong number of registers"
    | Some v -> Var.reify v

  let (.%()) file num = Bil.var file.:(num)

  let is_floating = function
    | `Basic {C.Type.Spec.t=#C.Type.real} -> true
    | _ -> false

  (* even x = x if x is even otherwise x+1 *)
  let even x = x + x land 1

  let data_model t =
    let bits = Theory.Target.bits t in
    new C.Size.base (if bits = 32 then `ILP32 else `LP64)

  let insert_args t _sub _attrs {C.Type.Proto.return; args} =
    let bits = Theory.Target.bits t in
    let a = Theory.Target.regs t ~roles:Theory.Role.Register.[
        integer; function_argument;
      ] in
    let fa = Theory.Target.regs t ~roles:Theory.Role.Register.[
        floating; function_argument;
      ] in
    let regs = Set.length a in
    let mem = Bil.var @@ Var.reify @@ Theory.Target.data t in
    let* sp = Theory.Target.reg t Theory.Role.Register.stack_pointer >>| Var.reify in
    let size = data_model t in
    let stack t n =
      size#bits t >>= Size.of_int_opt >>| fun sz ->
      C.Abi.data size t,
      Bil.load ~mem LittleEndian sz
        ~addr:Bil.(var sp + int (Word.of_int ~width:bits n)) in
    let param t n =
      if n > regs then stack t (n - regs)
      else
        size#bits t >>= fun s ->
        Monad.Option.guard (s <= 2 * bits) >>| fun () ->
        C.Abi.data size t, match is_floating return,s <= bits with
        | true,true -> fa.%(n)
        | true,false -> Bil.concat fa.%(even n) fa.%(even n + 1)
        | false,true -> a.%(n)
        | false,false -> Bil.concat a.%(even n) a.%(even n + 1) in
    let return = param return 0 in
    let+ (_,params) = Monad.Option.List.fold args
        ~init:(0,[]) ~f:(fun (used,pars) (_name,arg) ->
            size#bits arg >>= fun argsz ->
            param arg used >>| fun par ->
            let used = if argsz <= bits then used + 1
              else used + 2 + used land 1 in
            used,par::pars) in
    {C.Abi.return; params = List.rev params; hidden=[]}

  let apply_headers proj =
    let t = Project.target proj in
    if Theory.Target.belongs Target.parent t then
      let abi = C.Abi.{
          insert_args = insert_args t;
          apply_attrs = fun _ x -> x;
        } in
      C.Abi.register name abi;
      let size = data_model t in
      let apply_headers = C.Abi.create_api_processor size abi in
      Bap_api.process apply_headers;
      Project.set proj Bap_abi.name name
    else proj
end


let main _ctxt =
  enable_llvm Target.llvm64 "riscv64";
  enable_llvm Target.llvm32 "riscv32";
  enable_loader ();
  provide_decoding ();
  Bap_abi.register_pass Abi.apply_headers;
  Ok ()

let () = Bap_main.Extension.declare main
    ~provides
