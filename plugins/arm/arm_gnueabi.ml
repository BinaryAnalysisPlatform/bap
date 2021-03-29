open Core_kernel
open Bap.Std
open Bap_c.Std

include Self()

let size = object(self)
  inherit C.Size.base `ILP32
  method! enum _ = self#integer `uint
end


let nats = Seq.unfold ~init:0 ~f:(fun n -> Some (n,n+1))
let regs = ARM.CPU.[r0;r1;r2;r3] |> List.map ~f:Bil.var
let mems = Seq.map nats ~f:(C.Abi.Stack.create `armv7)

let align ncrn t =
  if Size.equal (size#alignment t) `r64 then match ncrn with
    | [_;_;_;_] -> ncrn
    | [_;r2;r3] -> [r2;r3]
    | _ -> []
  else ncrn

let concat = List.reduce_exn ~f:Bil.concat

let retregs = function
  | #C.Type.scalar as t ->
    List.take regs (Size.in_bytes (size#scalar t) / 4), [], regs
  | t -> match size#bits (t :> C.Type.t) with
    | Some sz when sz <= 32 -> List.take regs 1,[],regs
    | _ -> List.take regs 1, List.take regs 1, List.tl_exn regs

let ret : C.Type.t -> 'a = function
  | `Void -> None,[],regs
  | t -> match retregs t with
    | [],_,rest -> None,[],rest
    | rets,hids,rest ->
      let data = C.Abi.data size t in
      Some (data, concat rets),
      List.map hids ~f:(fun reg -> t,(C.Data.Ptr data, reg)),
      rest

let args _ _ {C.Type.Proto.return; args=ps} =
  let return,hidden,regs = ret return in
  let _,_,params =
    List.fold ps ~init:(regs,mems,[]) ~f:(fun (regs,mems,args) (_,t) ->
        let words = Option.value (size#bits t) ~default:32 / 32 in
        let exps,regs = List.split_n (align regs t) words in
        let rest,mems = Seq.split_n mems (words - List.length exps) in
        regs,mems, (C.Abi.data size t, (concat (exps@rest))) :: args) in
  Some C.Abi.{return; hidden; params = List.rev params}

let abi = C.Abi.{
    insert_args = args;
    apply_attrs = fun _ -> ident
  }

let api arch = C.Abi.create_api_processor arch abi

let main proj = match Project.arch proj with
  | #Arch.arm ->
    info "using armeabi ABI";
    C.Abi.register "eabi" abi;
    Bap_api.process (api size);
    Project.set proj Bap_abi.name "eabi"
  | _ -> proj

module Aapcs64 = struct
  open Bap_core_theory
  open Bap_c.Std
  open Bap.Std
  open Monads.Std
  open Monad.Option.Syntax
  open Monad.Option.Let

  let name = "aapcs64"            (* is there an official name? *)

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
    if Theory.Target.belongs Arm_target.LE.v8a t then
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

let setup () =
  Bap_abi.register_pass main;
  Bap_abi.register_pass Aapcs64.apply_headers;
