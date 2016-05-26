open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Bap_c.Std

include Self()

let size = object(self)
  inherit C.Size.base `ILP32
  method! enum s = self#integer `uint
end


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

let mapper gamma = object
  inherit Term.mapper as super
  method! map_sub sub =
    let name = Sub.name sub in
    match gamma name with
    | Some (`Function {C.Type.Spec.t}) ->
      List.fold (args name t) ~init:sub ~f:(Term.append arg_t)
    | _ -> super#map_sub sub
end

module Api = struct
  let language = "c"
  type t = Term.mapper
  let parse get_api intfs =
    let gamma = String.Table.create () in
    List.iter intfs ~f:(fun api ->
        debug "opening %s" api;
        match get_api api with
        | None -> warning "Can't open interface: %s" api
        | Some file ->
          debug "parsing %s" api;
          match C.Parser.run file with
          | Error e ->
            warning "Failed to parse api `%s': %a" api Error.pp e
          | Ok api ->
            List.iter api ~f:(fun (key,t) ->
                Hashtbl.set gamma ~key ~data:t));
    Ok (mapper (Hashtbl.find gamma))
  let mapper = ident
end

let setup () =
  let id = ref None in
  Stream.observe  Project.Info.arch (function
      | #Arch.arm ->
        Option.iter !id ~f:Bap_api.retract;
        let api : Bap_api.t = (module Api) in
        info "using armeabi ABI";
        id := Some (Bap_api.process api)
      | _ -> Option.iter !id ~f:Bap_api.retract)

let () = setup ()
