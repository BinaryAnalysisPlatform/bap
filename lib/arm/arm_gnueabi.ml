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
    List.take regs (Size.in_bytes (size#scalar t) / 4), [], regs
  | non_fundamental -> match size#bits non_fundamental with
    | Some sz when sz <= 32 -> List.take regs 1,[],regs
    | _ -> List.take regs 1, List.take regs 1, List.tl_exn regs

(* let size_value t = match size#bits t with *)
(*   | None -> C.Data.Top *)
(*   | Some sz -> *)



let ret sub = function
  | `Void -> None,[],regs
  | other as t -> match retregs t with
    | [],_,rest -> None,[],rest
    | rets,hids,rest ->
      assert false
(* let rets,hids,rest = retregs t in *)
(* [arg sub "result" int exps], regs *)

let args sub {C.Type.Proto.return; args=ps} =
  let this,regs = ret sub return in
  let _,_,args =
    List.fold ps ~init:(regs,mems,[]) ~f:(fun (regs,mems,args) (n,t) ->
        let words = Option.value (size#bits t) ~default:32 / 32 in
        let exps,regs = List.split_n (align regs t) words in
        let rest,mems = Seq.split_n mems (words - List.length exps) in
        regs,mems,arg sub n (C.Abi.arg_intent t) (exps@rest)::args) in
  List.rev (this@args)

let api = C.Abi.create_api_processor args

let setup () =
  let id = ref None in
  Stream.observe  Project.Info.arch (function
      | #Arch.arm ->
        Option.iter !id ~f:Bap_api.retract;
        info "using armeabi ABI";
        id := Some (Bap_api.process api)
      | _ -> Option.iter !id ~f:Bap_api.retract)

let () = setup ()
