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
  if size#alignment t = `r64 then match ncrn with
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
  | other as t -> match retregs t with
    | [],_,rest -> None,[],rest
    | rets,hids,rest ->
      let data = C.Abi.data size t in
      Some (data, concat rets),
      List.map hids ~f:(fun reg -> t,(C.Data.Ptr data, reg)),
      rest

let args {C.Type.Proto.return; args=ps} =
  let return,hidden,regs = ret return in
  let _,_,params =
    List.fold ps ~init:(regs,mems,[]) ~f:(fun (regs,mems,args) (n,t) ->
        let words = Option.value (size#bits t) ~default:32 / 32 in
        let exps,regs = List.split_n (align regs t) words in
        let rest,mems = Seq.split_n mems (words - List.length exps) in
        regs,mems, (C.Abi.data size t, (concat (exps@rest))) :: args) in
  Some C.Abi.{return; hidden; params = List.rev params}

let api arch = C.Abi.create_api_processor arch args

let setup () =
  let id = ref None in
  Stream.observe  Project.Info.arch (function
      | #Arch.arm as arch ->
        Option.iter !id ~f:Bap_api.retract;
        info "using armeabi ABI";
        id := Some (Bap_api.process (api arch))
      | _ -> Option.iter !id ~f:Bap_api.retract)
