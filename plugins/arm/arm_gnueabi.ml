open Core_kernel
open Bap.Std
open Bap_c.Std

include Self()

let size = object(self)
  inherit C.Size.base `ILP32
  method! enum _ = self#integer `uint
end


module Define(Arch : sig val name : arch end) = struct
  let arch = Arch.name
  let nats = Seq.unfold ~init:0 ~f:(fun n -> Some (n,n+1))
  let regs = ARM.CPU.[r0;r1;r2;r3] |> List.map ~f:Bil.var
  let mems = Seq.map nats ~f:(C.Abi.Stack.create arch)

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

  let api size = C.Abi.create_api_processor size abi
end

let main proj = match Project.arch proj with
  | #Arch.arm  | #Arch.thumb | #Arch.armeb | #Arch.thumbeb as arch ->
    let open Define(struct let name = arch end) in
    info "using armeabi ABI";
    C.Abi.register "eabi" abi;
    Bap_api.process (api size);
    Project.set proj Bap_abi.name "eabi"
  | _ -> proj

module Aapcs64 = struct
  open Bap_core_theory
  open Bap_c.Std
  open Bap.Std

  let name = "aapcs64"

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
    let* x8 = Arg.Arena.create [
        Option.value_exn (Theory.Target.var t "X8")] in

    (* integer calling convention *)
    let pass_integer refs regs t =
      Arg.count regs t >>= function
      | Some 1 -> Arg.choice [
          Arg.register regs t;
          Arg.memory t;
        ]
      | Some 2 -> Arg.choice [
          Arg.sequence [
            Arg.align_even regs;
            Arg.registers ~limit:2 regs t;
          ];
          Arg.memory t;
        ]
      | _ -> Arg.reference refs t in

    (* floating-point calling convention *)
    let pass_float refs regs t =
      Arg.count regs t >>= function
      | None -> Arg.reference refs t
      | Some _ -> Arg.choice [
          Arg.registers regs t;
          Arg.sequence [
            Arg.deplet regs;
            Arg.memory t
          ]
        ] in

    let arg refs iregs fregs r =
      if is_floating r
      then pass_float iregs fregs r
      else pass_integer refs iregs r in

    Arg.define ?return:(match r with
        | `Void -> None
        | r -> Some (arg x8 irets frets r))
      (Arg.List.iter args ~f:(fun (_,t) ->
           arg iargs iargs fargs t))

  let install () =
    List.iter Arm_target.[LE.v8a;EB.v8a] ~f:(fun parent ->
        Theory.Target.family parent |>
        List.iter ~f:define)


end

let setup () =
  Bap_abi.register_pass main;
  Aapcs64.install ();
