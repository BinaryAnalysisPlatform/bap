open Core_kernel.Std
open Bap.Std
open Bap_c.Std

include Self()

module Stack = C.Abi.Stack

type pos =
  | Ret_0
  | Ret_1
  | Arg of int

module type abi = sig
  val name : string
  val size : C.Size.base
  val arg  : pos -> exp
end

type abi = (module abi)

exception Unsupported

module Abi32 = struct
  open Bap_powerpc.Std
  open PowerPC_32

  let reg i = Int.Map.find_exn gpri i |> Bil.var
  let name = "ppc32"
  let size = object
    inherit C.Size.base `ILP32
  end
  let arg = function
    | Ret_0 -> reg 3
    | Ret_1 -> reg 4
    | Arg n -> reg (n + 3)
end

let supported_api (module Abi : abi) {C.Type.Proto.return; args} =
  let word = Arch.addr_size (`ppc :> arch) |> Size.in_bits in
  let return = match Abi.size#bits return with
    | None -> None
    | Some width -> match Size.of_int_opt width with
      | None ->
        warning "size of return object doesn't fit into word sizes";
        raise Unsupported
      | Some sz ->
        let data = C.Abi.data Abi.size return in
        if width > word && width <= word * 2
        then Some (data, Bil.(Abi.arg Ret_0 ^ Abi.arg Ret_1))
        else if width <= word
        then Some (data, Abi.arg Ret_0)
        else
          (warning "size of return object doesn't fit into double word\n";
           raise Unsupported) in
  let params = List.mapi args ~f:(fun i (n,t) ->
      match Abi.size#bits t with
      | None ->
        warning "size of %a parameter is unknown" C.Type.pp t;
        raise Unsupported
      | Some size -> match Size.of_int_opt size with
        | Some sz when size <= word ->
          C.Abi.data Abi.size t, Abi.arg (Arg i)
        | _ ->
          warning "argument %d doesn't fit into word" i;
          raise Unsupported) in
  C.Abi.{return; params; hidden=[]}

let api abi proto =
  try Some (supported_api abi proto) with Unsupported ->
    warning "skipped function due to unsupported abi";
    None

let dispatch abi sub attrs proto = api abi proto

let main proj = match Project.arch proj with
  | `ppc ->
    info "using powerpc ABI";
    let abi = C.Abi.{
        insert_args = dispatch (module Abi32);
        apply_attrs = fun _ -> ident
      } in
    let api = C.Abi.create_api_processor Abi32.size abi in
    Bap_api.process api;
    let prog = Project.program proj in
    Project.set (Project.with_program proj prog) Bap_abi.name Abi32.name
  | _ -> proj

let setup () = Bap_abi.register_pass main
