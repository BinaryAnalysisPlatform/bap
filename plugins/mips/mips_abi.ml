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
  val arg : pos -> exp
end

exception Unsupported

module Abi32 = struct
  open Mips.Std
  open MIPS_32

  let reg n = Bil.Var (Map.find_exn gpri n)
  let name = "mips32"
  let size = object
    inherit C.Size.base `ILP32
  end
  let arg = function
    | Ret_0 -> reg 2
    | Ret_1 -> reg 3
    | Arg n -> reg (n + 4)
end

module Abi64 = struct
  open Mips.Std
  open MIPS_64

  let reg n = Bil.Var (Map.find_exn gpri n)
  let name = "mips64"
  let size = object
    inherit C.Size.base `ILP64
  end
  let arg = function
    | Ret_0 -> reg 2
    | Ret_1 -> reg 3
    | Arg n -> reg (n + 4)
end


let supported_api arch (module Abi : abi) {C.Type.Proto.return; args} =
  let word = Arch.addr_size arch |> Size.in_bits in
  let return = match Abi.size#bits return with
    | None -> None
    | Some width  -> match Size.of_int_opt width with
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
           raise Unsupported)
  in
  let params = List.mapi args ~f:(fun i (n, t) ->
      match Abi.size#bits t with
      | None ->
        warning "size of %a parameter is unknown" C.Type.pp t;
        raise Unsupported
      | Some size -> match Size.of_int_opt size with
        | Some sz when size <= word ->
          C.Abi.data Abi.size t, Abi.arg (Arg i)
        | _ ->
          warning "argument %d doesn't fit into word" i;
          raise Unsupported)
  in
  C.Abi.{return; params; hidden = []}

let api abi proto arch =
  try Some (supported_api arch abi proto) with Unsupported ->
    warning "skipped function due to unsupported abi";
    None

let dispatch abi arch sub attrs proto = api abi proto arch

let strip_leading_dot s =
  match String.chop_prefix s ~prefix:"." with
  | None -> s
  | Some s -> s

let demangle demangle prog =
  Term.map sub_t prog ~f:(fun sub ->
      let name = demangle (Sub.name sub) in
      Tid.set_name (Term.tid sub) name;
      Sub.with_name sub name)

let set_abi proj m =
  let module A = (val m : abi) in
  let abi = C.Abi.{
      insert_args = dispatch m (Project.arch proj);
      apply_attrs = fun _ -> ident
    } in
  let api = C.Abi.create_api_processor A.size abi in
  Bap_api.process api;
  let prog = Project.program proj in
  let prog = demangle strip_leading_dot prog in
  Project.set (Project.with_program proj prog) Bap_abi.name A.name

let main proj = match Project.arch proj with
  | `mips ->
    info "using MIPS ABI";
    set_abi proj (module Abi32)
  | `mips64 ->
    info "using MIPS64 ABI";
    set_abi proj (module Abi64)
  | _ -> proj

let setup () = Bap_abi.register_pass main
