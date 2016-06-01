open Core_kernel.Std
open Bap.Std
open Bap_c.Std
open Bap_future.Std
include Self()

module Stack = C.Abi.Stack

type pos =
  | Ret_0
  | Ret_1
  | Arg of int


module type abi = sig
  val arch : Arch.x86
  val name : string
  val size : C.Size.base
  val arg  : pos -> exp
  val demangle : string -> string
end

type abi = (module abi)

module SysV = struct
  include X86_cpu.AMD64
  include Bil
  let name = "sysv"
  let arch = `x86_64
  let stack n = Stack.create arch n
  let arg = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg 0 -> var rdi
    | Arg 1 -> var rsi
    | Arg 2 -> var rdx
    | Arg 3 -> var rcx
    | Arg 4 -> var r.(0)
    | Arg 5 -> var r.(1)
    | Arg n -> stack Int.(n-6)

  let size = object
    inherit C.Size.base `LP64
  end
  let demangle = ident
end

module CDECL = struct
  include X86_cpu.IA32
  include Bil
  let name = "cdecl"
  let arch = `x86
  let stack n = Stack.create arch n
  let arg = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg n -> stack n

  let size = object
    inherit C.Size.base `ILP32
  end

  let demangle = ident
end

(* in our abstraction they are the same, as they have the same layout.*)
module STDCALL = struct
  include CDECL
  let name = "stdcall"
end

module MS_32 = struct
  include STDCALL
  let name = "ms"
end

module MS_64 = struct
  include SysV
  let name = "ms"
  let arg = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg 0 -> var rcx
    | Arg 1 -> var rdx
    | Arg 2 -> var r.(0)
    | Arg 3 -> var r.(1)
    | Arg n -> stack n

  let size = object
    inherit C.Size.base `LLP64 as super
    method! alignment = function
      | `Basic {C.Type.Spec.t=#C.Type.short} -> `r32
      | t -> super#alignment t
  end
end

module FASTCALL = struct
  include CDECL
  let name = "fastcall"
  let arg = function
    | Arg 0 -> var rcx
    | Arg 1 -> var rdx
    | other -> arg other
end

module WATCOM_STACK = struct
  include CDECL
  let name = "watcom-stack"
  let demangle s = match String.chop_suffix s ~suffix:"_" with
    | None -> s
    | Some s -> s
end


module WATCOM_REGS = struct
  include WATCOM_STACK
  let name = "watcom-regs"
  let arg = function
    | Arg 0 -> var rax
    | Arg 1 -> var rdx
    | Arg 2 -> var rbx
    | Arg 3 -> var rcx
    | Arg n -> stack Int.(n-4)
    | ret -> arg ret
end

exception Unsupported

let supported_api (module Abi : abi) {C.Type.Proto.return; args} =
  let word = Arch.addr_size (Abi.arch :> arch) |> Size.in_bits in
  let return = match Abi.size#bits return with
    | None -> None
    | Some width -> match Size.of_int_opt width with
      | None -> raise Unsupported
      | Some sz ->
        let data = C.Abi.data Abi.size return in
        if width = word * 2
        then Some (data, Bil.(Abi.arg Ret_0 ^ Abi.arg Ret_1))
        else if width = word
        then Some (data, Abi.arg Ret_0)
        else raise Unsupported in
  let params = List.mapi args ~f:(fun i (n,t) ->
      match Abi.size#bits t with
      | None -> raise Unsupported
      | Some size -> match Size.of_int_opt size with
        | Some sz when size = word ->
          C.Abi.data Abi.size t, Abi.arg (Arg i)
        | _ -> raise Unsupported) in
  C.Abi.{return; params; hidden=[]}



let supported () : (module abi) list = [
  (module SysV);
  (module CDECL);
  (module STDCALL);
  (module MS_32);
  (module MS_64);
  (module FASTCALL);
  (module WATCOM_STACK);
  (module WATCOM_REGS)
]

let name (module Abi : abi) = Abi.name
let arch (module Abi : abi) = Abi.arch
let find name = supported() |> List.find ~f:(fun (module Abi) ->
    Abi.name = name || Abi.name ^ "_abi" = name)

let api abi proto =
  try Some (supported_api abi proto) with Unsupported -> None


let default_abi arch : (module abi) = match arch with
  | `x86 -> (module CDECL)
  | `x86_64 -> (module SysV)


let dispatch default sub attrs proto =
  let abi = supported () |> List.find ~f:(fun (module Abi) ->
      List.exists attrs ~f:(fun {C.Type.Attr.name} ->
          Abi.name = name || Abi.name ^ "_abi" = name)) |> function
            | None -> default
            | Some abi -> abi in
  api abi proto


let demangle demangle prog =
  Term.map sub_t prog ~f:(fun sub ->
      let name = demangle (Sub.name sub) in
      Tid.set_name (Term.tid sub) name;
      Sub.with_name sub name)

let setup ?(abi=fun _ -> None) () =
  let main proj = match Project.arch proj with
    | #Arch.x86 as arch ->
      let abi = match abi arch with
        | None -> default_abi arch
        | Some abi -> abi in
      let module Abi = (val abi) in
      info "using %s ABI" Abi.name;
      let abi = C.Abi.{
          insert_args = dispatch abi;
          apply_attrs = fun _ -> ident
        } in
      let api = C.Abi.create_api_processor (Abi.arch :> arch) abi in
      Bap_api.process api;
      demangle Abi.demangle (Project.program proj) |>
      Project.with_program proj
    | _ -> proj in
  Bap_abi.register_pass main
