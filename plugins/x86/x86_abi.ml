open Core_kernel.Std
open Bap.Std
open Bap_c.Std

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
end

module SysV = struct
  include X86_cpu.AMD64
  include Bil
  let name = "sysv"
  let arch = `x86_64
  let stack = Stack.create arch
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
end

module CDECL = struct
  include X86_cpu.IA32
  include Bil
  let name = "cdecl"
  let arch = `x86
  let stack = Stack.create arch
  let arg = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg n -> stack n

  let size = object
    inherit C.Size.base `ILP32
  end
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
      | `Basic {C.Type.Spec.t=#C.Type.short} -> 32
      | t -> super#alignment t
  end
end

module FASTCALL = struct
  include CDECL
  let abi = function
    | Arg 0 -> var rcx
    | Arg 1 -> var rdx
    | other -> arg other
end

exception Unsupported

let api (module Abi : abi) {C.Type.Proto.return; args} =
  let word = Arch.addr_size (Abi.arch :> arch) |> Size.in_bits in
  let ret = match Abi.size#bits return with
    | None -> []
    | Some width -> match Size.of_int_opt width with
      | None -> raise Unsupported
      | Some sz ->
        if width = word * 2
        then [C.Data.(Imm (sz,Top)), Bil.(Abi.arg Ret_0 ^ Abi.arg Ret_1)]
        else if width = word
        then [C.Data.(Imm (sz,Top)), Abi.arg Ret_0]
        else raise Unsupported in
  let args = List.mapi args ~f:(fun i (n,t) -> match Abi.size#bits t with
      | None -> raise Unsupported
      | Some size -> match Size.of_int_opt size with
        | Some sz when size = word -> C.Data.(Imm (sz,Top)), Abi.arg (Arg i)
        | _ -> raise Unsupported) in
  ret,args



(* Bap_api_abi.register Abi.arch Abi.name Abi.abi *)

(* let abis : (module abi) list = [ *)
(*   (module SysV); *)
(*   (module CDECL); *)
(*   (module STDCALL); *)
(*   (module FASTCALL); *)
(*   (module MS_32); *)
(*   (module MS_64); *)
(* ] *)


(* let args_of_proto (module Abi : abi) {C.Type.args; return} = () *)



(* let x64_cc_attrs = ["ms_abi"; "sysv_abi"] *)
(* let x32_cc_attrs = x64_cc_attrs @ ["cdecl"; "fastcall"; "thiscall"; "stdcall";] *)
(* let cc_attrs = function `x86 -> x32_cc_attrs | `x86_64 -> x64_cc_attrs *)

(* let x32_default = Option.value ~default:CDECL.name *)
(* let x64_default = Option.value ~default:SysV.name *)
(* let default = function `x86 -> x32_default | `x86_64 -> x64_default *)

(* let gnu_resolver default cc_attrs name attrs = *)
(*   let cc_attrs = ["cdecl"; "fastcall"; "thiscall"; "stdcall"; *)
(*                   "ms_abi"; "sysv_abi" ] in *)
(*   List.find attrs ~f:(fun x -> List.mem cc_attrs x.C.Type.attr_name) |> function *)
(*   | None -> default *)
(*   | Some x -> match String.chop_suffix ~suffix:"_abi" x.C.Type.attr_name with *)
(*     | None -> name *)
(*     | Some name -> name *)

(* let register_resolver ?default_abi:abi (arch : Arch.x86) = () *)
(* (\* Bap_api_abi.override_resolver (arch :> arch) *\) *)
(* (\*   (gnu_resolver (default arch abi) (cc_attrs arch)) *\) *)

(* let register () = *)
(*   List.iter abis ~f:register *)
