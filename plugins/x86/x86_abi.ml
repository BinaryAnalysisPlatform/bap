open Core_kernel.Std
open Bap.Std
open Bap_api


module Stack = Bap_api_abi.Stack

module SysV = struct
  include X86_cpu.AMD64
  include Bil
  let name = "sysv"
  let arch = `x86_64
  let stack = Stack.create arch
  let abi = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg 0 -> var rdi
    | Arg 1 -> var rsi
    | Arg 2 -> var rdx
    | Arg 3 -> var rcx
    | Arg 4 -> var r.(0)
    | Arg 5 -> var r.(1)
    | Arg n -> stack Int.(n-6)
end

module CDECL = struct
  include X86_cpu.IA32
  include Bil
  let name = "cdecl"
  let arch = `x86
  let stack = Stack.create arch
  let abi = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg n -> stack n
end

(* in our abstraction they are the same.
   In STDCALL callee cleans the stack, in CDECL
   caller cleans, the layout is still the same.
*)
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
  let abi = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg 0 -> var rcx
    | Arg 1 -> var rdx
    | Arg 2 -> var r.(0)
    | Arg 3 -> var r.(1)
    | Arg n -> stack n
end

module FASTCALL = struct
  include CDECL
  let abi = function
    | Arg 0 -> var rcx
    | Arg 1 -> var rdx
    | other -> abi other
end


module type abi = sig
  val arch : arch
  val name : string
  val abi : pos -> exp
end

let register (module Abi : abi) =
  Bap_api_abi.register Abi.arch Abi.name Abi.abi

let abis : (module abi) list = [
  (module SysV);
  (module CDECL);
  (module STDCALL);
  (module FASTCALL);
  (module MS_32);
  (module MS_64);
]

let x64_cc_attrs = ["ms_abi"; "sysv_abi"]
let x32_cc_attrs = x64_cc_attrs @ ["cdecl"; "fastcall"; "thiscall"; "stdcall";]
let cc_attrs = function `x86 -> x32_cc_attrs | `x86_64 -> x64_cc_attrs

let x32_default = Option.value ~default:CDECL.name
let x64_default = Option.value ~default:SysV.name
let default = function `x86 -> x32_default | `x86_64 -> x64_default

let gnu_resolver default cc_attrs name attrs =
  let cc_attrs = ["cdecl"; "fastcall"; "thiscall"; "stdcall";
                  "ms_abi"; "sysv_abi" ] in
  List.find attrs ~f:(fun x -> List.mem cc_attrs x.attr_name) |> function
  | None -> default
  | Some x -> match String.chop_suffix ~suffix:"_abi" x.attr_name with
    | None -> name
    | Some name -> name

let register_resolver ?default_abi:abi (arch : Arch.x86) =
  Bap_api_abi.override_resolver (arch :> arch)
    (gnu_resolver (default arch abi) (cc_attrs arch))

let register () =
  List.iter abis ~f:register
