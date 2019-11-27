open Core_kernel
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
  val arg  : C.Type.t -> int -> pos -> exp
  val demangle : string -> string
  val autodetect : project -> bool
end

type abi = (module abi)

module SysV = struct
  include X86_cpu.AMD64
  include Bil
  let name = "sysv"
  let arch = `x86_64
  let stack n = Stack.create arch n


  let xmm r width =  Bil.(cast low width (var ymms.(r)))

  let flt width = function
    | Ret_0 -> xmm 0 width
    | Ret_1 -> xmm 1 width
    | Arg n -> if Int.(n < 8) then (xmm n width) else stack Int.(n-8)

  let int _ = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg 0 -> var rdi
    | Arg 1 -> var rsi
    | Arg 2 -> var rdx
    | Arg 3 -> var rcx
    | Arg 4 -> var r.(0)
    | Arg 5 -> var r.(1)
    | Arg n -> stack Int.(n-6)

  let arg t width =
    match t with
    | `Basic {C.Type.Spec.t=(`float|`double)} -> flt width
    | _ -> int 64

  let size = object
    inherit C.Size.base `LP64
  end
  let demangle = ident
  let autodetect _ = false
end

module CDECL = struct
  include X86_cpu.IA32
  include Bil
  let name = "cdecl"
  let arch = `x86
  let stack n = Stack.create arch n
  let arg _ _ = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg n -> stack Int.(n+1)

  let size = object
    inherit C.Size.base `ILP32
  end

  let demangle = ident
  let autodetect _ = false
end

(* in our abstraction they are the same, as they have the same layout.*)
module STDCALL = struct
  include CDECL
  let name = "stdcall"
end

let strip_leading_underscore s =
  match String.chop_prefix s ~prefix:"_" with
  | None -> s
  | Some s -> s

let has_symbol fn proj =
  Option.is_some (Symtab.find_by_name (Project.symbols proj) fn)

module MS_32 = struct
  include STDCALL
  let name = "ms"
  let demangle = strip_leading_underscore
  let autodetect = has_symbol "__GetPEImageBase"
end

module MS_64 = struct
  include SysV
  let name = "ms"
  let arg _ _ = function
    | Ret_0 -> var rax
    | Ret_1 -> var rdx
    | Arg 0 -> var rcx
    | Arg 1 -> var rdx
    | Arg 2 -> var r.(0)
    | Arg 3 -> var r.(1)
    | Arg n -> stack Int.(n+1)

  let size = object
    inherit C.Size.base `LLP64 as super
    method! alignment = function
      | `Basic {C.Type.Spec.t=#C.Type.short} -> `r32
      | t -> super#alignment t
  end
  let autodetect = has_symbol "_GetPEImageBase"
end

module FASTCALL = struct
  include CDECL
  let name = "fastcall"
  let arg w t = function
    | Arg 0 -> var rcx
    | Arg 1 -> var rdx
    | other -> arg w t other
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
  let arg w t = function
    | Arg 0 -> var rax
    | Arg 1 -> var rdx
    | Arg 2 -> var rbx
    | Arg 3 -> var rcx
    | Arg n -> stack Int.(n-4)
    | ret -> arg w t ret
end

exception Unsupported

let supported_api (module Abi : abi) {C.Type.Proto.return; args} =
  let word = Arch.addr_size (Abi.arch :> arch) |> Size.in_bits in
  let return = match Abi.size#bits return with
    | None -> None
    | Some width -> match Size.of_int_opt width with
      | None ->
        warning "size of return object doesn't fit into word sizes";
        raise Unsupported
      | Some _ ->
        let data = C.Abi.data Abi.size return in
        if width > word && width <= word * 2
        then Some (data, Bil.(Abi.arg return width Ret_0 ^ Abi.arg return width Ret_1))
        else if width <= word
        then Some (data, Abi.arg return width Ret_0)
        else
          (warning "size of return object doesn't fit into double word\n";
           raise Unsupported) in
  let params = List.mapi args ~f:(fun i (_,t) ->
      match Abi.size#bits t with
      | None ->
        warning "size of %a parameter is unknown" C.Type.pp t;
        raise Unsupported
      | Some size -> match Size.of_int_opt size with
        | Some _ when size <= word ->
          C.Abi.data Abi.size t, Abi.arg t size (Arg i)
        | _ ->
          warning "argument %d doesn't fit into word" i;
          raise Unsupported) in
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

let has_same_name abi name =
  let module Abi = (val abi : abi) in
  String.equal Abi.name name ||
  String.equal (Abi.name ^ "_abi") name

let name (module Abi : abi) = Abi.name
let arch (module Abi : abi) = Abi.arch
let find name = supported () |> List.find ~f:(fun abi -> has_same_name abi name)

let auto proj = supported () |> List.find ~f:(fun (module Abi) ->
    Abi.autodetect proj)

let api abi proto =
  try Some (supported_api abi proto) with Unsupported ->
    warning "skipped function due to unsupported abi";
    None


let default_abi arch : (module abi) = match arch with
  | `x86 -> (module CDECL)
  | `x86_64 -> (module SysV)


let dispatch default sub attrs proto =
  let abi = supported () |> List.find ~f:(fun abi ->
      List.exists attrs ~f:(fun {C.Type.Attr.name} ->
          has_same_name abi name)) |> function
            | None -> default
            | Some abi -> abi in
  info "applying %s to %s" (name abi) (Sub.name sub);
  api abi proto


let demangle demangle prog =
  Term.map sub_t prog ~f:(fun sub ->
      let name = demangle (Sub.name sub) in
      Sub.with_name sub name)

let setup ?(abi=fun _ -> None) () =
  let main proj = match Project.arch proj with
    | #Arch.x86 as arch ->
      let abi = match abi arch with
        | Some abi -> abi
        | None -> match auto proj with
          | Some abi ->
            info "autodetected ABI";
            abi
          | None ->
            info "can't detect ABI, falling back to default";
            default_abi arch in
      let module Abi = (val abi) in
      info "using %s ABI" Abi.name;
      let abi = C.Abi.{
          insert_args = dispatch abi;
          apply_attrs = fun _ -> ident
        } in
      let api = C.Abi.create_api_processor Abi.size abi in
      Bap_api.process api;
      let prog = demangle Abi.demangle (Project.program proj) in
      Project.set (Project.with_program proj prog) Bap_abi.name Abi.name
    | _ -> proj in
  Bap_abi.register_pass main
