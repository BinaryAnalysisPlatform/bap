open Core_kernel
open Bap.Std
open Bap_llvm.Std
open X86_asm.Reg
open X86_tools_types

type semantics = Mem_to_reg | Reg_to_mem

module type S = sig
  type t [@@deriving bin_io, sexp, compare, enumerate]
  val asm_of_t  : t -> X86_asm.reg
  val semantics : semantics
end

module type Version = sig
  module Mov_oa_ia32  : S
  module Mov_oa_amd64 : S
  module Mov_ao_ia32  : S
  module Mov_ao_amd64 : S
  val allow_nil : bool
end

module type Semantics = sig
  val lift : X86_asm.reg -> semantics -> bool -> lifter
end

module Insn_semantics(Tools : X86_tools.S) = struct
  open Tools

  let apply mem seg disp reg sem =
    let mem = MM.of_mem ?seg ~disp mem in
    match sem with
    | Mem_to_reg ->
      Ok [MM.load mem ~size:(RR.width reg) |> RR.set reg ]
    | Reg_to_mem ->
      Ok [RR.get reg |> MM.store mem ~size:(RR.width reg)]

  let lift asm sem allow_nil =
    let reg = RR.of_asm_exn asm in
    if allow_nil then
      X86_operands.ir ~f:(fun mem off seg -> apply mem (Some seg) off reg sem)
    else
      X86_operands.i ~f:(fun mem off -> apply mem None off reg sem)
end

module Ver_34 = struct

  let allow_nil = false

  module Mov_oa_ia32 = struct
    type t = MOV8o8a | MOV16o16a | MOV32o32a
    [@@deriving bin_io, sexp, compare, enumerate]

    let asm_of_t t = match t with
      | MOV8o8a    -> `AL
      | MOV16o16a  -> `AX
      | MOV32o32a  -> `EAX

    let semantics = Mem_to_reg
  end

  module Mov_oa_amd64 = struct
    type t = MOV64o8a | MOV64o16a | MOV64o32a | MOV64o64a
    [@@deriving bin_io, sexp, compare, enumerate]

    let asm_of_t t = match t with
      | MOV64o8a  -> `AL
      | MOV64o16a -> `AX
      | MOV64o32a -> `EAX
      | MOV64o64a -> `RAX

    let semantics = Mem_to_reg
  end

  module Mov_ao_ia32 = struct
    type t = MOV8ao8 | MOV16ao16 | MOV32ao32
    [@@deriving bin_io, sexp, compare, enumerate]

    let asm_of_t t = match t with
      | MOV8ao8   -> `AL
      | MOV16ao16 -> `AX
      | MOV32ao32 -> `EAX

    let semantics = Reg_to_mem
  end

  module Mov_ao_amd64 = struct
    type t = MOV64ao8 | MOV64ao16 | MOV64ao32 | MOV64ao64
    [@@deriving bin_io, sexp, compare, enumerate]

    let asm_of_t t = match t with
      | MOV64ao8  -> `AL
      | MOV64ao16 -> `AX
      | MOV64ao32 -> `EAX
      | MOV64ao64 -> `RAX

    let semantics = Reg_to_mem
  end
end

module Ver_common = struct

  let allow_nil = true

  module Mov_oa_ia32 = struct
    type t =
      | MOV8o16a
      | MOV8o32a
      | MOV16o32a
      | MOV32o32a
      | MOV16o16a
      | MOV32o16a
    [@@deriving bin_io, sexp, compare, enumerate]

    let asm_of_t op = match op with
      | MOV8o16a  | MOV8o32a  -> `AL
      | MOV16o16a | MOV16o32a -> `AX
      | MOV32o16a | MOV32o32a -> `EAX

    let semantics = Reg_to_mem
  end

  module Mov_oa_amd64 = struct
    type t = MOV8o64a | MOV16o64a | MOV32o64a | MOV64o32a | MOV64o64a
    [@@deriving bin_io, sexp, compare, enumerate]

    let asm_of_t op = match op with
      | MOV8o64a  -> `AL
      | MOV16o64a -> `AX
      | MOV32o64a -> `EAX
      | MOV64o32a | MOV64o64a -> `RAX

    let semantics = Reg_to_mem
  end

  module Mov_ao_ia32 = struct
    type t =
      | MOV8ao16
      | MOV8ao32
      | MOV16ao32
      | MOV32ao32
      | MOV16ao16
      | MOV32ao16
    [@@deriving bin_io, sexp, compare, enumerate]

    let asm_of_t op = match op with
      | MOV8ao16  | MOV8ao32  -> `AL
      | MOV16ao16 | MOV16ao32 -> `AX
      | MOV32ao16 | MOV32ao32 -> `EAX

    let semantics = Mem_to_reg
  end

  module Mov_ao_amd64 = struct
    type t = MOV8ao64 | MOV16ao64 | MOV32ao64 | MOV64ao32 | MOV64ao64
    [@@deriving bin_io, sexp, compare, enumerate]

    let asm_of_t op = match op with
      | MOV8ao64  -> `AL
      | MOV16ao64 -> `AX
      | MOV32ao64 -> `EAX
      | MOV64ao32 | MOV64ao64 -> `RAX

    let semantics = Mem_to_reg
  end
end

module Make(V : Version) = struct
  module IA32 = X86_backend.IA32
  module AMD64 = X86_backend.AMD64
  module Sema32 = Insn_semantics(X86_tools.IA32)
  module Sema64 = Insn_semantics(X86_tools.AMD64)
  open V

  let add insn back sema =
    let module L = (val insn : S) in
    let module B = (val back : X86_backend.S) in
    let module S = (val sema : Semantics) in
    List.iter L.all (fun op ->
        let f = S.lift (L.asm_of_t op) L.semantics V.allow_nil in
        let s = L.sexp_of_t op |> Sexp.to_string in
        B.register s f)

  let register () =
    add (module Mov_oa_ia32)  (module IA32)  (module Sema32);
    add (module Mov_ao_ia32)  (module IA32)  (module Sema32);
    add (module Mov_oa_amd64) (module AMD64) (module Sema64);
    add (module Mov_ao_amd64) (module AMD64) (module Sema64)
end

module T_34 = Make(Ver_34)
module T = Make(Ver_common)

module Self = Self ()

let () =
  let llvm_version = String.sub llvm_version 0 3 in
  if llvm_version = "3.4" then T_34.register ()
  else
  if List.mem ["3.8";"4.0";"5.0";"6.0";"7.0"] llvm_version ~equal:String.equal
  then T.register ()
  else
    Self.error
      "x86 MOV with offset instructions will not lifted due to unknown \
       llvm version %s\n" llvm_version
