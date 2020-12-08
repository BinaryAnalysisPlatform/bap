open Core_kernel
open Bap.Std
open X86_tools_types

module Make(CPU : X86CPU) : RR = struct
  type t = X86_asm.reg [@@deriving sexp]

  let of_asm = function
    | r when CPU.avaliable r -> Some r
    | r -> None

  let of_mc reg =
    let open Option in
    X86_asm.Reg.decode reg >>= (function
        | #X86_asm.reg as t -> of_asm t
        | _ -> None)

  let of_asm_exn reg = Option.value_exn (of_asm reg)

  let of_mc_exn reg = Option.value_exn (of_mc reg)

  let to_asm t = t

  let size = match CPU.arch with `x86 -> `r32 | `x86_64 -> `r64

  let bitsize = size |> Size.in_bits

  let width t = match t with
    | #X86_asm.Reg.gpr as r -> X86_asm.Reg.width r
    | #X86_asm.Reg.segment_base -> size
    | #X86_asm.Reg.segment -> `r16

  let bitwidth t = width t |> Size.in_bits

  let var_all = function
    | `AL | `AH | `AX | `EAX | `RAX -> CPU.rax
    | `DL | `DH | `DX | `EDX | `RDX -> CPU.rdx
    | `CL | `CH | `CX | `ECX | `RCX -> CPU.rcx
    | `BL | `BH | `BX | `EBX | `RBX -> CPU.rbx
    | `DIL | `DI | `EDI | `RDI -> CPU.rdi
    | `BPL | `BP | `EBP | `RBP -> CPU.rbp
    | `SPL | `SP | `ESP | `RSP -> CPU.rsp
    | `SIL | `SI | `ESI | `RSI -> CPU.rsi
    | `R8B | `R8W | `R8D | `R8 -> CPU.r.(0)
    | `R9B | `R9W | `R9D | `R9 -> CPU.r.(1)
    | `R10B | `R10W | `R10D | `R10 -> CPU.r.(2)
    | `R11B | `R11W | `R11D | `R11 -> CPU.r.(3)
    | `R12B | `R12W | `R12D | `R12 -> CPU.r.(4)
    | `R13B | `R13W | `R13D | `R13 -> CPU.r.(5)
    | `R14B | `R14W | `R14D | `R14 -> CPU.r.(6)
    | `R15B | `R15W | `R15D | `R15 -> CPU.r.(7)
    | `FS_BASE -> CPU.fs_base
    | `GS_BASE -> CPU.gs_base
    | `CS -> X86_env.cs | `DS -> X86_env.ds
    | `ES -> X86_env.es | `FS -> X86_env.fs
    | `GS -> X86_env.gs | `SS -> X86_env.ss
    | `XMM0 | `YMM0 -> CPU.ymms.(0)
    | `XMM1 | `YMM1 -> CPU.ymms.(1)
    | `XMM2 | `YMM2 -> CPU.ymms.(2)
    | `XMM3 | `YMM3 -> CPU.ymms.(3)
    | `XMM4 | `YMM4 -> CPU.ymms.(4)
    | `XMM5 | `YMM5 -> CPU.ymms.(5)
    | `XMM6 | `YMM6 -> CPU.ymms.(6)
    | `XMM7 | `YMM7 -> CPU.ymms.(7)
    | `XMM8 | `YMM8 -> CPU.ymms.(8)
    | `XMM9 | `YMM9 -> CPU.ymms.(9)
    | `XMM10 | `YMM10 -> CPU.ymms.(10)
    | `XMM11 | `YMM11 -> CPU.ymms.(11)
    | `XMM12 | `YMM12 -> CPU.ymms.(12)
    | `XMM13 | `YMM13 -> CPU.ymms.(13)
    | `XMM14 | `YMM14 -> CPU.ymms.(14)
    | `XMM15 | `YMM15 -> CPU.ymms.(15)
    | `XMM16 | `YMM16 -> CPU.ymms.(16)
    | `XMM17 | `YMM17 -> CPU.ymms.(17)
    | `XMM18 | `YMM18 -> CPU.ymms.(18)
    | `XMM19 | `YMM19 -> CPU.ymms.(19)
    | `XMM20 | `YMM20 -> CPU.ymms.(20)
    | `XMM21 | `YMM21 -> CPU.ymms.(21)
    | `XMM22 | `YMM22 -> CPU.ymms.(22)
    | `XMM23 | `YMM23 -> CPU.ymms.(23)
    | `XMM24 | `YMM24 -> CPU.ymms.(24)
    | `XMM25 | `YMM25 -> CPU.ymms.(25)
    | `XMM26 | `YMM26 -> CPU.ymms.(26)
    | `XMM27 | `YMM27 -> CPU.ymms.(27)
    | `XMM28 | `YMM28 -> CPU.ymms.(28)
    | `XMM29 | `YMM29 -> CPU.ymms.(29)
    | `XMM30 | `YMM30 -> CPU.ymms.(30)
    | `XMM31 | `YMM31 -> CPU.ymms.(31)

  let here = Lexing.dummy_pos

  let var t =
    if CPU.avaliable t then var_all t
    else Error.failwiths ~here
        "invalid reg variable"
        t X86_asm.sexp_of_reg

  let bvar r = var r |> Bil.var

  let get r =
    let open X86_asm in
    let v = bvar r in
    match r, CPU.arch with
    | #Reg.r8h, _ -> Bil.(extract ~hi:15 ~lo:8 v)
    | (#Reg.r8l as r1), _ | (#Reg.r16 as r1), _ | (#Reg.r32 as r1), `x86_64
    | (#Reg.r128 as r1), _  ->
      Bil.(cast low (bitwidth r1) v)
    | #Reg.r32, `x86 | #Reg.r64, _ | #Reg.segment_base, _
    | #Reg.segment, _ | #Reg.r256, _ -> v

  let set r e =
    let open X86_asm in
    let lhs = var r in
    let rhs =
      let v = bvar r in
      match r, CPU.arch with
      | #Reg.r8h, _ ->
        let hp = bitsize - 16 in
        Bil.((cast high hp v) ^ e ^ (cast low 8 v))
      | (#Reg.r8l as r1), _ | (#Reg.r16 as r1), _
      | (#Reg.r128 as r1), _ ->
        let hp = bitsize - bitwidth r1 in
        Bil.((cast high hp v) ^ e)
      | #Reg.r32, `x86_64 -> Bil.(cast unsigned bitsize e)
      | #Reg.r32, `x86 | #Reg.r64, `x86_64 | #Reg.segment_base, _
      | #Reg.segment, _ | #Reg.r256, _ -> e
      | #Reg.r64, `x86 -> Error.failwiths ~here "invalid reg"
                            r X86_asm.sexp_of_reg
    in
    Bil.(lhs := rhs)
end
