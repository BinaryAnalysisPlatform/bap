open Bap.Std
open X86_asm_reg_types

let base_var mode reg =
  let module CPU = (val (X86_env.vars_of_mode mode)) in
  let internal_reg = X86_asm_reg.decode reg in
  match internal_reg with
  | None -> None
  | Some reg ->
    if mode = X86_types.X86 then (
      match reg with
      (** 64-bit registers are invalid *)
      | `R8B | `R9B | `R10B | `R11B
      | `R12B | `R13B | `R14B | `R15B
      | `R8W | `R9W | `R10W | `R11W
      | `R12W | `R13W | `R14W | `R15W
      | `R8D | `R9D | `R10D | `R11D
      | `R12D | `R13D | `R14D | `R15D
      | #r64 | `RIP
      | `XMM8 | `YMM8 | `XMM9 | `YMM9
      | `XMM10 | `YMM10 | `XMM11 | `YMM11
      | `XMM12 | `YMM12 | `XMM13 | `YMM13
      | `XMM14 | `YMM14 | `XMM15 | `YMM15 ->
        invalid_arg "Provided register is unavailable for given mode"
      | _ ->
        ()
    );
    (match reg with
     | `AL | `AH | `AX | `EAX | `RAX -> Some CPU.rax
     | `BL | `BH | `BX | `EBX | `RBX -> Some CPU.rbx
     | `CL | `CH | `CX | `ECX | `RCX -> Some CPU.rcx
     | `DL | `DH | `DX | `EDX | `RDX -> Some CPU.rdx
     | `SIL | `SI | `ESI | `RSI -> Some CPU.rsi
     | `DIL | `DI | `EDI | `RDI -> Some CPU.rdi
     | `BPL | `BP | `EBP | `RBP -> Some CPU.rbp
     | `SPL | `SP | `ESP | `RSP -> Some CPU.rsp
     | `R8B | `R8W | `R8D | `R8 -> Some CPU.r.(0)
     | `R9B | `R9W | `R9D | `R9 -> Some CPU.r.(1)
     | `R10B | `R10W | `R10D | `R10 -> Some CPU.r.(2)
     | `R11B | `R11W | `R11D | `R11 -> Some CPU.r.(3)
     | `R12B | `R12W | `R12D | `R12 -> Some CPU.r.(4)
     | `R13B | `R13W | `R13D | `R13 -> Some CPU.r.(5)
     | `R14B | `R14W | `R14D | `R14 -> Some CPU.r.(6)
     | `R15B | `R15W | `R15D | `R15 -> Some CPU.r.(7)
     | `EIP | `IP | `RIP -> Some CPU.rip

     | `GS -> CPU.seg_gs
     | `FS_BASE -> Some CPU.fs_base
     | `CS -> CPU.seg_cs
     | `GS_BASE -> Some CPU.gs_base
     | `DS -> CPU.seg_ds
     | `ES -> CPU.seg_es
     | `SS -> CPU.seg_ss
     | `FS -> CPU.seg_fs

     | `XMM0 | `YMM0 -> Some CPU.ymms.(0)
     | `XMM1 | `YMM1 -> Some CPU.ymms.(1)
     | `XMM2 | `YMM2 -> Some CPU.ymms.(2)
     | `XMM3 | `YMM3 -> Some CPU.ymms.(3)
     | `XMM4 | `YMM4 -> Some CPU.ymms.(4)
     | `XMM5 | `YMM5 -> Some CPU.ymms.(5)
     | `XMM6 | `YMM6 -> Some CPU.ymms.(6)
     | `XMM7 | `YMM7 -> Some CPU.ymms.(7)
     | `XMM8 | `YMM8 -> Some CPU.ymms.(8)
     | `XMM9 | `YMM9 -> Some CPU.ymms.(9)
     | `XMM10 | `YMM10 -> Some CPU.ymms.(10)
     | `XMM11 | `YMM11 -> Some CPU.ymms.(11)
     | `XMM12 | `YMM12 -> Some CPU.ymms.(12)
     | `XMM13 | `YMM13 -> Some CPU.ymms.(13)
     | `XMM14 | `YMM14 -> Some CPU.ymms.(14)
     | `XMM15 | `YMM15 -> Some CPU.ymms.(15)
    )