open Core_kernel.Std
open Bap.Std
open X86_tools_types

module type S = X86_tools_types.S

module Make (CPU : X86CPU) : S = struct
  module RR = X86_tools_reg.Make (CPU)
  module FR = X86_tools_flags.Make (CPU)
  module IM = X86_tools_imm
  module MM = X86_tools_mem.Make (CPU) (RR) (IM)
  module IV = X86_tools_vector
  module PR = X86_tools_prefix.Make (RR) (FR) (IV)
end

module IA32CPU : X86CPU = struct
 type regs = [
   | `AL | `BL | `CL | `DL
   | `AH | `BH | `CH | `DH
   | `AX | `BX | `CX | `DX
   | `DI | `SI | `BP | `SP
   | `EAX | `EBX | `ECX | `EDX
   | `EDI | `ESI | `EBP | `ESP
   | X86_asm.Reg.segment_base
   | X86_asm.Reg.segment
 ]

 let arch = `x86
 let avaliable = function
   | #regs -> true
   | _ -> false

 let cf = X86_env.cf
 let pf = X86_env.pf
 let af = X86_env.af
 let zf = X86_env.zf
 let sf = X86_env.sf
 let oF = X86_env.oF
 let df = X86_env.df

 include X86_env.R32
end

module AMD64CPU : X86CPU = struct
 type regs = X86_asm.reg

 let arch = `x86_64
 let avaliable = function
   | #regs -> true
   | _ -> false

 let cf = X86_env.cf
 let pf = X86_env.pf
 let af = X86_env.af
 let zf = X86_env.zf
 let sf = X86_env.sf
 let oF = X86_env.oF
 let df = X86_env.df

 include X86_env.R64
end

module IA32 = Make(IA32CPU)
module AMD64 = Make(AMD64CPU)
