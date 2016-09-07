open Core_kernel.Std
open Bap.Std
open X86_opcode
open X86_asm.Reg

module Make (Tools : X86_tools.S) = struct
  open Tools


  let mov_rr (op:mov_rr) =
    X86_operands.rr ~f:(fun _mem dst src ->
        let dst = RR.of_mc_exn dst in
        let src = RR.of_mc_exn src in
        Ok [RR.get src |> RR.set dst])

  let mov_ri (op:mov_ri) =
    X86_operands.ri ~f:(fun _mem reg imm ->
        RR.of_mc_exn reg |> fun reg ->
        IM.of_imm imm |> fun imm ->
        Ok [IM.get ~width:(RR.width reg) imm |> RR.set reg])

  let mov_mi (op:mov_mi) =
    X86_operands.mi ~f:(fun mem ~seg ~base ~scale ~index ~disp imm ->
        let mem = MM.of_mem mem ~seg ~base ~scale ~index ~disp in
        let imm = IM.of_imm imm in
        let size = match op with
          | `MOV8mi -> `r8
          | `MOV16mi -> `r16
          | `MOV32mi -> `r32
          | `MOV64mi32 -> `r64 in
        Ok [IM.get ~width:size imm |>
            MM.store mem ~size])

  let mov_rm (op:mov_rm) =
    X86_operands.rm ~f:(fun mem reg ~seg ~base ~scale ~index ~disp ->
        let reg = RR.of_mc_exn reg in
        let mem = MM.of_mem mem ~seg ~base ~scale ~index ~disp in
        Ok [MM.load mem ~size:(RR.width reg) |>
            RR.set reg])

  let mov_mr (op:mov_mr) =
    X86_operands.mr ~f:(fun mem ~seg ~base ~scale ~index ~disp reg ->
        let mem = MM.of_mem mem ~seg ~base ~scale ~index ~disp in
        let reg = RR.of_mc_exn reg in
        Ok [RR.get reg |>
            MM.store mem ~size:(RR.width reg)])

  let mov_oa (op:mov_oa) =
    X86_operands.i ~f:(fun _mem off ->
        let mem = MM.of_offset off in
        let reg =
          let asm = match op with
            | `MOV8o8a
            | `MOV64o8a -> `AL
            | `MOV16o16a
            | `MOV64o16a -> `AX
            | `MOV32o32a
            | `MOV64o32a -> `EAX
            | `MOV64o64a -> `RAX in
          RR.of_asm_exn asm in
        Ok [MM.load mem ~size:(RR.width reg) |>
            RR.set reg])

  let mov_ao (op:mov_ao) =
    X86_operands.i ~f:(fun _mem off ->
        let mem = MM.of_offset off in
        let reg =
          let asm = match op with
            | `MOV8ao8
            | `MOV64ao8 -> `AL
            | `MOV16ao16
            | `MOV64ao16 -> `AX
            | `MOV32ao32
            | `MOV64ao32 -> `EAX
            | `MOV64ao64 -> `RAX in
          RR.of_asm_exn asm in
        Ok [RR.get reg |>
            MM.store mem ~size:(RR.width reg)])

  let mov_rs (op:mov_rs) =
    X86_operands.rr ~f:(fun _mem reg seg ->
        let reg = RR.of_mc_exn reg in
        let seg = RR.of_mc_exn seg in
        let bil = match op, RR.size with
          | `MOV16rs, _
          | `MOV32rs, `r64 -> RR.get seg |> RR.set reg
          | `MOV32rs, `r32
          | `MOV64rs, _ -> let size = RR.size |> Size.in_bits in
            RR.get seg |> Bil.(cast unsigned size) |> RR.set reg in
        Ok [bil])

  let mov_ms (op:mov_ms) =
    X86_operands.mr ~f:(fun mem ~seg ~base ~scale ~index ~disp reg ->
        let mem = MM.of_mem mem ~seg ~base ~scale ~index ~disp in
        let reg = RR.of_mc_exn reg in
        Ok [RR.get reg |>
            MM.store mem ~size:(RR.width reg)])

  let mov_sr (op:mov_sr) =
    X86_operands.rr ~f:(fun _mem seg reg ->
        let seg = RR.of_mc_exn seg in
        let reg = RR.of_mc_exn reg in
        let bil = match op with
          | `MOV16sr -> RR.get reg |> RR.set seg
          | `MOV32sr
          | `MOV64sr ->
            let width = RR.width seg |> Size.in_bits in
            let reg = RR.var reg in
            Bil.(cast low width (var reg)) |> RR.set seg in
        Ok [bil])


  let mov_sm (op:mov_sm) =
    X86_operands.rm ~f:(fun mem reg ~seg ~base ~scale ~index ~disp ->
        let reg = RR.of_mc_exn reg in
        let mem = MM.of_mem mem ~seg ~base ~scale ~index ~disp in
        Ok [MM.load mem ~size:(RR.width reg) |>
            RR.set reg])

  let mov (op:X86_opcode.mov) =
    match op with
    | #mov_rr as op -> mov_rr op
    | #mov_ri as op -> mov_ri op
    | #mov_rm as op -> mov_rm op
    | #mov_mr as op -> mov_mr op
    | #mov_mi as op -> mov_mi op
    | #mov_oa as op -> mov_oa op
    | #mov_ao as op -> mov_ao op
    | #mov_rs as op -> mov_rs op
    | #mov_ms as op -> mov_ms op
    | #mov_sr as op -> mov_sr op
    | #mov_sm as op -> mov_sm op
end

module IA32 = Make (X86_tools.IA32)
module AMD64 = Make (X86_tools.AMD64)

let () =
  List.iter (all_of_mov_ia32 :> mov list) ~f:(fun op ->
      X86_backend.IA32.register (op :> X86_opcode.t) (IA32.mov op));
  List.iter (all_of_mov_amd64 :> mov list) ~f:(fun op ->
      X86_backend.AMD64.register (op :> X86_opcode.t) AMD64.(mov op));
