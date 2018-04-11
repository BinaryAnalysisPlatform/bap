open Core_kernel
open Bap.Std
open X86_opcode_cmpxchg
open X86_asm.Reg

module Make (Tools : X86_tools.S) (Backend : X86_backend.S) = struct
  open Tools

  let local_var name width =
    Var.create ~is_virtual:true ~fresh:true name @@ Type.imm width

  let get_rax = function
    | `CMPXCHG8rr
    | `CMPXCHG8rm -> `AL
    | `CMPXCHG16rr
    | `CMPXCHG16rm -> `AX
    | `CMPXCHG32rr
    | `CMPXCHG32rm -> `EAX
    | `CMPXCHG64rr
    | `CMPXCHG64rm -> `RAX

  let cmpxchg_rr (op:cmpxchg_rr) =
    X86_operands.rr ~f:(fun mem fst snd ->
        let fst = RR.of_mc_exn fst in
        let snd = RR.of_mc_exn snd in
        let rax = get_rax op |> RR.of_asm_exn in
        let size = RR.width rax in
        let v = local_var "v" @@ Size.in_bits size in
        let bil =
          let sub = Bil.(v := RR.get rax - RR.get fst) in
          let flags = FR.after_sub ~diff:(Bil.var v)
              ~op1:(RR.get rax) ~op2:(RR.get fst) size in
          let cmpxchg = Bil.(if_ (RR.get rax = RR.get fst)
                               ([RR.get snd |> RR.set fst])
                               ([RR.get fst |> RR.set rax])) in
          List.concat [sub::flags; [cmpxchg]] in
        Ok bil)

  let cmpxchg_rm (op:cmpxchg_rm) =
    X86_operands.mr ~f:(fun mem ~seg ~base ~scale ~index ~disp reg ->
        let fst = MM.of_mem mem ~seg ~base ~scale ~index ~disp in
        let snd = RR.of_mc_exn reg in
        let rax = get_rax op |> RR.of_asm_exn in
        let size = RR.width rax in
        let bits_size = Size.in_bits size in
        let d = local_var "d" bits_size in
        let v = local_var "v" bits_size in
        let bil =
          let load = Bil.(d := MM.load fst ~size) in
          let sub = Bil.(v := RR.get rax - var d) in
          let flags = FR.after_sub ~diff:(Bil.var v)
              ~op1:(RR.get rax) ~op2:(Bil.var d) size in
          let cmpxchg = Bil.(if_ (RR.get rax = var d)
                               ([RR.get snd |> MM.store fst ~size])
                               ([RR.set rax (Bil.var d)])) in
          List.concat[load::sub::flags; [cmpxchg]] in
        Ok bil)

  let cmpxchg (op:cmpxchg) =
    let open Or_error in
    let lift =
      match op with
      | #cmpxchg_rr as op -> cmpxchg_rr op
      | #cmpxchg_rm as op -> cmpxchg_rm op in
    fun mem insn ->
      lift mem insn >>= fun bil ->
      match op with
      | #cmpxchg_rm when X86_prefix.exists `xF0 mem -> Ok (PR.lock bil)
      | _ -> Ok bil

  let register what =
    let name op =
      sexp_of_cmpxchg (op :> cmpxchg) |> Sexp.to_string in
    List.iter (what :> cmpxchg list)
      ~f:(fun op -> Backend.register (name op) (cmpxchg op))
end

module IA32 = Make (X86_tools.IA32) (X86_backend.IA32)
module AMD64 = Make (X86_tools.AMD64) (X86_backend.AMD64)

let () =
  IA32.register all_of_cmpxchg_rr_ia32;
  IA32.register all_of_cmpxchg_rm_ia32;
  AMD64.register all_of_cmpxchg_rr;
  AMD64.register all_of_cmpxchg_rm
