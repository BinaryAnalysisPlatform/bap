open Core_kernel.Std
open Bap.Std
open Or_error
open Or_error.Monad_infix

open Riscv_types
open Riscv_utils

module Insn = Disasm_expert.Basic.Insn
module Branch = Riscv_branch
module Env = Riscv_env

module Riscv = struct
  (** Defines the register map *)
  module CPU = struct
    let mem = Var.create "mem" @@ mem32_t `r8
    let reg name = Var.create name reg32_t
    let regs pref = Array.init ~f:(fun i -> reg @@ sprintf "%s%d" pref i)
    let zero = reg "zero"
    let r = regs "r" 31

    let ra = reg "r1"
    let sp = reg "r2"
    let gp = reg "r3"
    let tp = reg "r4"
    let fp = reg "r8"

    let pc = reg "pc"

    let gprs = Array.concat [
        r;
        [|zero; gp; tp; sp; fp; ra|] ;
    ]

    let gpr = Array.to_list gprs |> Var.Set.of_list

    let reg_of_name name =
      let name = String.lowercase name in
      Array.find gprs ~f:(fun reg ->
      Var.name reg = name)

    (* the problem is that RISC-V doesn't have flags at all,
       but we can just pretend that they are. They will not
       be used.
       Seems that a drawback of some BAP architecture...
       *)
    let flag n = Var.create n bool_t
    let zf = flag "zf"
    let cf = flag "cf"
    let vf = flag "vf"
    let nf = flag "nf"
    let never _ = false
    let is_reg = Set.mem gpr
    let is_flag = never
    let is_zf = never
    let is_cf = never
    let is_vf = never
    let is_nf = never
    let is_sp v = Var.same v sp
    let is_bp v = Var.same v fp
    let is_mem v = Var.same v mem

    let addr_of_pc m = Addr.(Memory.min_addr m ++ 4)
  end

  (** simplify an expression by applying constant folding *)
  let simpl = Bil.fixpoint Bil.fold_consts

  (** [reg op] is [Ok reg] if operand is a register with the same
      name as reg *)
  let reg = function
    | Op.Imm _ | Op.Fmm _ -> Or_error.errorf "expected register"
    | Op.Reg reg ->
      let name = Reg.name reg in
      match CPU.reg_of_name name with
        | None -> invalid_argf "unknown register %s" name ()
        | Some reg -> Ok reg

  let imm = function
    | Op.Reg _ | Op.Fmm _ -> Or_error.errorf "expected immediate"
    | Op.Imm imm -> Ok imm

  (** [rrr_type f d s t] uses lifter [f] for an rrr-type instruction with
      arguments [d], [s], and [t]. *)
  let rrr_type f d s t = match reg d, reg s, reg t with
    | Ok d, Ok s, Ok t -> Ok (simpl (f d s t))
    | e1,e2,e3 -> Or_error.errorf "invalid instruction"

  (** [rri_type f d s i] uses lifter [f] for an rri-type instruction with
      arguments [d], [s], and [i]. *)
  let rri_type f d s i = match reg d, reg s, imm i with
    | Ok d, Ok s, Ok i -> Ok (simpl (f d s i))
    | e1,e2,e3 -> Or_error.errorf "invalid instruction"

  (** [rr_type f d s ] uses lifter [f] for an rr-type instruction with
      arguments [d], [s]. *)
  let rr_type f d s = match reg d, reg s with
    | Ok d, Ok s -> Ok (simpl (f d s))
    | e1,e2 -> Or_error.errorf "invalid instruction"

  (** [ri_type f d i] uses lifter [f] for an ri-type instruction with
      arguments [d], and [i]. *)
  let ri_type f d i = match reg d, imm i with
    | Ok d, Ok i -> Ok (simpl (f d i))
    | e1,e2 -> Or_error.errorf "invalid instruction"

  (** [rim_type f d i m] uses lifter [f] for an rim-type instruction with
      arguments [d], [i], and [m]. *)
  (* TODO: Check the memory input? *)
  let rim_type f d i m = match reg d, imm i with
    | Ok d, Ok i -> Ok (simpl (f d i m))
    | e1,e2 -> Or_error.errorf "invalid instruction"

  (** [mem_type_load f w s d b o] uses lifter [f] for an mem-type instruction with
      arguments [w], [s], [d], [b] and [o]. *)
  (* TODO: Check also the other arguments *)
  let mem_type_load f w s d b o = match reg d, reg b, imm o with
    | Ok d, Ok b, Ok o -> Ok (simpl (f w s d b o))
    | e1,e2,e3 -> Or_error.errorf "invalid instruction"

  (** [mem_type_store f w d b o] uses lifter [f] for an mem-type instruction with
      arguments [w], [d], [b] and [o]. *)
  (* TODO: Check also the other arguments *)
  let mem_type_store f w d b o = match reg d, reg b, imm o with
    | Ok d, Ok b, Ok o -> Ok (simpl (f w d b o))
    | e1,e2,e3 -> Or_error.errorf "invalid instruction"

  (** [bra_type f m s1 s2 a] uses lifter [f] for a branch-type instruction with
      arguments [m], [s1], [s1], and [a]. *)
  let bra_type f m s1 s2 a = match reg s1, reg s2, imm a with
    | Ok s1, Ok s2, Ok a -> Ok (simpl (f m s1 s2 a))
    | e1,e2,e3 -> Or_error.errorf "invalid instruction"

  (** [bra_type2 f m d a] uses lifter [f] for a branch-type instruction with
      arguments [m], [d], and [a]. *)
  let bra_type2 f m d a = match reg d, imm a with
    | Ok d, Ok a -> Ok (simpl (f m d a))
    | e1,e2 -> Or_error.errorf "invalid instruction"

  (** [!$reg] lifts [reg] into a Bil expression, substitution a zero
      register with zero value (a smart version of [Bil.Var]. *)
  let (!$) reg =
    if Var.equal reg CPU.zero
    then Bil.int (Word.zero 32)
    else Bil.var reg

  let (!%) imm =
    let imval = (Imm.to_word ~width:32 imm) in
    match imval with
      | None -> Bil.int (Word.zero 32)
      | Some iv -> Bil.int iv

  (** {2 Instruction semantics}  *)
  (* TODO: Improve signedness handling *)

  let addi r0 r1 imm = Bil.[
      r0 := !$r1 + !%imm;
    ]
  let add r0 r1 r2 = Bil.[
      r0 := !$r1 + !$r2;
    ]
  let sub r0 r1 r2 = Bil.[
      r0 := !$r1 - !$r2;
    ]
  let _and r0 r1 r2 = Bil.[
      r0 := !$r1 land !$r2;
    ]
  let andi r0 r1 imm = Bil.[
      r0 := !$r1 land !%imm;
    ]
  (* TODO: Make a difference in signed/unsigned *)
  let slt r0 r1 r2 = Bil.[
      Bil.if_ (!$r1 < !$r2) [
        r0 := Bil.int (Word.of_int ~width:32 1);
      ] [
        r0 := Bil.int (Word.of_int ~width:32 0);
      ];
    ]
  let sltu r0 r1 r2 = Bil.[
      Bil.if_ (!$r1 < !$r2) [
        r0 := Bil.int (Word.of_int ~width:32 1);
      ] [
        r0 := Bil.int (Word.of_int ~width:32 0);
      ];
    ]
  let slti r0 r1 imm = Bil.[
      Bil.if_ (!$r1 < !%imm) [
        r0 := Bil.int (Word.of_int ~width:32 1);
      ] [
        r0 := Bil.int (Word.of_int ~width:32 0);
      ];
    ]
  let sltiu r0 r1 imm = Bil.[
      Bil.if_ (!$r1 < !%imm) [
        r0 := Bil.int (Word.of_int ~width:32 1);
      ] [
        r0 := Bil.int (Word.of_int ~width:32 0);
      ];
    ]
  let sll r0 r1 r2 = Bil.[
      r0 := !$r1 lsl !$r2;
    ]
  let slli r0 r1 imm = Bil.[
      r0 := !$r1 lsl !%imm;
    ]
  let srl r0 r1 r2 = Bil.[
      r0 := !$r1 lsr !$r2;
    ]
  let srli r0 r1 imm = Bil.[
      r0 := !$r1 lsr !%imm;
    ]
  let sra r0 r1 r2 = Bil.[
      r0 := !$r1 lsr !$r2;
    ]
  let srai r0 r1 imm = Bil.[
      r0 := !$r1 lsr !%imm;
    ]
  let _or r0 r1 r2 = Bil.[
      r0 := !$r1 lor !$r2;
    ]
  let ori r0 r1 imm = Bil.[
      r0 := !$r1 lor !%imm;
    ]
  let xor r0 r1 r2 = Bil.[
      r0 := !$r1 lxor !$r2;
    ]
  let xori r0 r1 imm = Bil.[
      r0 := !$r1 lxor !%imm;
    ]

  let lui r0 imm = Bil.[
      r0 := !%imm lsl Bil.int (Word.of_int 12 ~width:32);
  ]

  let auipc r0 imm mem = Bil.[
      r0 := !%imm lsl Bil.int (Word.of_int 12 ~width:32) + Bil.int (Memory.min_addr mem);
  ]

  (* rd - destination register *)
  let load size sign rd base offset =
    let address = Bil.(var base + !%offset) in
    let temp = match size with
      | B | H -> tmp reg32_t
      | _ -> rd in
    let rhs = cast_of_sign sign 32 Bil.(var temp) in
    let extend = match size with
      | B | H -> [Bil.move rd rhs]
      | W | D -> [] in
    let typ = match size with
      | B -> `r8
      | H -> `r16
      | W | D -> `r32 in
    let load  m n = Bil.(load  m n LittleEndian typ) in
    let loads  =
        let mem = Bil.var (Env.mem) in
        if size = W then [
            Bil.move rd (load mem address);
        ] else [
            assn temp (load mem address);
        ] in
    List.concat[
        loads;
        extend;
    ]

  (* rs - source register *)
  let store size rs base offset =
    let address = Bil.(var base + !%offset) in
    let temp = match size with
      | B | H -> tmp reg32_t
      | _ -> rs in
    (* truncate the value if necessary *)
    let trunc = match size with
      | B | H ->
        let n = if size = B then 8 else 16 in
        [Bil.move temp Bil.(cast low n (var rs))]
      | W | D -> [] in
    let typ = match size with
      | B -> `r8
      | H -> `r16
      | W | D -> `r32 in
    let store m n v = Bil.(store m n v LittleEndian typ) in
    let stores =
      let m = Env.mem in
      let v = Bil.var m in
      match size with
        | D | W -> [
          Bil.move m (store v address Bil.(var rs));
        ]
        | B | H -> [
          Bil.move m (store v address Bil.(var temp));
        ] in
    List.concat [
      trunc;                   (* truncate the value if necessary *)
      stores;
    ]

  (* TODO: handle also read/write from R0 (zero) register *)
  (* TODO: handle also immediate *)
  (* TODO: do CSR registers size varies? *)
  let csrrw rs rd csr =
    let temp = tmp reg8_t in
    let dest = Env.of_reg rd in
    let rs = Env.of_reg rs |> Bil.var in
    let csr = Env.of_reg csr |> Bil.var in
    exec Bil.([
        assn temp (load (var Env.mem) csr LittleEndian `r8);
        Env.mem :=
          store (var Env.mem) csr (extract 7 0 rs) LittleEndian `r8;
        assn dest (cast unsigned 32 (var temp));
      ])

  (* TODO: do CSR registers size varies? *)
  let csrrs rs rd csr =
    let temp = tmp reg8_t in
    let temp2 = tmp reg8_t in
    let dest = Env.of_reg rd in
    let rs = Env.of_reg rs |> Bil.var in
    let csr = Env.of_reg csr |> Bil.var in
    exec Bil.([
        assn temp (load (var Env.mem) csr LittleEndian `r8);
        assn dest (cast unsigned 32 (var temp));
        temp2 := (Bil.var temp) lor rs;
        Env.mem :=
          store (var Env.mem) csr (extract 7 0 (Bil.var temp2)) LittleEndian `r8;
      ])

  (* TODO: do CSR registers size varies? *)
  let csrrc rs rd csr =
    let temp = tmp reg8_t in
    let temp2 = tmp reg8_t in
    let dest = Env.of_reg rd in
    let rs = Env.of_reg rs |> Bil.var in
    let csr = Env.of_reg csr |> Bil.var in
    exec Bil.([
        assn temp (load (var Env.mem) csr LittleEndian `r8);
        assn dest (cast unsigned 32 (var temp));
        temp2 := (Bil.var temp) land lnot rs;
        Env.mem :=
          store (var Env.mem) csr (extract 7 0 (Bil.var temp2)) LittleEndian `r8;
      ])

  (** [lift mem insn] dispatches instructions to corresponding lifters. *)
  let lift_move mem insn = match Insn.name insn, Insn.ops insn with
    | "ADD", [|r0;r1;r2|] -> rrr_type add r0 r1 r2
    | "ADDI", [|r0;r1;imm|] -> rri_type addi r0 r1 imm
    | "SLT", [|r0;r1;r2|] -> rrr_type slt r0 r1 r2
    | "SLTI", [|r0;r1;imm|] -> rri_type slti r0 r1 imm
    | "SLTU", [|r0;r1;r2|] -> rrr_type sltu r0 r1 r2
    | "SLTIU", [|r0;r1;imm|] -> rri_type sltiu r0 r1 imm
    | "SLL", [|r0;r1;r2|] -> rrr_type sll r0 r1 r2
    | "SLLI", [|r0;r1;imm|] -> rri_type slli r0 r1 imm
    | "SRL", [|r0;r1;r2|] -> rrr_type srl r0 r1 r2
    | "SRLI", [|r0;r1;imm|] -> rri_type srli r0 r1 imm
    | "SRA", [|r0;r1;r2|] -> rrr_type sra r0 r1 r2
    | "SRAI", [|r0;r1;imm|] -> rri_type srai r0 r1 imm
    | "AND", [|r0;r1;r2|] -> rrr_type _and r0 r1 r2
    | "ANDI", [|r0;r1;imm|] -> rri_type andi r0 r1 imm
    | "OR", [|r0;r1;r2|] -> rrr_type _or r0 r1 r2
    | "ORI", [|r0;r1;imm|] -> rri_type ori r0 r1 imm
    | "SUB", [|r0;r1;r2|] -> rrr_type sub r0 r1 r2
    | "XOR", [|r0;r1;r2|] -> rrr_type xor r0 r1 r2
    | "XORI", [|r0;r1;imm|] -> rri_type xori r0 r1 imm
    | "LUI", [|r0;imm|] -> ri_type lui r0 imm
    | "AUIPC", [|r0;imm|] -> rim_type auipc r0 imm mem
    | _ -> Ok [Bil.special (Insn.asm insn)]

  let lift_mem mem insn = match Insn.name insn, Insn.ops insn with
    | "LW", [|r0;r1;off|] -> mem_type_load load W Signed r0 r1 off
    | "LH", [|r0;r1;off|] -> mem_type_load load H Signed r0 r1 off
    | "LHU", [|r0;r1;off|] -> mem_type_load load H Unsigned r0 r1 off
    | "LB", [|r0;r1;off|] -> mem_type_load load B Signed r0 r1 off
    | "LBU", [|r0;r1;off|] -> mem_type_load load B Unsigned r0 r1 off
    | "SW", [|r0;r1;off|] -> mem_type_store store W r0 r1 off
    | "SH", [|r0;r1;off|] -> mem_type_store store H r0 r1 off
    | "SB", [|r0;r1;off|] -> mem_type_store store B r0 r1 off
    | _ -> Ok [Bil.special (Insn.asm insn)]

  (** Branching instructions *)
  let beq mem rs1 rs2 dst = Bil.[
    Bil.if_ (!$rs1 = !$rs2) [
      Bil.Jmp (Bil.int (Memory.min_addr mem) + !%dst);
    ][];
  ]

  let bne mem rs1 rs2 dst = Bil.[
    Bil.if_ (!$rs1 <> !$rs2) [
      Bil.Jmp (Bil.int (Memory.min_addr mem) + !%dst);
    ][];
  ]

  let bge mem rs1 rs2 dst = Bil.[
    Bil.if_ (!$rs1 > !$rs2) [
      Bil.Jmp (Bil.int (Memory.min_addr mem) + !%dst);
    ][];
  ]

  let blt mem rs1 rs2 dst = Bil.[
    (* Bil.if_ (signed rs1 < signed rs2) [ *)
    Bil.if_ (!$rs1 < !$rs2 ) [
      Bil.Jmp (Bil.int (Memory.min_addr mem) + !%dst);
    ][];
  ]

  let jalr mem rs rd off = Bil.[
    rd := Bil.int (Memory.min_addr mem) + Bil.int (Word.of_int 4 ~width:32);
    Bil.Jmp (!$rs + !%off);
  ]

  let jal mem rd off = Bil.[
    rd := Bil.int (Memory.min_addr mem) + Bil.int (Word.of_int 4 ~width:32);
    Bil.Jmp (!%off);
  ]

  let lift_branch mem insn = match Insn.name insn, Insn.ops insn with
    | "BEQ", [|r0;r1;addr|] -> bra_type beq mem r0 r1 addr
    | "BNE", [|r0;r1;addr|] -> bra_type bne mem r0 r1 addr
    | "BGE", [|r0;r1;addr|] -> bra_type bge mem r0 r1 addr
    | "BLT", [|r0;r1;addr|] -> bra_type blt mem r0 r1 addr
    | "BLTU", [|r0;r1;addr|] -> bra_type blt mem r0 r1 addr
    | "BGEU", [|r0;r1;addr|] -> bra_type bge mem r0 r1 addr
    | "JAL", [|r0;addr|] -> bra_type2 jal mem r0 addr
    | "JALR", [|r0;r1;off|] -> bra_type jalr mem r0 r1 off
    (* | "RET", [||] -> rrr_type ret *)
    | _ -> Ok [Bil.special (Insn.asm insn)]

(*
  let lift_csr mem insn = match Insn.name insn, Insn.ops insn with
    | "CSRRW", [|r0;r1|] -> rr_type csrrw r0 r1 r1
    | "CSRRS", [|r0;r1|] -> rr_type csrrs r0 r1 r1
    | "CSRRC", [|r0;r1|] -> rr_type csrrc r0 r1 r1
    | "CSRRWI", [|r0;imm|] -> ri_type csrrw r0 imm
    | "CSRRSI", [|r0;imm|] -> ri_type csrrs r0 imm
    | "CSRRCI", [|r0;imm|] -> ri_type csrrc r0 imm
    | _ -> Ok [Bil.special (Insn.asm insn)]
*)

  let riscv_ops_exn ops () =
    Array.map (ops) ~f:(fun op ->
      Option.value_exn
        ~here:[%here]
        ~error:(Error.create "unsupported operand" op Op.sexp_of_t )
        (Riscv_op.create op))

  let riscv_ops ops = try_with (riscv_ops_exn ops)


  (** Substitute PC with its value  *)
  let resolve_pc mem = Stmt.map (object(self)
    inherit Stmt.mapper as super
    method! map_var var =
      if Var.(equal var CPU.pc) then
        Bil.int (CPU.addr_of_pc mem)
      else super#map_var var
  end)

  let insn_exn mem insn : bil Or_error.t =
    let name = Basic.Insn.name insn in
    Memory.(Addr.Int_err.(!$(max_addr mem) - !$(min_addr mem)))
    >>= Word.to_int >>= fun s -> Size.of_int ((s+1) * 8) >>= fun size ->
    Memory.get ~scale:(size ) mem >>| fun word ->
    match Riscv_insn.of_basic insn with
      | None -> [Bil.special (sprintf "unsupported: %s" name)]
      | Some riscv_insn -> match riscv_ops (Basic.Insn.ops insn) with
        | Error err -> [Bil.special (Error.to_string_hum err)]
        | Ok ops -> match riscv_insn with
            | #move_insn as op -> lift_move mem insn
            | #mem_insn  as op -> lift_mem mem insn
            | #branch_insn as op -> lift_branch mem insn
  (*          | #csr_insn as op -> lift_csr ops op *)

  let lift mem insn =
    try insn_exn mem insn >>| resolve_pc mem with
      | Lifting_failed msg -> errorf "%s:%s" (Basic.Insn.name insn) msg
      | exn -> of_exn exn

end

let () = register_target `riscv (module Riscv)
