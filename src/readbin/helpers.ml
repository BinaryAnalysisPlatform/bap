open Core_kernel.Std
open Bap.Std
open Options

module type ABI = sig
  val is_reg : var -> bool
  val is_sp : var -> bool
  val is_pc : var -> bool
  val addr_of_pc : mem -> addr
  val is_flag : var -> bool
  val is_zf : var -> bool
  val is_cf : var -> bool
  val is_vf : var -> bool
  val is_nf : var -> bool
  val is_mem : var -> bool


  val is_permanent : var -> bool
  val is_return : var -> bool
  val is_formal : int -> var -> bool
end

module ARM : ABI = struct
  open Bap_disasm_arm_env
  let regs = Var.Set.of_list [
      r0; r1; r2; r3; r4;
      r5; r6; r7; r8; r9;
      r10; r11; r12; r13;
      r14; r15; pc;  sp;
    ]

  let perms = Var.Set.of_list [
      r4; r5; r6; r7; r8; r9; r10; r11;
    ]

  let flags = Var.Set.of_list @@ [
      nf; zf; cf; qf;
    ] @ Array.to_list ge

  let is = Var.equal

  let is_reg = Set.mem regs
  let is_sp = is sp
  let is_pc = is pc
  let addr_of_pc m = Addr.(Memory.min_addr m ++ 8)
  let is_flag = Set.mem flags
  let is_zf = is zf
  let is_cf = is cf
  let is_vf = is vf
  let is_nf = is nf

  let is_return = is r0
  let is_formal = function
    | 0 -> is r0
    | 1 -> is r1
    | 2 -> is r2
    | 3 -> is r3
    | _ -> fun _ -> false

  let is_permanent = Set.mem perms
  let is_mem = is mem
end

module ABI = ARM

module Make(Env : Printing.Env) = struct
  module Printing = Printing.Make(Env)
  open Env
  open Printing

  (** maps immediates to symbols.
      For any given value, if it belongs to some basic block, then
      substitute it with [base + off], where [base] is a start of
      basic block and [off] is the offset from the [base]. *)
  let resolve_to_symbols =
    let jump_type = match Arch.addr_size arch with
      | `r32 -> reg32_t
      | `r64 -> reg64_t in
    let make_var name =
      Exp.var (Var.create name jump_type) in
    (object inherit Bil.mapper as super
      method! map_int addr =
        match Table.find_addr syms addr with
        | Some (mem,sym) ->
          let start = Memory.min_addr mem in
          if Addr.(start = addr) then make_var sym else
            let off = Addr.Int_exn.(addr - start) in
            Exp.(make_var sym + int off)
        | None -> Exp.Int addr
    end)#run

  let resolve_indirects =
    Bil.map (object inherit Bil.mapper as super
      method! map_load ~src ~addr endian scale =
        let exp = super#map_load ~src ~addr endian scale in
        match addr with
        | Bil.Int addr -> (match Memory.get ~scale ~addr base with
            | Ok w -> Bil.int w
            | _ -> exp)
        | _ -> exp
    end)

  let resolve_pc mem = Bil.map (object(self)
      inherit Bil.mapper as super
      method! map_var var =
        if ABI.is_pc var then
          Bil.int (ABI.addr_of_pc mem)
        else super#map_var var
    end)

  (* we're very conservative here *)
  let has_side_effect e scope = (object inherit [bool] Bil.visitor
    method! enter_load  ~src:_ ~addr:_ _e _s _r = true
    method! enter_store ~dst:_ ~addr:_ ~src:_ _e _s _r = true
    method! enter_var v r = r || Bil.is_modified v scope
  end)#visit_exp e false

  (** This optimization will inline temporary variables that occurres
      inside the instruction definition if the right hand side of the
      variable definition is either side-effect free, or another
      variable, that is not changed in the scope of the variable definition. *)
  let inline_variables stmt =
    let rec loop ss = function
      | [] -> List.rev ss
      | Bil.Move _ as s :: [] -> loop (s::ss) []
      | Bil.Move (x, Exp.Var y) as s :: xs when Var.is_tmp x ->
        if Bil.is_modified y xs || Bil.is_modified x xs
        then loop (s::ss) xs else
          let xs = Bil.substitute (Exp.var x) (Exp.var y) xs in
          loop ss xs
      | Bil.Move (x, y) as s :: xs when Var.is_tmp x ->
        if has_side_effect y xs || Bil.is_modified x xs
        then loop (s::ss) xs
        else loop ss (Bil.substitute (Exp.var x) y xs)
      | s :: xs -> loop (s::ss) xs in
    loop [] stmt

  let disable_if option optimization =
    if Field.get option options then Fn.id else optimization

  let optimizations (mem,bil) =
    let open Fields in
    let f = List.map ~f:Bil.fixpoint [
        disable_if no_resolve       resolve_indirects;
        disable_if no_resolve       resolve_to_symbols;
        disable_if keep_alive       Bil.prune_unreferenced;
        disable_if keep_consts      Bil.fold_consts;
        disable_if keep_consts      Bil.normalize_negatives;
        disable_if keep_consts      (resolve_pc mem);
        disable_if no_inline        inline_variables;
      ]
            |> List.reduce_exn ~f:Fn.compose
            |> Bil.fixpoint
            |> disable_if no_optimizations in
    f bil

  let bil_of_insns insns =
    List.(insns >>| Tuple2.map2 ~f:Insn.bil >>| optimizations |> concat)


  let bil_of_block blk : bil =
    bil_of_insns List.(Block.insns blk)
end
