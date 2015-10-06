open Core_kernel.Std
open Bap.Std
open Options

module Make(Env : sig
    val options : Options.t
    val project : project
    module Target : Target
  end) = struct
  module Printing = Printing.Make(Env)
  open Env
  open Printing

  let arch = Project.arch project
  let syms = Project.symbols project
  let memory = Project.memory project


  (** maps immediates to symbols.
      For any given value, if it belongs to some basic block, then
      substitute it with [base + off], where [base] is a start of
      basic block and [off] is the offset from the [base]. *)
  let resolve_to_symbols =
    let jump_type = match Arch.addr_size arch with
      | `r32 -> reg32_t
      | `r64 -> reg64_t in
    let make_var name =
      Bil.var (Var.create name jump_type) in
    (object inherit Bil.mapper as super
      method! map_int addr =
        Symtab.fns_of_addr syms addr |> List.hd |> function
        | Some fn ->
          let start = Block.addr (Symtab.entry_of_fn fn) in
          let sym = Symtab.name_of_fn fn in
          if Addr.(start = addr) then make_var sym else
            let off = Addr.Int_exn.(addr - start) in
            Bil.(make_var sym + int off)
        | None -> Bil.Int addr
    end)#run


  (** substitute loads with the value of corresponding memory *)
  let resolve_indirects =
    Bil.map (object inherit Bil.mapper as super
      method! map_load ~mem ~addr endian scale =
        let exp = super#map_load ~mem ~addr endian scale in
        match addr with
        | Bil.Int addr ->
          let exp = Memmap.lookup memory addr |> Seq.hd |> function
            | None -> exp
            | Some (mem,_) -> match Memory.get ~scale ~addr mem with
              | Ok w -> Bil.int w
              | _ -> exp in
          exp
        | _ -> exp
    end)

  (** Substitute PC with its value  *)
  let resolve_pc mem = Bil.map (object(self)
      inherit Bil.mapper as super
      method! map_var var =
        if Target.CPU.is_pc var then
          Bil.int (Target.CPU.addr_of_pc mem)
        else super#map_var var
    end)

  (* we're very conservative here *)
  let has_side_effect e scope = (object inherit [bool] Bil.visitor
    method! enter_load  ~mem:_ ~addr:_ _e _s _r = true
    method! enter_store ~mem:_ ~addr:_ ~exp:_ _e _s _r = true
    method! enter_var v r = r || Bil.is_assigned v scope
  end)#visit_exp e false

  (** This optimization will inline temporary variables that occurres
      inside the instruction definition if the right hand side of the
      variable definition is either side-effect free, or another
      variable, that is not changed in the scope of the variable definition. *)
  let inline_variables stmt =
    let rec loop ss = function
      | [] -> List.rev ss
      | Bil.Move _ as s :: [] -> loop (s::ss) []
      | Bil.Move (x, Bil.Var y) as s :: xs when Var.is_tmp x ->
        if Bil.is_assigned y xs || Bil.is_assigned x xs
        then loop (s::ss) xs else
          let xs = Bil.substitute (Bil.var x) (Bil.var y) xs in
          loop ss xs
      | Bil.Move (x, y) as s :: xs when Var.is_tmp x ->
        if has_side_effect y xs || Bil.is_assigned x xs
        then loop (s::ss) xs
        else loop ss (Bil.substitute (Bil.var x) y xs)
      | s :: xs -> loop (s::ss) xs in
    loop [] stmt

  let disable_if option optimization =
    if Field.get option options then Fn.id else optimization

  let optimizations (mem,bil) =
    let open Fields in
    List.map ~f:Bil.fixpoint [
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
    |> disable_if no_optimizations
    |> fun optimize -> optimize bil

  let bil_of_insn insn = optimizations (Tuple2.map2 ~f:Insn.bil insn)


  let bil_of_insns insns =
    List.(insns >>| bil_of_insn |> concat)


  let bil_of_block blk : bil =
    bil_of_insns (Block.insns blk)
end
