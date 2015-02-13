open Core_kernel.Std
open Bap.Std
open Options

module Make(Env : Printing.Env) = struct
  module Printing = Printing.Make(Env)
  open Env
  open Printing

  (** maps immediates to symbols.
      For any given value, if it belongs to some basic block, then
      substitute it with [base + off], where [base] is a start of
      basic block and [off] is the offset from the [base]. *)
  let resolve_jumps =
    let jump_type = match Arch.addr_size arch with
      | `r32 -> reg32_t
      | `r64 -> reg64_t in
    let blk_base blk =
      let name = Format.asprintf "%a" pp_blk_name blk in
      Exp.var (Var.create name jump_type) in
    (object inherit Bil.mapper as super
      method! map_int addr =
        match Table.find_addr cfg addr with
        | Some (mem,blk) ->
          let start = Memory.min_addr mem in
          if Addr.(start = addr) then blk_base blk else
            let off = Addr.Int_exn.(addr - start) in
            Exp.(blk_base blk + int off)
        | None -> Exp.Int addr
    end)#run



  (* we're very conservative here *)
  let has_side_effect e scope = (object inherit [bool] Bil.visitor
    method! enter_load  ~src:_ ~addr:_ _e _s _r = true
    method! enter_store ~dst:_ ~addr:_ ~src:_ _e _s _r = true
    method! enter_var v r = r || Bil.is_modified v scope
  end)#visit_exp e false

  (** This optimization will inline temporary variables that occurres
      inside the instruction definition if the right hand side of the
      variable definition is either side-effect free, or another
      variable, that is not changed in the scope of the variable definition.
  *)
  let inline_variables stmt =
    let rec loop ss = function
      | [] -> List.rev ss
      | Stmt.Move (x, Exp.Var y) as s :: xs when Var.is_tmp x ->
        if Bil.is_modified y xs || Bil.is_modified x xs
        then loop (s::ss) xs else
          let xs = Bil.substitute (Exp.var x) (Exp.var y) xs in
          loop ss xs
      | Stmt.Move (x, y) as s :: xs when Var.is_tmp x ->
        if has_side_effect y xs || Bil.is_modified x xs
        then loop (s::ss) xs
        else loop ss (Bil.substitute (Exp.var x) y xs)
      | s :: xs -> loop (s::ss) xs in
    loop [] stmt

  let disable_if option optimization =
    if Field.get option options then Fn.id else optimization

  let optimizations =
    let open Fields in
    List.map ~f:Bil.fixpoint [
      disable_if no_resolve       resolve_jumps;
      disable_if keep_alive       Bil.prune_unreferenced;
      disable_if keep_consts      Bil.fold_consts;
      disable_if keep_consts      Bil.normalize_negatives;
      disable_if no_inline        inline_variables;
    ]
    |> List.reduce_exn ~f:Fn.compose
    |> Bil.fixpoint
    |> disable_if no_optimizations

  let bil_of_insns insns =
    let insns = Seq.(insns >>| Insn.bil |> to_list) in
    List.(insns >>| optimizations |> concat)

  let bil_of_block blk : bil =
    bil_of_insns Seq.(Block.insns blk >>| snd)
end
