open Core_kernel[@@warning "-D"]
open Bap_types.Std
open Graphlib.Std
open Bap_ir

module Ssa = Bap_sema_ssa
module G = Bap_tid_graph

let (++) = Set.union and (--) = Set.diff

let ssa_free_vars sub =
  let is_undefined v = Var.index v = 0 in
  Term.enum blk_t sub |> Seq.fold ~init:Var.Set.empty ~f:(fun vars blk ->
      vars ++ Set.filter (Ir_blk.free_vars blk) ~f:is_undefined)

module Live = struct
  type tran = {
    defs : Var.Set.t;
    uses : Var.Set.t;
  }

  type t = {
    blks : tran Tid.Map.t;
    outs : (tid, Var.Set.t) Solution.t
  }

  let pp_vars ppf vars =
    Format.pp_print_list
      ~pp_sep:Format.pp_print_space
      Var.pp
      ppf (Set.to_list vars)

  let pp_transfer ppf {uses; defs} =
    Format.fprintf ppf "(%a) / (%a)"
      pp_vars uses pp_vars defs

  let blk_defs blk =
    Term.enum def_t blk |>
    Seq.fold ~init:Var.Set.empty  ~f:(fun defs def ->
        Set.add defs (Ir_def.lhs def))

  let update blk trans ~f = Map.update trans blk ~f:(function
      | None -> f {defs=Var.Set.empty; uses=Var.Set.empty}
      | Some had -> f had)

  let block_transitions sub =
    Term.enum blk_t sub |>
    Seq.fold ~init:Tid.Map.empty ~f:(fun fs blk ->
        Map.add_exn fs (Term.tid blk) {
          defs = blk_defs blk;
          uses = Ir_blk.free_vars blk;
        }) |> fun trans ->
    Term.enum blk_t sub |>
    Seq.fold ~init:trans ~f:(fun init blk ->
        Term.enum phi_t blk |>
        Seq.fold ~init ~f:(fun init phi ->
            Ir_phi.values phi |>
            Seq.fold ~init ~f:(fun fs (src,exp) ->
                update src fs ~f:(fun {defs; uses} -> {
                      defs = Set.add defs (Ir_phi.lhs phi);
                      uses = uses ++ (Exp.free_vars exp -- defs)
                    }))))

  let lookup blks n = match Map.find blks n with
    | None -> {defs = Var.Set.empty; uses = Var.Set.empty}
    | Some t -> t

  let is_pseudo n = Tid.equal n G.exit || Tid.equal n G.start
  let apply {defs;uses} vars = vars -- defs ++ uses
  let transfer blks n vars =
    if is_pseudo n then vars
    else apply (lookup blks n) vars

  let initialize ?(init=Var.Set.empty) sub =
    Tid.Map.singleton G.exit @@
    Seq.fold (Term.enum arg_t sub) ~init ~f:(fun vars arg ->
        match Ir_arg.intent arg with
        | None | Some In -> vars
        | Some (Out | Both) ->
          vars ++ Exp.free_vars (Ir_arg.rhs arg))

  let compute ?keep:init sub =
    let g = G.create sub in
    let init = Solution.create (initialize ?init sub) Var.Set.empty in
    let blks = block_transitions sub in {
      blks;
      outs = Graphlib.fixpoint (module G) ~init ~start:G.exit ~rev:true g
          ~merge:Set.union
          ~equal:Var.Set.equal
          ~f:(transfer blks)
    }

  let outs {outs} blk = Solution.get outs blk
  let ins {outs; blks} blk =
    transfer blks blk (Solution.get outs blk)

  let defs {blks} blk = (lookup blks blk).defs
  let uses {blks} blk = (lookup blks blk).uses

  let fold live ~init ~f =
    Map.fold live.blks ~init ~f:(fun ~key:blk ~data:trans init ->
        let outs = Solution.get live.outs blk in
        f init blk (apply trans outs))

  let blks live var =
    fold live ~init:Tid.Set.empty ~f:(fun blks blk ins ->
        if Set.mem ins var
        then Set.add blks blk
        else blks)

  let vars live = outs live G.start

  let solution {outs=x} = x

  let pp ppf live =
    Format.pp_open_vbox ppf 0;
    fold live ~init:() ~f:(fun () blk live ->
        Format.fprintf ppf "@[<h>%a: @[<hov 2>(%a)@]@]@;"
          Tid.pp blk pp_vars live);
    Format.pp_close_box ppf ()
end

let compute_liveness sub =
  Live.(solution@@compute sub)

let free_vars_of_sub sub  =
  if Ssa.is_transformed sub
  then ssa_free_vars sub
  else Live.(vars@@compute sub)
