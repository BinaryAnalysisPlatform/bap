open Core_kernel
open Bap_types.Std
open Bap_ir


let get_direct_typ (e : exp) : Type.t = match e with
  | Bil.Var v -> Var.typ v
  | Bil.Unknown (_,t) -> t
  | Bil.Int w -> Type.Imm (Word.bitwidth w)
  | _ -> failwith "the expression is not flattened"

class substituter (x : var) (x' : var) = object
  inherit Exp.mapper as super

  method! map_var v =
    if Var.equal x v then Var x' else super#map_var v

  method! map_let v ~exp ~body =
    let exp = super#map_exp exp in
    let body = if Var.equal x v then body else super#map_exp body in
    Let (v, exp, body)
end

let flatten_exp (exp : exp) (blk : blk term) (before : tid) : exp * blk term =
  let is_virtual = true in
  let fresh = true in
  let rec aux (exp : exp) (blk : blk term) = match exp with
    | Bil.Load (x, y, endian, s) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let vtype = Type.Imm (Size.in_bits s) in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Load (x, y, endian, s) in
      let def = Ir_def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Store (x, y, z, endian, s) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let z, blk = aux z blk in
      let vtype = Type.Imm (Size.in_bits s) in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Store (x, y, z, endian, s) in
      let def = Ir_def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.BinOp (b, x, y) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let vtype = get_direct_typ x in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.BinOp(b, x, y) in
      let def = Ir_def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.UnOp (u, x) ->
      let x, blk = aux x blk in
      let vtype = get_direct_typ x in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.UnOp(u, x) in
      let def = Ir_def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Var _
    | Bil.Int _ -> exp, blk
    | Bil.Cast (c, n, x) ->
      let x, blk = aux x blk in
      let vtype = Type.Imm n in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Cast (c, n, x) in
      let def = Ir_def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Let (v, x, y) ->
      let x, blk = aux x blk in
      let var, blk = match x with
        | Var v -> v, blk
        | _ ->
          let vtype = Var.typ v in
          let var = Var.create ~is_virtual ~fresh "flt" vtype in
          let def = Ir_def.create var x in
          var, Term.prepend def_t ~before blk def in
      let y = (new substituter v var)#map_exp y in
      aux y blk
    | Bil.Unknown (_, _) -> exp, blk
    | Bil.Ite (x, y, z) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let z, blk = aux z blk in
      let vtype = get_direct_typ y in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Ite (x, y, z) in
      let def = Ir_def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Extract (n, p, x) ->
      let x, blk = aux x blk in
      let vtype = get_direct_typ x in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Extract (n, p, x) in
      let def = Ir_def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Concat (x, y) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let vtype = get_direct_typ x in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Concat (x, y) in
      let def = Ir_def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def in
  aux exp blk

let flatten_blk original_blk =
  let rec flatten_elts (elts : Ir_blk.elt seq) (blk : blk term) =
    let rec flatten_jmp (jmp : Ir_jmp.t) (expseq : exp seq) (blk : blk term) =
      match Seq.next expseq with
      | Some(hd, tl) ->
        let exp, blk = flatten_exp hd blk (Term.tid jmp) in
        Ir_jmp.substitute jmp hd exp |> Term.update jmp_t blk |>
        flatten_jmp jmp tl
      | None -> blk in

    match Seq.next elts with
    | Some (hd, tl) -> (match hd with
        | `Def def ->
          let exp, blk = flatten_exp (Ir_def.rhs def) blk (Term.tid def) in
          Ir_def.with_rhs def exp |> Term.update def_t blk |>
          flatten_elts tl
        | `Jmp jmp -> flatten_jmp jmp (Ir_jmp.exps jmp) blk
        | `Phi phi -> flatten_elts tl blk)
    | None -> blk in

  flatten_elts (Ir_blk.elts original_blk) original_blk

let flatten_sub =
  Term.map blk_t ~f:flatten_blk
