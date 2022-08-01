open Core_kernel[@@warning "-D"]
open Bap_types.Std
open Bap_ir
open Bap_core_theory

open KB.Syntax

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

let new_var s = Theory.Var.fresh s >>| Var.reify

let new_def var e =
  Theory.Label.fresh >>| fun tid ->
  Ir_def.create ~tid var e

let flatten_exp
    ?(before : tid option = None)
    (exp : exp)
    (blk : blk term) : (exp * blk term) KB.t =
  let insert blk def = match before with
    | None -> Term.append def_t blk def
    | Some before -> Term.prepend def_t ~before blk def in
  let rec aux (exp : exp) (blk : blk term) = match exp with
    | Bil.Load (x, y, endian, s) ->
      aux x blk >>= fun (x, blk) ->
      aux y blk >>= fun (y, blk) ->
      new_var (Theory.Bitv.define (Size.in_bits s)) >>= fun var ->
      let e = Bil.Load (x, y, endian, s) in
      new_def var e >>| fun def ->
      Bil.Var var, insert blk def
    | Bil.Store (x, y, z, endian, s) ->
      aux x blk >>= fun (x, blk) ->
      aux y blk >>= fun (y, blk) ->
      aux z blk >>= fun (z, blk) ->
      new_var (Theory.Bitv.define (Size.in_bits s)) >>= fun var ->
      let e = Bil.Store (x, y, z, endian, s) in
      new_def var e >>| fun def ->
      Bil.Var var, insert blk def
    | Bil.BinOp (b, x, y) ->
      aux x blk >>= fun (x, blk) ->
      aux y blk >>= fun (y, blk) ->
      new_var (Var.sort_of_typ (get_direct_typ x)) >>= fun var ->
      let e = Bil.BinOp(b, x, y) in
      new_def var e >>| fun def ->
      Bil.Var var, insert blk def
    | Bil.UnOp (u, x) ->
      aux x blk >>= fun (x, blk) ->
      new_var (Var.sort_of_typ (get_direct_typ x)) >>= fun var ->
      let e = Bil.UnOp(u, x) in
      new_def var e >>| fun def ->
      Bil.Var var, insert blk def
    | Bil.Var _ | Bil.Int _ -> !!(exp, blk)
    | Bil.Cast (c, n, x) ->
      aux x blk >>= fun (x, blk) ->
      new_var (Theory.Bitv.define n) >>= fun var ->
      let e = Bil.Cast (c, n, x) in
      new_def var e >>| fun def ->
      Bil.Var var, insert blk def
    | Bil.Let (v, x, y) ->
      aux x blk >>= fun (x, blk) ->
      begin match x with
        | Var v -> !!(v, blk)
        | _ ->
          new_var (Var.sort v) >>= fun var ->
          new_def var x >>| fun def ->
          var, insert blk def
      end >>= fun (var, blk) ->
      let y = (new substituter v var)#map_exp y in
      aux y blk
    | Bil.Unknown (_, _) -> !!(exp, blk)
    | Bil.Ite (x, y, z) ->
      aux x blk >>= fun (x, blk) ->
      aux y blk >>= fun (y, blk) ->
      aux z blk >>= fun (z, blk) ->
      new_var (Var.sort_of_typ (get_direct_typ y)) >>= fun var ->
      let e = Bil.Ite (x, y, z) in
      new_def var e >>| fun def ->
      Bil.Var var, insert blk def
    | Bil.Extract (n, p, x) ->
      aux x blk >>= fun (x, blk) ->
      new_var (Var.sort_of_typ (get_direct_typ x)) >>= fun var ->
      let e = Bil.Extract (n, p, x) in
      new_def var e >>| fun def ->
      Bil.Var var, insert blk def
    | Bil.Concat (x, y) ->
      aux x blk >>= fun (x, blk) ->
      aux y blk >>= fun (y, blk) ->
      new_var (Var.sort_of_typ (get_direct_typ y)) >>= fun var ->
      let e = Bil.Concat (x, y) in
      new_def var e >>| fun def ->
      Bil.Var var, insert blk def in
  aux exp blk

let flatten_blk original_blk =
  let rec flatten_elts (elts : Ir_blk.elt seq) (blk : blk term) =
    let rec flatten_jmp (jmp : Ir_jmp.t) (expseq : exp seq) (blk : blk term) =
      match Seq.next expseq with
      | Some (hd, tl) ->
        flatten_exp hd blk >>= fun (exp, blk) ->
        Ir_jmp.substitute jmp hd exp |> Term.update jmp_t blk |>
        flatten_jmp jmp tl
      | None -> !!blk in
    match Seq.next elts with
    | None -> !!blk
    | Some (hd, tl) -> match hd with
      | `Def def ->
        let before = Some (Term.tid def) in
        flatten_exp (Ir_def.rhs def) blk ~before >>= fun (exp, blk) ->
        Ir_def.with_rhs def exp |> Term.update def_t blk |>
        flatten_elts tl
      | `Jmp jmp -> flatten_jmp jmp (Ir_jmp.exps jmp) blk
      | `Phi _phi -> flatten_elts tl blk in
  flatten_elts (Ir_blk.elts original_blk) original_blk

let flatten_sub sub =
  let attrs = Term.attrs sub in
  Term.enum blk_t sub |> KB.Seq.map ~f:flatten_blk >>| fun blks ->
  Term.with_attrs begin Ir_sub.create ()
    ~args:(Term.enum arg_t sub |> Seq.to_list)
    ~blks:(Seq.to_list blks)
    ~name:(Ir_sub.name sub)
    ~tid:(Term.tid sub)
  end attrs

module KB = struct
  let flatten_blk = flatten_blk
  let flatten_sub = flatten_sub
end

let flatten_blk blk =
  let result = Toplevel.var "flatten-blk" in
  Toplevel.put result @@ flatten_blk blk;
  Toplevel.get result

let flatten_sub sub =
  let result = Toplevel.var "flatten-sub" in
  Toplevel.put result @@ flatten_sub sub;
  Toplevel.get result
