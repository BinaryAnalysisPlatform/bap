open Bap.Std
open Core_kernel
include Self()


let get_direct_typ (e : exp) : Type.t = match e with
  | Bil.Var v -> Var.typ v
  | Bil.Unknown (_,t) -> t
  | Bil.Int w -> Type.Imm (Word.bitwidth w)
  | _ -> failwith "the expression is not flattened"

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
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Store (x, y, z, endian, s) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let z, blk = aux z blk in
      let vtype = Type.Imm (Size.in_bits s) in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Store (x, y, z, endian, s) in
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.BinOp (b, x, y) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let vtype = get_direct_typ x in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.BinOp(b, x, y) in
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.UnOp (u, x) ->
      let x, blk = aux x blk in
      let vtype = get_direct_typ x in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.UnOp(u, x) in
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Var _
    | Bil.Int _ -> exp, blk
    | Bil.Cast (c, n, x) ->
      let x, blk = aux x blk in
      let vtype = Type.Imm n in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Cast (c, n, x) in
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Let (v, x, y) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let vtype = Var.typ v in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Let (v, x, y) in
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Unknown (_, _) -> exp, blk
    | Bil.Ite (x, y, z) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let z, blk = aux z blk in
      let vtype = get_direct_typ y in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Ite (x, y, z) in
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Extract (n, p, x) ->
      let x, blk = aux x blk in
      let vtype = get_direct_typ x in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Extract (n, p, x) in
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def
    | Bil.Concat (x, y) ->
      let x, blk = aux x blk in
      let y, blk = aux y blk in
      let vtype = get_direct_typ x in
      let var = Var.create ~is_virtual ~fresh "flt" vtype in
      let e = Bil.Concat (x, y) in
      let def = Def.create var e in
      Bil.Var var,
      Term.prepend def_t ~before blk def in
  aux exp blk

let flatten_blk original_blk =
  let rec flatten_elts (elts : Blk.elt seq) (blk : blk term) =
    let rec flatten_jmp (jmp : Jmp.t) (expseq : exp seq) (blk : blk term) =
      match Seq.next expseq with
      | Some(hd, tl) ->
        let exp, blk = flatten_exp hd blk (Term.tid jmp) in
        Jmp.substitute jmp hd exp |> Term.update jmp_t blk |>
        flatten_jmp jmp tl
      | None -> blk in

    match Seq.next elts with
    | Some (hd, tl) -> (match hd with
        | `Def def ->
          let exp, blk = flatten_exp (Def.rhs def) blk (Term.tid def) in
          Def.with_rhs def exp |> Term.update def_t blk |>
          flatten_elts tl
        | `Jmp jmp -> flatten_jmp jmp (Jmp.exps jmp) blk
        | `Phi phi -> flatten_elts tl blk)
    | None -> blk in

  flatten_elts (Blk.elts original_blk) original_blk

let flatten_sub =
  Term.map blk_t ~f:flatten_blk

let main = Project.map_program ~f:(Term.map sub_t ~f:flatten_sub)

;;
Config.manpage [
  `S "DESCRIPTION";
  `P "Flatten all AST in the program.";
  `S "EXAMPLE";
  `Pre {|
  ;; input 
  #10 := 11 * (#9 + 13) - 17
  ;; output
  #11 := #9 + 13
  #12 := 11 * #11 
  #10 := #12 - 17
  |}

]

let () = Config.when_ready (fun _ -> Project.register_pass main);;
