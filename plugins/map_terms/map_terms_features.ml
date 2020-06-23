open Core_kernel
open Bap.Std
open Bap_bml

class content_visitor = object(self)
  inherit [bool] Term.visitor

  method! enter_term cls t x =
    Term.switch cls t
      ~program:(fun t -> self#enter_program t x)
      ~sub:(fun t -> self#enter_sub t x)
      ~arg:(fun t -> self#enter_arg t x)
      ~blk:(fun t -> self#enter_blk t x)
      ~phi:(fun t -> self#enter_phi t x)
      ~def:(fun t -> self#enter_def t x)
      ~jmp:(fun t -> self#enter_jmp t x)
end

let word_of_string width str =
  Option.try_with (fun () ->
      Addr.of_int64 ~width (Int64.of_string str))

let term_attr test tag x =
  object inherit content_visitor
    method! enter_term cls t _ = match Term.get_attr t tag with
      | None -> false
      | Some y -> test x y
  end

let term_addr =
  let test_addr str x =
    match word_of_string (Addr.bitwidth x) str with
    | Some y -> Addr.equal x y
    | None -> false in
  term_attr test_addr address

let term_tid x =
  let test = match Tid.from_string x with
    | Error _ -> fun _ -> false
    | Ok tid -> fun t -> Tid.(t = tid) in
  object inherit content_visitor
    method! enter_term cls t _ = test (Term.tid t)
  end

let term_name x =
  object inherit [bool] Term.visitor
    method! enter_term cls t _ =
      String.equal (Term.name t) x
  end

let def_lhs x =
  object inherit content_visitor
    method! enter_def t _ =
      String.equal (Var.name (Def.lhs t)) x
  end

let def_uses x =
  object inherit content_visitor
    method! enter_def t _ =
      Set.exists (Def.free_vars t)
        ~f:(fun y -> String.equal x (Var.name y))
  end

let exists_child par cls tid =
  Term.to_sequence cls par |>
  Seq.exists ~f:(fun t -> Tid.equal (Term.tid t) tid)

let find_parent sub child_tid =
  if exists_child sub blk_t child_tid then Some `Sub
  else
    let blocks = Term.to_sequence blk_t sub in
    let has cls b = exists_child b cls child_tid in
    let has_jmp = has jmp_t in
    let has_def = has def_t in
    let has_phi = has phi_t in
    let has_child b = has_jmp b || has_def b || has_phi b in
    Option.value_map ~default:None ~f:(fun b -> Some (`Blk b))
      (Seq.find blocks ~f:has_child )

let parent x =
  match Tid.from_string x with
  | Error _ ->
    eprintf "Didn't find term with a name %s" x;
    new content_visitor
  | Ok tid ->
    object
      inherit content_visitor
      val mutable marked : tid option = None

      method! enter_blk b _ = match marked with
        | None -> false
        | Some tid -> Tid.equal (Term.tid b) tid

      method! enter_sub s _ =
        match find_parent s tid with
        | Some `Sub -> true
        | Some (`Blk b) -> marked <- Some (Term.tid b); false
        | None -> false
    end

let init () =
  let (:=) = Predicates.Unary.register in
  "term-addr" := term_addr;
  "term-tid"  := term_tid;
  "term-name" := term_name;
  "term-parent" := parent;
  "def-lhs"  := def_lhs;
  "def-uses" := def_uses;
