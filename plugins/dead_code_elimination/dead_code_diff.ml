open Core_kernel
open Bap.Std
open Regular.Std
include Self()

type jmp_diff = Target of exp | Cond of exp
[@@deriving bin_io, sexp, compare]

type t = {
  dead : Tid.Set.t;
  defs : exp Tid.Map.t;
  jmps : jmp_diff list Tid.Map.t;
} [@@deriving bin_io, sexp, compare]

type diff = t
type snap = [`Def of def term | `Jmp of jmp term] Tid.Table.t

let empty = {dead = Tid.Set.empty; defs = Tid.Map.empty; jmps = Tid.Map.empty}

let empty_snap : snap = Tid.Table.create ()

let snapshot snap sub =
  (object
    inherit [unit] Term.visitor
    method! enter_def def () =
      Hashtbl.add_exn snap (Term.tid def) (`Def def)
    method! enter_jmp jmp () =
      Hashtbl.add_exn snap (Term.tid jmp) (`Jmp jmp)
  end)#visit_sub sub ();
  snap

let snapshot = List.fold ~init:empty_snap ~f:snapshot

let create prog subs =
  let snap = snapshot subs in
  let find_jmp' tid = match Hashtbl.find snap tid with
    | Some (`Jmp j) -> j
    | _ -> assert false in
  let indirect_target jmp = match Jmp.kind jmp with
    | Ret _ | Int _ | Goto (Direct _) -> None
    | Goto (Indirect a) -> Some a
    | Call c -> match Call.target c with
      | Indirect a -> Some a
      | _ -> None in
  (object
    inherit [diff] Term.visitor

    method! enter_def d ({dead;defs} as diff) =
      let tid = Term.tid d in
      match Hashtbl.find snap tid with
      | None -> {diff with dead = Set.add dead tid }
      | Some (`Def d') ->
        if Exp.equal (Def.rhs d) (Def.rhs d')
        then diff
        else {diff with defs = Map.add defs tid (Def.rhs d')}
      | _ -> assert false

    method! enter_jmp jmp ({jmps} as diff) =
      let tid = Term.tid jmp in
      let jmp' = find_jmp' tid in
      let jmp_diffs =
        match indirect_target jmp, indirect_target jmp' with
        | Some a, Some a' ->
          if Exp.equal a a' then []
          else [Target a']
        | _ -> [] in
      let jmp_diffs =
        if Exp.equal (Jmp.cond jmp) (Jmp.cond jmp') then jmp_diffs
        else Cond (Jmp.cond jmp') :: jmp_diffs in
      match jmp_diffs with
      | [] -> diff
      | jmp_diffs -> {diff with jmps = Map.add jmps tid jmp_diffs}
  end)#run prog empty

let apply prog {dead; defs; jmps} =
  let replace_indirect jmp x =
    let kind = match Jmp.kind jmp with
      | Ret _ | Int _ | Goto (Direct _) as kind -> kind
      | Goto (Indirect _) -> Goto (Indirect x)
      | Call c as kind -> match Call.target c with
        | Indirect _ -> Call (Call.with_target c (Indirect x))
        | _ -> kind in
    Jmp.with_kind jmp kind in
  Term.map sub_t prog ~f:(Term.map blk_t ~f:(fun blk ->
      Term.filter_map def_t blk ~f:(fun def ->
          let tid = Term.tid def in
          if Set.mem dead tid then None
          else match Map.find defs tid with
            | None -> Some def
            | Some rhs -> Some (Def.with_rhs def rhs)) |>
      Term.map jmp_t ~f:(fun jmp ->
          match Map.find jmps (Term.tid jmp) with
          | None -> jmp
          | Some diffs ->
            List.fold diffs ~init:jmp ~f:(fun jmp -> function
                | Target t -> replace_indirect jmp t
                | Cond c -> Jmp.with_cond jmp c))))

include Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, sexp, compare]
    let version = version
    let module_name = None
    let hash = Hashtbl.hash
    let pp fmt t =
      Format.fprintf fmt "dead tids: ";
      Set.iter t.dead ~f:(Format.fprintf fmt "%a " Tid.pp);
      Format.pp_print_newline fmt ();
      if not (Map.is_empty t.defs) then
        let () = Format.fprintf fmt "substitutions: " in
        Map.iteri t.defs ~f:(fun ~key:tid ~data:exp ->
            Format.fprintf fmt "(%a %a) " Tid.pp tid Exp.pp exp);
        Format.pp_print_newline fmt ()
  end)
