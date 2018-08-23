open Core_kernel
open Bap.Std
open Regular.Std
include Self()

type jmp_change = {
  cond : exp;
  kind : jmp_kind;
} [@@deriving bin_io, sexp, compare]

type change = Rhs of exp | Jmp of jmp_change
[@@deriving bin_io, sexp, compare]

type diff = Update of change | Kill [@@deriving bin_io, sexp, compare]
type t = diff Tid.Map.t  [@@deriving bin_io, sexp, compare]

let empty = Tid.Map.empty

let drop_index = (object
  inherit Exp.mapper
  method! map_sym var = Var.base var
end)#map_exp

let changes_in_def d1 d2 =
  let different = not (phys_equal d1 d2) && Exp.(Def.rhs d1 <> Def.rhs d2) in
  if different then
    let d2 = Def.map_exp ~f:drop_index d2 in
    Some (Term.tid d1, Update (Rhs (Def.rhs d2)))
  else None

let changes_in_jmp j1 j2 =
  let different = not (phys_equal j1 j2) && Jmp.(j1 <> j2) in
  if different then
    let j2 = Jmp.map_exp j2 ~f:drop_index in
    let update = Update (Jmp {kind=Jmp.kind j2; cond=Jmp.cond j2}) in
    Some (Term.tid j2, update)
  else None

let elts ~add elts = Seq.fold ~init:empty elts ~f:add

let add_blk_elt acc elt = match elt with
  | `Def def -> Map.add acc (Term.tid def) elt
  | `Jmp jmp -> Map.add acc (Term.tid jmp) elt
  | _ -> acc

let add_elt acc t = Map.add acc (Term.tid t) t
let blk_elts b = elts ~add:add_blk_elt (Blk.elts b)
let sub_elts s = elts ~add:add_elt (Term.enum blk_t s)

let diff_of_blk b1 b2 =
  let add_changes xs diff = match xs with
    | None -> diff
    | Some (tid,changes) -> Map.add diff tid changes in
  let kill d = Some (Term.tid d, Kill) in
  let (++) = add_changes in
  let was = blk_elts b1 and now = blk_elts b2 in
  Map.fold2 was now ~init:empty ~f:(fun ~key:_ ~data acc -> match data with
      | `Both (`Def d1, `Def d2) -> changes_in_def d1 d2 ++ acc
      | `Both (`Jmp j1, `Jmp j2) -> changes_in_jmp j1 j2 ++ acc
      | `Left (`Def d) -> kill d ++ acc
      | _ -> acc)

let diff_of_sub s1 s2 =
  let add ~key ~data acc = Map.add ~key ~data acc in
  let was = sub_elts s1 and now = sub_elts s2 in
  Map.fold2 was now ~init:empty ~f:(fun ~key:_ ~data acc ->
      match data with
      | `Both (b1, b2) -> diff_of_blk b1 b2 |> Map.fold ~init:acc ~f:add
      | _ -> acc)

let apply sub diff =
  let apply_to_def d =
    match Map.find diff (Term.tid d) with
    | None -> Some d
    | Some Kill -> None
    | Some (Update (Rhs e)) -> Some (Def.with_rhs d e)
    | _ -> assert false in
  let apply_to_jmp j =
    match Map.find diff (Term.tid j) with
    | None -> j
    | Some (Update (Jmp {cond; kind})) ->
      let j = Jmp.with_cond j cond in
      Jmp.with_kind j kind
    | _ -> assert false in
  Term.map blk_t sub ~f:(fun b ->
      Term.filter_map def_t b ~f:apply_to_def |>
      Term.map jmp_t ~f:apply_to_jmp)

include Data.Make(struct
    type nonrec t = t
    let version = "0.1"
  end)
