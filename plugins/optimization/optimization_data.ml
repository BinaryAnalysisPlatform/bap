open Core_kernel
open Bap.Std
open Regular.Std

type jmp_update = {
  cond : exp;
  kind : jmp_kind;
}

type update = Rhs of exp | Jmp of jmp_update

type t = {
  deads   : Tid.Set.t;
  updates : update Tid.Map.t;
}

let updated_term = Value.Tag.register (module Unit)
    ~name:"updated-term"
    ~uuid:"d21d76fa-12dd-470f-902e-f0e890e382d3"

let mark_updated t = Term.set_attr t updated_term ()
let is_updated t = Option.is_some (Term.get_attr t updated_term)

let drop_index = (object
  inherit Exp.mapper
  method! map_sym var = Var.base var
end)#map_exp

let updates_of_sub sub =
  let fold t cls init ~f = Seq.fold (Term.enum cls t) ~init ~f in
  let update_rhs updates d =
    let rhs = drop_index (Def.rhs d) in
    Map.add updates (Term.tid d) (Rhs rhs) in
  let update_jmp updates j =
    let j = Jmp.map_exp ~f:drop_index j in
    let data = Jmp {cond = Jmp.cond j; kind = Jmp.kind j} in
    Map.add updates (Term.tid j) data in
  let add_if add updates t =
    if is_updated t then add updates t else updates in
  fold sub blk_t Tid.Map.empty ~f:(fun updates b ->
      fold b def_t updates ~f:(add_if update_rhs) |>
      fold b jmp_t ~f:(add_if update_jmp))

let create ~deads sub = {deads; updates = updates_of_sub sub}

let apply sub {deads; updates} =
  let apply_to_def d =
    if Set.mem deads (Term.tid d) then None
    else
      match Map.find updates (Term.tid d) with
      | None -> Some d
      | Some (Rhs e) -> Some (Def.with_rhs d e)
      | _ -> assert false in
  let apply_to_jmp j =
    match Map.find updates (Term.tid j) with
    | None -> j
    | Some (Jmp {cond; kind}) ->
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
