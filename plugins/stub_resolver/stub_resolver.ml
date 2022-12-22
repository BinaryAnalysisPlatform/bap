open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_core_theory
open Bap_knowledge

include Self ()
let package = "bap"

open KB.Syntax

type state = {
  groups : (tid, tid) Bap_relation.t;
  names  : (tid, string) Bap_relation.t;
  stubs  : Tid.Set.t;
  units  : Theory.Unit.t Tid.Map.t;
}

module Class = struct
  type t

  let t : (t,unit) KB.cls = Knowledge.Class.declare ~package "stubs" ()

  let links = Knowledge.Class.property t "stub-refs"
      ~package
      ~desc:"Describes unambiguous connections between stubs \
             and their implementations" @@
    Knowledge.Domain.mapping (module Tid) "links"
      ~equal:Tid.equal
      ~inspect:sexp_of_tid

  let stubs = KB.Class.property t "stub-tids"
      ~package
      ~desc:"The set of identified stubs" @@
    KB.Domain.powerset (module Tid) "tids"
end

let empty = {
  groups = Bap_relation.empty Tid.compare Tid.compare;
  names  = Bap_relation.empty Tid.compare String.compare;
  stubs  = Set.empty (module Tid);
  units  = Map.empty (module Tid);
}

let in_file file f =
  KB.Symbol.in_package file @@ fun () ->
  Theory.Unit.for_file file >>= fun unit ->
  let promise _ = !!(Some unit) in
  KB.promising Theory.Label.unit ~promise @@ fun () ->
  f unit

let is_stub sub =
  if Term.has_attr sub Sub.stub then !!true
  else match Term.(get_attr sub address, get_attr sub filename) with
    | None, _ | _, None -> !!true
    | Some addr, Some file -> in_file file @@ fun _unit ->
      Theory.Label.for_addr (Word.to_bitvec addr) >>= fun sub ->
      KB.collect (Value.Tag.slot Sub.stub) sub >>| Option.is_some

let aliases_of_sub s =
  KB.collect Theory.Label.aliases (Term.tid s) >>= fun aliases ->
  match Term.(get_attr s address, get_attr s filename) with
  | None, _ | _, None -> !!aliases
  | Some addr, Some file -> in_file file @@ fun _unit ->
    Theory.Label.for_addr (Word.to_bitvec addr) >>=
    KB.collect Theory.Label.aliases >>| Set.union aliases

let update_stubs t sub =
  is_stub sub >>| function
  | false -> t
  | true ->
    let tid = Term.tid sub in
    {t with stubs = Set.add t.stubs tid}

let update_units t sub =
  match Term.get_attr sub filename with
  | Some file -> in_file file @@ fun unit ->
    let tid = Term.tid sub in
    !!{t with units = Map.add_exn t.units tid unit}
  | None -> !!t

let should_link aliases ~link_only ~no_link =
  Set.(is_empty @@ inter aliases no_link) && begin
    Set.is_empty link_only ||
    not Set.(is_empty @@ inter aliases link_only)
  end

let update_names t sub ~link_only ~no_link =
  aliases_of_sub sub >>| fun aliases ->
  if should_link aliases ~link_only ~no_link then
    let groups, names =
      let tid = Term.tid sub in
      let stub = Set.mem t.stubs tid in
      let init = t.groups, t.names in
      Set.fold aliases ~init ~f:(fun (groups, names) name ->
          let groups =
            Bap_relation.findr names name |>
            List.fold ~init:groups ~f:(fun groups x ->
                if Tid.(x <> tid) then match stub, Set.mem t.stubs x with
                  | true,  true  -> groups
                  | true,  false -> Bap_relation.add groups tid x
                  | false, true  -> Bap_relation.add groups x tid
                  | false, false -> groups
                else groups) in
          groups, Bap_relation.add names tid name) in
    {t with groups; names}
  else t

let add t sub ~link_only ~no_link =
  update_stubs t sub >>= fun t ->
  update_units t sub >>= fun t ->
  update_names t sub ~link_only ~no_link

let find_pairs t =
  let add stub impl links = Map.change links stub ~f:(function
      | None -> Some impl
      | Some i when Tid.(impl <> i) -> None
      | Some _ as i -> i) in
  Bap_relation.matching t.groups Tid.Map.empty
    ~unmatched:(fun reason links -> match reason with
        | Non_injective_bwd (_, stub) -> Map.remove links stub
        | Non_injective_fwd (stubs, impl) ->
          List.fold stubs ~init:links ~f:(fun links stub ->
              match Bap_relation.findl t.groups stub with
              | _ :: _ :: _ -> links
              | _ -> add stub impl links))
    ~saturated:add

let resolve prog ~link_only ~no_link =
  Term.to_sequence sub_t prog |>
  Knowledge.Seq.fold ~init:empty
    ~f:(add ~link_only ~no_link) >>| fun state ->
  state, find_pairs state

let label_name x =
  KB.collect Theory.Label.name x >>| function
  | None -> Tid.to_string x
  | Some name -> name

let unit_path units x = match Map.find units x with
  | None -> !!"(none)"
  | Some unit -> KB.collect Theory.Unit.path unit >>| function
    | None -> "(none)"
    | Some path -> path

let log_stubs units stubs =
  Set.to_sequence stubs |>
  KB.Seq.iter ~f:(fun stub ->
      label_name stub >>= fun name ->
      unit_path units stub >>| fun path ->
      info "identified stub %s in unit %s" name path)

let log_links units links =
  Map.to_sequence links |>
  KB.Seq.iter ~f:(fun (x, y) ->
      label_name x >>= fun xname ->
      label_name y >>= fun yname ->
      unit_path units x >>= fun xpath ->
      unit_path units y >>| fun ypath ->
      info "resolved stub %s in unit %s to implementation %s in unit %s%!"
        xname xpath yname ypath)

let provide prog ~link_only ~no_link =
  Knowledge.Object.create Class.t >>= fun obj ->
  resolve prog ~link_only ~no_link >>= fun ({stubs; units},links) ->
  KB.sequence [
    log_stubs units stubs;
    log_links units links;
    KB.provide Class.links obj links;
    KB.provide Class.stubs obj stubs;
  ] >>= fun () ->
  KB.return obj

let run
    ?(link_only = String.Set.empty)
    ?(no_link = String.Set.empty)
    prog =
  Toplevel.current () |>
  Knowledge.run Class.t (provide prog ~link_only ~no_link) |> function
  | Ok (v,_) -> v
  | Error cnf ->
    error "%a\n" Knowledge.Conflict.pp cnf;
    KB.Value.empty Class.t

type t = (Class.t,unit) KB.cls KB.Value.t
let links = KB.Value.get Class.links
let stubs = KB.Value.get Class.stubs
