open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_core_theory
open Bap_knowledge

include Self ()
let package = "bap"

open KB.Syntax

type groups = int Tid.Map.t
type names = String.Set.t Int.Map.t

type state = {
  groups : groups;
  names  : names;
  next   : int;
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
  groups = Map.empty (module Tid);
  names  = Map.empty (module Int);
  units  = Map.empty (module Tid);
  stubs  = Set.empty (module Tid);
  next   = 0;
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

let find_groups names aliases =
  Map.fold names ~init:[]
    ~f:(fun ~key:group ~data:aliases' groups ->
        if Set.(is_empty @@ inter aliases aliases')
        then groups
        else group :: groups)

let unite_names t groups =
  List.fold groups ~init:(Set.empty (module String))
    ~f:(fun als id ->
        Set.union als (Map.find_exn t.names id))

let pick_representative = function
  | [] -> assert false
  | groups ->
    Option.value_exn (List.min_elt groups ~compare:Int.compare)


let redirect t ~from ~to_ =
  Map.map t.groups ~f:(fun id ->
      if List.mem from id ~equal:Int.equal
      then to_
      else id)

let add t sub =
  update_stubs t sub >>= fun t ->
  update_units t sub >>= fun t ->
  aliases_of_sub sub >>| fun aliases ->
  match find_groups t.names aliases with
  | [] ->
    let groups = Map.add_exn t.groups (Term.tid sub) t.next in
    let names  = Map.add_exn t.names t.next aliases in
    {t with groups; names; next = t.next + 1}
  | [id] ->
    let groups = Map.add_exn t.groups (Term.tid sub) id in
    let names = Map.update t.names id ~f:(function
        | None -> assert false
        | Some als' -> Set.union aliases als') in
    {t with names; groups}
  | groups ->
    let grp = pick_representative groups in
    let aliases = Set.union aliases (unite_names t groups) in
    let names = List.fold groups ~init:t.names ~f:Map.remove in
    let names = Map.add_exn names ~key:grp ~data:aliases in
    let groups = redirect t ~from:groups ~to_:grp in
    {t with names; groups}

let collect_by_group_id stubs groups =
  Map.fold groups ~init:Int.Map.empty
    ~f:(fun ~key:tid ~data:id xs ->
        Map.update xs id ~f:(function
            | None -> [tid]
            | Some tids -> tid :: tids)) |>
  Map.map ~f:(List.partition_tf ~f:(Set.mem stubs))

let unambiguous_pairs names xs ~link_only ~no_link =
  let should_link id names =
    let names = Map.find_exn names id in
    Set.(is_empty @@ inter names no_link) && begin
      Set.is_empty link_only ||
      not Set.(is_empty @@ inter names link_only)
    end in
  let add y pairs x = Map.add_exn pairs x y in
  Map.fold xs ~init:(Map.empty (module Tid))
    ~f:(fun ~key:id ~data:(stubs, impls) init ->
        match impls with
        | [y] when should_link id names ->
          List.fold stubs ~init ~f:(add y)
        | _ -> init)

let find_pairs t ~link_only ~no_link =
  unambiguous_pairs t.names ~link_only ~no_link @@
  collect_by_group_id t.stubs t.groups

let resolve prog ~link_only ~no_link =
  Term.to_sequence sub_t prog |>
  Knowledge.Seq.fold ~init:empty ~f:add >>| fun state ->
  state, find_pairs state ~link_only ~no_link

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
