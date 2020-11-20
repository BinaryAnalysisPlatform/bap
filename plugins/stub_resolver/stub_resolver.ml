open Core_kernel
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
  stubs  = Set.empty (module Tid);
  next   = 0;
}

let is_stub sub =
  if Term.has_attr sub Sub.stub then KB.return true
  else match Term.get_attr sub address with
    | None -> KB.return true
    | Some addr ->
      Theory.Label.for_addr (Word.to_bitvec addr) >>= fun sub ->
      KB.collect (Value.Tag.slot Sub.stub) sub >>= function
      | None -> KB.return false
      | Some () -> KB.return true

let aliases_of_sub s = KB.collect Theory.Label.aliases (Term.tid s)

let update_stubs t sub =
  is_stub sub >>| fun is_stub ->
  if is_stub
  then { t with stubs = Set.add t.stubs (Term.tid sub) }
  else t

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
  aliases_of_sub sub >>| fun aliases ->
  match find_groups t.names aliases with
  | [] ->
    let groups = Map.add_exn t.groups (Term.tid sub) t.next in
    let names  = Map.add_exn t.names t.next aliases in
    { t with groups; names; next = t.next + 1 }
  | [id] ->
    let groups = Map.add_exn t.groups (Term.tid sub) id in
    let names = Map.update t.names id ~f:(function
        | None -> assert false
        | Some als' -> Set.union aliases als') in
    { t with names; groups }
  | groups ->
    let grp = pick_representative groups in
    let aliases = Set.union aliases (unite_names t groups) in
    let names = List.fold groups ~init:t.names ~f:Map.remove in
    let names = Map.add_exn names grp aliases in
    let groups = redirect t ~from:groups ~to_:grp in
    {t with names; groups;}

let collect_by_group_id groups =
  Map.fold groups ~init:Int.Map.empty
    ~f:(fun ~key:tid ~data:id xs ->
        Map.update xs id ~f:(function
            | None -> [tid]
            | Some tids -> tid :: tids ))

let unambiguous_pairs stubs xs =
  let is_stub tid = Set.mem stubs tid in
  Map.fold xs ~init:(Map.empty (module Tid))
    ~f:(fun ~key:_group_id ~data:tids pairs ->
        match tids with
        | [x; y] ->
          begin
            match is_stub x, is_stub y with
            | true, false -> Map.add_exn pairs x y
            | false, true -> Map.add_exn pairs y x
            | _ -> pairs
          end
        | _ -> pairs)

let find_pairs t =
  collect_by_group_id t.groups |>
  unambiguous_pairs t.stubs

let resolve prog =
  Term.to_sequence sub_t prog |>
  Knowledge.Seq.fold ~init:empty ~f:add >>| fun state ->
  state, find_pairs state

let provide prog =
  Knowledge.Object.create Class.t >>= fun obj ->
  resolve prog >>= fun ({stubs},links) ->
  KB.sequence [
    KB.provide Class.links obj links;
    KB.provide Class.stubs obj stubs;
  ] >>= fun () ->
  KB.return obj

let run prog =
  match Knowledge.run Class.t (provide prog) (Toplevel.current ()) with
  | Ok (v,_) -> v
  | Error cnf ->
    error "%a\n" Knowledge.Conflict.pp cnf;
    KB.Value.empty Class.t

type t = (Class.t,unit) KB.cls KB.Value.t
let links = KB.Value.get Class.links
let stubs = KB.Value.get Class.stubs
