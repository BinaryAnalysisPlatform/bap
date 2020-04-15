open Core_kernel
open Bap.Std
open Bap_core_theory
open Bap_knowledge

include Self ()

open KB.Syntax

type groups = int Tid.Map.t
type names = String.Set.t Int.Map.t

type t = {
  groups : groups;
  names  : names;
  next   : int;
  stubs  : Tid.Set.t;
}

let tids = Knowledge.Domain.mapping (module Tid) "tids"
    ~equal:Tid.equal
    ~inspect:sexp_of_tid

let slot = Knowledge.Class.property
    Theory.Program.cls "stubs"
    tids
    ~persistent:(Knowledge.Persistent.of_binable (module struct
                   type t = tid Tid.Map.t
                   [@@deriving bin_io]
                 end))
    ~public:true
    ~desc:"The mapping from stubs to real symbols"

let cls = Knowledge.Slot.cls slot

let empty = {
  groups = Map.empty (module Tid);
  names  = Map.empty (module Int);
  next   = 0;
  stubs  = Set.empty (module Tid)
}

let is_stub sub =
  KB.collect (Value.Tag.slot Sub.stub) (Term.tid sub) >>= function
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
        if Set.(is_empty @@ inter aliases aliases') then
          groups
        else group :: groups)

let add t sub =
  update_stubs t sub >>= fun t ->
  aliases_of_sub sub >>| fun als ->
  match find_groups t.names als with
  | [] ->
    let groups = Map.add_exn t.groups (Term.tid sub) t.next in
    let names  = Map.add_exn t.names t.next als in
    let next   = t.next + 1 in
    {t with groups; names; next}
  | [id] ->
    let groups = Map.add_exn t.groups (Term.tid sub) id in
    let names = Map.update t.names id ~f:(function
        | None -> assert false
        | Some als' -> Set.union als als') in
    { t with names; groups }
  | groups ->
    let aliases = List.fold groups ~init:als ~f:(fun als id ->
        Set.union als (Map.find_exn t.names id)) in
    let representative =
      Option.value_exn (
        List.min_elt groups ~compare:Int.compare) in
    let names = List.fold groups
        ~init:t.names ~f:Map.remove in
    let names  = Map.add_exn names representative aliases in
    let groups = Map.map t.groups ~f:(fun id ->
        if List.mem groups id ~equal:Int.equal then
          representative
        else id) in
    {t with names; groups;}

let find_pairs t =
  let is_stub t tid = Set.mem t.stubs tid in
  let pairs =
    Map.fold t.groups ~init:Int.Map.empty
      ~f:(fun ~key:tid ~data:id xs ->
          Map.update xs id ~f:(function
              | None -> [tid]
              | Some tids -> tid :: tids )) in
  Map.fold pairs ~init:(Map.empty (module Tid))
    ~f:(fun ~key:_ ~data:tids pairs ->
        match tids with
        | [x; y] ->
          (match is_stub t x, is_stub t y with
           | true, false -> Map.add_exn pairs x y
           | false, true -> Map.add_exn pairs y x
           | _ -> pairs)
        | _ -> pairs)

let run prog =
  Knowledge.Seq.fold ~init:empty
    (Term.to_sequence sub_t prog) ~f:add >>|
  find_pairs

let provide prog =
  Knowledge.Object.create cls >>= fun obj ->
  run prog >>= fun links ->
  KB.provide slot obj links >>= fun () ->
  KB.return obj

let find_pairs prog =
  match Knowledge.run cls (provide prog) (Toplevel.current ()) with
  | Ok (v,_) -> Knowledge.Value.get slot v
  | Error cnf ->
    error "%a\n" Knowledge.Conflict.pp cnf;
    Map.empty (module Tid)
