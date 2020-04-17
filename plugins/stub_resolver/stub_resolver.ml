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
    Theory.Program.cls "stubs" tids
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
  stubs  = Set.empty (module Tid);
  next   = 0;
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
    List.min_elt groups ~compare:Int.compare |>
    Option.value_exn

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
  Knowledge.Seq.fold ~init:empty
    (Term.to_sequence sub_t prog) ~f:add >>|
  find_pairs

let provide prog =
  Knowledge.Object.create cls >>= fun obj ->
  resolve prog >>= fun links ->
  KB.provide slot obj links >>= fun () ->
  KB.return obj

let run prog =
  match Knowledge.run cls (provide prog) (Toplevel.current ()) with
  | Ok (v,_) -> Knowledge.Value.get slot v
  | Error cnf ->
    error "%a\n" Knowledge.Conflict.pp cnf;
    Map.empty (module Tid)
