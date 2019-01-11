open Core_kernel

module Cst = Parsexp.Cst
module Loc = Bap_lisp__loc
module Index = Bap_lisp__index

module Id = Index.Make()
module Eq = Index.Make()

type error = Bad_sexp of (string * Parsexp.Parse_error.t)

type ('a,'i,'e) interned = ('a,'i,'e) Index.interned = {
  data : 'a;
  id : 'i;
  eq : 'e;
}
type 'a indexed = ('a,Id.t,Eq.t) interned

type tree = token indexed
and token = Atom of string | List of tree list

type t = {
  lastid : Id.t;
  lasteq : Eq.t;
  hashed : Eq.t Sexp.Map.t;
  equivs : Eq.t Id.Map.t;
  origin : string Id.Map.t;
  ranges : Loc.range Id.Map.t;
  source : tree list String.Map.t;
  rclass : Id.t Id.Map.t;
}

let empty = {
  lastid = Id.null;
  lasteq = Eq.null;
  hashed = Sexp.Map.empty;
  equivs = Id.Map.empty;
  origin = Id.Map.empty;
  ranges = Id.Map.empty;
  source = String.Map.empty;
  rclass = Id.Map.empty;
}

let nextid p = {
  p with lastid = Id.next p.lastid
}

let nexteq p = {p with lasteq = Eq.next p.lasteq}

let rec repr s id = match Map.find s.rclass id with
  | None -> id
  | Some id' -> if id = id' then id else repr s id'

let hashcons p sexp =
  match Map.find p.hashed sexp with
  | Some eq -> p,eq
  | None ->
    let p = nexteq p in
    {p with hashed = Map.add p.hashed ~key:sexp ~data:p.lasteq},
    p.lasteq

let unify p eq =
  {p with equivs = Map.add p.equivs ~key:p.lastid ~data:eq}


let nopos = Parsexp.Positions.beginning_of_file
let norange = Parsexp.Positions.make_range_incl
    ~start_pos:nopos
    ~last_pos:nopos

let getrange pos parents child = match pos with
  | None -> norange
  | Some pos ->
    Parsexp.Positions.find_sub_sexp_in_list_phys
      pos parents ~sub:child |> function
    | None -> norange
    | Some range -> range

let add_range p data =
  {p with ranges = Map.add p.ranges ~key:p.lastid ~data}

let of_cst p sexps =
  let newterm p r s =
    let p = nextid p in
    let p = add_range p r in
    hashcons p (Cst.Forget.t s) in
  let rec of_sexp p s = match s with
    | Cst.Comment _ -> p,None
    | Cst.Sexp (Atom {atom; loc; unescaped} as s) ->
      let p,eq = newterm p loc s in
      let x = Option.value unescaped ~default:atom in
      unify p eq, Some {data=Atom x; id=p.lastid; eq}
    | Cst.Sexp (List {elements=xs; loc} as s)  ->
      let p,data = List.fold ~init:(p,[]) ~f:(fun (p,xs) sexp ->
          match of_sexp p sexp with
          | p,None -> p,xs
          | p,Some x -> p,(x::xs)) xs in
      let p,eq = newterm p loc s in
      unify p eq,Some {data = List (List.rev data); id=p.lastid; eq} in
  let p,trees = List.fold sexps ~init:(p,[]) ~f:(fun (p,xs) x ->
      match of_sexp p x with
      | p,None -> p,xs
      | p,Some x -> p,(x::xs)) in
  p,List.rev trees


let of_sexps ?pos p sexps =
  let getrange = getrange pos sexps in
  let newterm p s =
    let p = nextid p in
    let p = add_range p (getrange s) in
    hashcons p s in
  let rec of_sexp p s = match s with
    | Sexp.Atom x ->
      let p,eq = newterm p s in
      unify p eq,{data=Atom x; id=p.lastid; eq}
    | Sexp.List xs ->
      let p,data = List.fold ~init:(p,[]) ~f:(fun (p,xs) sexp ->
          let p,x = of_sexp p sexp in
          p,(x::xs)) xs in
      let p,eq = newterm p s in
      unify p eq,{data = List (List.rev data); id=p.lastid; eq} in
  let p,trees = List.fold sexps ~init:(p,[]) ~f:(fun (p,xs) x ->
      let p,x = of_sexp p x in
      p,(x::xs)) in
  p,List.rev trees


let add_origin origins origin trees =
  let rec add origins token =
    let origins = Map.add origins ~key:token.id ~data:origin in
    match token.data with
    | Atom _ -> origins
    | List tokens -> List.fold tokens ~init:origins ~f:add in
  List.fold ~init:origins ~f:add trees

let load p filename =
  let source = In_channel.read_all filename in
  match Parsexp.Many_cst.parse_string source with
  | Error err -> Error (Bad_sexp (filename,err))
  | Ok cst ->
    let p,tree = of_cst p cst in
    let origin = add_origin p.origin filename tree in
    Ok {
      p with
      origin;
      source = Map.add p.source ~key:filename ~data:tree
    }

let find p filename = Map.find p.source filename
let range p id = match Map.find p.ranges (repr p id) with
  | None -> norange
  | Some rng -> rng

let filename p id  = match Map.find p.origin (repr p id) with
  | None -> "/unknown/"
  | Some file -> file

let loc p tree = Loc.{
    file = filename p tree;
    range = range p tree;
  }

let has_loc p id = Map.mem p.origin (repr p id)

let lastid s = s.lastid
let lasteq s = s.lasteq

let fold p ~init ~f = Map.fold ~init p.source ~f:(fun ~key ~data user ->
    f key data user)

let derived p ~from id =
  if Id.null = from then {
    p with lastid = Id.max id p.lastid
  } else {
    p with
    lastid = Id.max id p.lastid;
    rclass = Map.add p.rclass ~key:id ~data:from;
  }

let pp_error ppf (Bad_sexp (filename,err)) =
  Parsexp.Parse_error.report ppf ~filename err

let rec sexp_of_tree = function
  | {data=List xs} -> Sexp.List (List.map xs ~f:sexp_of_tree)
  | {data=Atom x} -> Sexp.Atom x

let pp_tree ppf t =
  Sexp.pp_hum ppf (sexp_of_tree t)
