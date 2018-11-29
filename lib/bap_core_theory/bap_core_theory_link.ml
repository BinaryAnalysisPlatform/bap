open Core_kernel
open Bap.Std

open Bap_knowledge

type info = {
  name : string option;
  ivec : int option;
  addr : addr option;
} [@@deriving compare]

type links = {
  links  : info Label.Map.t;
  names  : label String.Map.t;
  addrs  : label Addr.Map.t;
  ivecs  : label Int.Map.t;
}

type t = links

type _ field = Link : <
    get : links -> ('a,label,'c) Map.t;
    prj : info -> 'a option;
    put : 'a -> info -> info;
    set : ('a,label,'c) Map.t -> links -> links
  > -> 'a field


module Domain = struct
  type t = links


  let empty = {
    links = Label.Map.empty;
    names = String.Map.empty;
    addrs = Addr.Map.empty;
    ivecs = Int.Map.empty;
  }

  let partial _ _ = Domain.Order.NC
  let inspect _ = Sexp.Atom "<links>"
end

let link_t = Semantics.declare ~name:"links" (module Domain)
let links = Knowledge.declare
    ~public:true
    ~name:"edu.cmu.ece.bap/linker"
    ~desc:"linker information"
    link_t


let empty = Domain.empty

let noinfo = {name=None; ivec=None; addr=None}

let field obj = Link obj

let update (Link field) link item s =
  let links = Map.update s.links link ~f:(function
      | None -> field#put item noinfo
      | Some info -> field#put item info) in
  let infos = Map.add (field#get s) ~key:item ~data:link in
  field#set infos {s with links}

let lookup (Link field) link s = match Map.find s.links link with
  | Some info -> field#prj info
  | None -> None

let next = Label.Generator.fresh



let name = field @@ object
    method get s = s.names
    method set names s = {s with names}
    method prj i = i.name
    method put s info = {info with name = Some s}
  end

let addr = field @@ object
    method get s = s.addrs
    method set addrs s = {s with addrs}
    method prj i = i.addr
    method put s info = {info with addr = Some s}
  end

let ivec = field @@ object
    method get s = s.ivecs
    method set ivecs s = {s with ivecs}
    method prj i = i.ivec
    method put s info = {info with ivec = Some s}
  end

open Knowledge.Syntax


let link k x : label knowledge =
  Knowledge.collect links Label.root >>= fun s ->
  Label.Generator.fresh >>= fun l ->
  let s = update k l x s in
  Knowledge.provide links Label.root s >>= fun () ->
  Knowledge.return l

let resolve k x =
  Knowledge.collect links Label.root >>| fun s ->
  lookup k x s


module Syntax = struct
  let link_addr = link addr
  let link_name = link name
  let link_ivec = link ivec
  let resolve_addr = resolve addr
  let resolve_name = resolve name
  let resolve_ivec = resolve ivec
end
