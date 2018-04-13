open Core_kernel.Std
open Bap_future.Std
open Regular.Std

module Service_t = struct
  type t = {
    uuid : string;
    name : string;
    desc : string;
  } [@@deriving bin_io, compare, fields, sexp]

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Service"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s %s" t.name t.desc
    end)
end

type service = Service_t.t  [@@deriving bin_io, compare, sexp]

module Provider_t = struct
  type t = {
    name : string;
    desc : string;
    service : service;
  } [@@deriving bin_io, compare, fields, sexp]

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Provider"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s %s" t.name t.desc
    end)
end

type provider = Provider_t.t [@@deriving bin_io, compare, sexp]

module Product_t = struct

  type t = {
    digest    : string;
    providers : provider list;
  } [@@deriving bin_io, compare, fields, sexp]

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Product"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s" t.digest
    end)
end

type product = Product_t.t [@@deriving bin_io, compare, sexp]

module Product = struct
  include Product_t

  let create ~digest provider = { digest; providers = [provider] }

  let providers t = t.providers

  let combine p p' = {
    digest = String.concat [p.digest; p'.digest];
    providers = p.providers @ p'.providers;
  }

end

module Service = struct
  include Service_t

  let services = Table.create ()

  let declare ~desc ~uuid name =
    let t = {desc=desc; uuid; name;} in
    Hashtbl.set services t (Stream.create ());
    t

  let provide t product =
    let p = Hashtbl.find_exn services t in
    Signal.send (snd p) product

  let request t = fst (Hashtbl.find_exn services t)

end

module Provider = struct

  include Provider_t

  let providers = String.Table.create ()

  let declare ~desc name service =
    let p = {name; desc; service} in
    Hashtbl.set providers name p;
    p

  let all f = List.filter (Hashtbl.data providers) ~f
  let all_by_name n = all (fun p -> String.equal p.name n)
  let all_by_service s = all (fun p -> Service.equal p.service s)

  let select ?by_service ?by_name () =
    let find = Option.value_map ~default:[] in
    find ~f:all_by_service by_service @
    find ~f:all_by_name by_name
end
