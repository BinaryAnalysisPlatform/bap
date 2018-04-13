open Core_kernel.Std
open Bap_future.Std
open Regular.Std

type 'a named = {
  name : string;
  desc : string;
  data : 'a;
} [@@deriving bin_io, compare, sexp]

type service = string named
[@@deriving bin_io, compare, sexp]

type provider = service named
[@@deriving bin_io, compare, sexp]

type product = {
  digest    : string;
  providers : provider list;
} [@@deriving bin_io, compare, sexp]

module Service = struct
  type t = service
  [@@deriving bin_io, compare, sexp]

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Service"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s %s" t.name t.desc
    end)

  let services = Table.create ()

  let declare ~desc ~uuid name =
    let t = {desc=desc; data=uuid; name;} in
    Hashtbl.set services t (Stream.create ());
    t

  let provide t product =
    let p = Hashtbl.find_exn services t in
    Signal.send (snd p) product

  let request t = fst (Hashtbl.find_exn services t)
end

module Provider = struct
  type t = provider
  [@@deriving bin_io, compare, sexp]

  let providers = String.Table.create ()

  let declare ~desc name service =
    let p = {name; desc; data=service} in
    Hashtbl.set providers name p;
    p

  let name p = p.name

  let all f = List.filter (Hashtbl.data providers) ~f
  let all_by_name n = all (fun p -> String.equal p.name n)
  let all_by_service s = all (fun p -> Service.equal p.data s)

  let select ?by_service ?by_name () =
    let find = Option.value_map ~default:[] in
    find ~f:all_by_service by_service @
    find ~f:all_by_name by_name


  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Provider"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s %s" t.name t.desc
    end)

end

module Product = struct
  type t = product [@@deriving bin_io, compare, sexp]

  let create ~digest provider = { digest; providers = [provider] }

  let digest t = t.digest
  let providers t = t.providers

  let combine p p' = {
    digest = String.concat [p.digest; p'.digest];
    providers = p.providers @ p'.providers;
  }

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Product"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s" t.digest
    end)
end
