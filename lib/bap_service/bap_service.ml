open Core_kernel.Std
open Bap_future.Std
open Regular.Std

type void
type literal = (void,void,void) format

type 'a named = {
  name : string;
  desc : string;
  data : 'a;
} [@@deriving bin_io, compare, fields, sexp]

type service = string
[@@deriving bin_io, compare, sexp]

type provider = service named
[@@deriving bin_io, compare, sexp]

type product = {
  digest   : string;
  provider : provider;
} [@@deriving bin_io, compare, sexp]

module Table = String.Table

type info = {
  products  : product stream;
  update    : product signal;
  providers : provider Table.t;
} [@@deriving fields]

module Service = struct
  type t = service

  let all : info named Table.t = Table.create ()

  let info s = Hashtbl.find_exn all s
  let data s = data (info s)
  let products  s = products (data s)
  let update    s = update (data s)
  let providers s = providers (data s)

  let check_uuid x =
    if Option.is_none (Uuidm.of_string x) then
      invalid_arg "Invalid UUID format";
    if Hashtbl.mem all x then
      invalid_argf "UUID %s is already in use" x ()

  let declare ~desc ~uuid name =
    let uuid = string_of_format uuid in
    check_uuid uuid;
    let providers = Table.create () in
    let products,update = Stream.create () in
    let data = {products; update; providers} in
    Hashtbl.set all uuid {name; desc; data};
    uuid

  let request = products
end

module Provider = struct
  type t = provider
  [@@deriving bin_io, compare, sexp]

  let declare ~desc name service =
    let p = {name; desc; data=service} in
    Hashtbl.set (Service.providers service) p.name p;
    p

  let name p = p.name
  let service p = p.data

  let all_by_service s = Hashtbl.data (Service.providers s)

  let all_by_name n =
    Hashtbl.keys Service.all |>
    List.map ~f:all_by_service |>
    List.concat |>
    List.filter ~f:(fun p -> String.equal p.name n)

  let select ?by_service ?by_name () =
    let find = Option.value_map ~default:[] in
    find ~f:all_by_service by_service @
    find ~f:all_by_name by_name
end

module Product = struct
  type t = product [@@deriving bin_io, compare, sexp]

  let digest t = t.digest
  let provider t = t.provider

  let provide ~digest provider =
    let sid = Provider.service provider in
    Signal.send (Service.update sid) {digest; provider}

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap_service.Product"
      let hash = Hashtbl.hash
      let version = "1.0"
      let pp fmt t = Format.fprintf fmt "%s" (digest t)
    end)
end
