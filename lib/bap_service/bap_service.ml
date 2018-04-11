open Core_kernel.Std
open Bap_future.Std

type service = {
  uuid  : string;
  sname : string;
  sdesc : string;
  products : product stream * product signal;
} and provider = {
    pname : string;
    pdesc : string;
    service : service;
  } and product = {
    digest   : string;
    provider : provider;
  }

let todo () = failwith "unimplemented"

module Product = struct
  type t = product
  let create ~digest provider = { digest; provider; }
  let digest t = t.digest
  let combine _p _p' = todo ()
  let providers _p = todo ()
end

module Service = struct
  type t = service

  let services = String.Table.create ()

  let declare ~desc ~uuid sname =
    let products = Stream.create () in
    let t = { sdesc=desc; uuid; sname; products } in
    Hashtbl.set services sname t;
    t

  let provide t product =
    Signal.send (snd t.products) product

  let request t = t.products

  let equal s s' = String.equal s.uuid s'.uuid

end

module Provider = struct
  type t = provider

  let providers = String.Table.create ()

  let declare ~desc pname service =
    let p = {pname;pdesc=desc;service} in
    Hashtbl.set providers pname p;
    p

  let name t = t.pname

  let select ?by_service ?by_name () =
    let find = Option.value_map in
    let filter_services s =
      List.filter (Hashtbl.data providers)
        ~f:(fun p -> Service.equal p.service s) in
    let services = find by_service ~default:[] ~f:filter_services in
    match find by_name ~default:None ~f:(Hashtbl.find providers) with
    | None -> services
    | Some x -> x :: services

  let pp fmt t = Format.fprintf fmt "%s %s" t.pname t.pdesc
end


module Try = struct

  type 'a t = 'a Or_error.t stream
  type 'a source = 'a t

  module type Factory = sig
    type t
    val list : unit -> string list
    val find : string -> t source option
    val register : string -> t source -> unit
  end

  module Tab = String.Table

  module Factory = struct
    module type S = Factory
    module Make(T : T) : S with type t = T.t = struct
      type t = T.t
      let factory : t source Tab.t = Tab.create ()

      let register _provider _source = todo ()

      let provide _provider _source = todo ()

      let reguest _provider = todo ()

      let register name source =
        Hashtbl.set factory ~key:name ~data:source

      let list () = Hashtbl.keys factory
      let find = Hashtbl.find factory
    end
  end

end
