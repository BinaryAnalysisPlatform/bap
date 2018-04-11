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

module Provider_t = struct
  type t = {
    name : string;
    desc : string;
    service : Service_t.t;
  } [@@deriving bin_io, compare, fields, sexp]

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Provider"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s %s" t.name t.desc
    end)
end

module Product_t = struct
  type t = {
    digest   : string;
    provider : Provider_t.t;
    products : t list;
  } [@@deriving bin_io, compare, fields, sexp]

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Product"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s" t.digest
    end)
end

module Product = struct
  include Product_t

  let create ~digest provider = { digest; provider; products = []}

  let providers t =
    t.provider :: List.map ~f:provider t.products

  let combine t t' =
    let get x f = f x :: List.map ~f x.products in
    let both f = get t f @ get t' f in
    let digest   = String.concat (both digest) in
    let products = List.concat (both products) in
    {t with digest; products}

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

type provider = Provider.t [@@deriving bin_io, compare, sexp]
type service  = Service.t  [@@deriving bin_io, compare, sexp]
type product  = Product.t  [@@deriving bin_io, compare, sexp]

module Try = struct

  type 'a t = 'a Or_error.t stream
  type 'a source = 'a t

  module type Factory = sig
    type t
    val list : unit -> string list
    val find : string -> t source option
    val register : string -> t source -> unit

    val provide : provider -> t source -> unit
    val request : provider -> t source option
    val providers : unit -> provider list

  end

  module Tab = Provider.Table

  module Factory = struct
    module type S = Factory
    module Make(T : T) : S with type t = T.t = struct
      type t = T.t
      let factory : t source Tab.t = Tab.create ()

      let register name source =
        match List.hd @@ Provider.select ~by_name:name () with
        | None -> failwith (sprintf "no providers declared under the name %s" name)
        | Some p -> Hashtbl.set factory ~key:p ~data:source

      let provide provider source =
        Hashtbl.set factory ~key:provider ~data:source

      let request = Hashtbl.find factory

      let list () = Hashtbl.keys factory |> List.map ~f:Provider.name

      let find name =
        List.find_map (Hashtbl.to_alist factory)
          ~f:(fun (p,s) ->
              Option.some_if (String.equal (Provider.name p) name) s)

      let providers () = Hashtbl.keys factory

    end
  end

end
