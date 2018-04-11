open Core_kernel.Std
open Bap_future.Std
open Regular.Std

type provider = {
  pname : string;
  pdesc : string;
} [@@deriving bin_io, compare, sexp]

type product = {
  digest   : string;
  provider : provider;
} [@@deriving bin_io, compare, sexp]

type service = {
  uuid  : string;
  sname : string;
  sdesc : string;
} [@@deriving bin_io, compare, sexp]

let todo () = failwith "unimplemented"

module Product = struct
  type t = product [@@deriving bin_io, compare, sexp]
  let create ~digest provider = { digest; provider; }
  let digest t = t.digest
  let combine _p _p' = todo ()
  let providers _p = todo ()
end

module Service = struct
  type t = service [@@deriving bin_io, compare, sexp]

  module Regular = Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Service"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s %s" t.sname t.sdesc
    end)

  let services = Regular.Table.create ()

  let declare ~desc ~uuid sname =
    let t = {sdesc=desc; uuid; sname;} in
    Hashtbl.set services t (Stream.create ());
    t

  let provide t product =
    let p = Hashtbl.find_exn services t in
    Signal.send (snd p) product

  let request t = fst (Hashtbl.find_exn services t)

  include Regular

end

module Provider = struct
  type t = provider [@@deriving bin_io, compare, sexp]

  module Regular = Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Service.Provider"
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t = Format.fprintf fmt "%s %s" t.pname t.pdesc
    end)

  let providers : service Regular.Table.t = Regular.Table.create ()

  let declare ~desc pname service =
    let p = {pname;pdesc=desc} in
    Hashtbl.set providers p service;
    p

  let name t = t.pname

  let find_by_name = function
    | None -> None
    | Some n ->
      List.find (Hashtbl.keys providers)
        ~f:(fun p -> String.equal p.pname n)

  let filter_by_service = function
    | None -> []
    | Some s ->
      List.filter_map (Hashtbl.to_alist providers)
        ~f:(fun (p,s') ->
            Option.some_if (Service.equal s' s) p)

  let select ?by_service ?by_name () =
    let ps = filter_by_service by_service in
    match find_by_name by_name with
    | None -> ps
    | Some p -> p :: ps

  include Regular

end

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
        | None -> failwith (sprintf "no providers decalred under the name %s" name)
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
