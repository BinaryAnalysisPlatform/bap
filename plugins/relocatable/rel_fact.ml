open Core_kernel.Std
open Bap_future.Std
open Bap.Std
open Image
open Monads.Std

include Self()

module Fact = Ogre.Make(Monad.Ident)

module Rel = struct
  open Image.Scheme
  open Fact.Syntax

  let of_seq s ~fkey ~fdata =
    Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
        Map.add m ~key:(fkey key) ~data:(fdata data))

  let addr_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let relocations =
    Fact.collect Ogre.Query.(select (from relocation))

  let external_symbols  =
    Fact.collect Ogre.Query.(
        select (from external_reference))

  let relocations =
    addr_width >>= fun width ->
    relocations >>= fun rels ->
    external_symbols >>= fun exts ->
    let to_addr = Addr.of_int64 ~width in
    Fact.return (of_seq rels ~fkey:to_addr ~fdata:to_addr,
                 of_seq exts ~fkey:to_addr ~fdata:ident)

end

type 'a fixups = 'a Addr.Map.t

type t = {
  rels : addr fixups;
  exts : string fixups;
}

let create spec =
  match Fact.eval Rel.relocations spec with
  | Ok (rels,exts) -> Ok {rels; exts;}
  | Error er -> Error er

let relocations t = t.rels
let externals t = t.exts

let find ~from ~to_ where =
  let rec get addr =
    if Addr.(addr > to_) then None
    else
      match Map.find where addr with
      | None -> get (Addr.succ addr)
      | Some value -> Some value in
  get from
