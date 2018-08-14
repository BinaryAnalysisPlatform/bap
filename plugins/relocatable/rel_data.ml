open Core_kernel
open Bap.Std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)

module Rel = struct
  open Image.Scheme
  open Fact.Syntax

  let of_aseq s =
    Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
        Map.add m ~key ~data)

  let arch_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let relocations =
    arch_width >>= fun width ->
    Fact.collect Ogre.Query.(select (from relocation)) >>= fun s ->
    Fact.return
      (of_aseq @@ Seq.map s ~f:(fun (addr, data) ->
           Addr.of_int64 ~width addr, Addr.of_int64 ~width data))

  let external_symbols  =
    arch_width >>= fun width ->
    Fact.collect Ogre.Query.(
        select (from external_reference)) >>= fun s ->
    Fact.return
      (of_aseq @@ Seq.map s ~f:(fun (addr, data) ->
           Addr.of_int64 ~width addr, data))
end

let find min_addr max_addr data =
  let rec run addr =
    if Addr.(addr > max_addr) then None
    else
      match Map.find data addr with
      | None -> run (Addr.succ addr)
      | Some value -> Some value in
  run min_addr

let relocations spec = Fact.eval Rel.relocations spec
let external_symbols spec = Fact.eval Rel.external_symbols spec
