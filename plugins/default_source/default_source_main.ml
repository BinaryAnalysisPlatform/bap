open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Bap_service

include Self ()

module type S = sig
  type t
  val empty : t
  val of_image : image -> t
  val service : Bap_service.service
  module Factory : Bap_disasm_source.Factory with type t = t
end

let internal name s =
  let desc = sprintf "Provides internal %s" name in
  Provider.declare ~desc "internal" s, name

let brancher = internal "brancher" Brancher.service
let symbolizer = internal "symbolizer" Symbolizer.service
let rooter = internal "rooter" Rooter.service

let provide (provider,service_name) x =
  let module S = (val x : S) in
  let source =
    Stream.map Project.Info.img ~f:(fun img -> Ok (S.of_image img)) in
  S.Factory.provide provider source;
  let digest = sprintf "%s-%s" (Provider.name provider) service_name in
  Product.provide ~digest provider;
  info "product issued for %s" digest

let () =
  provide brancher (module Brancher);
  provide rooter (module Rooter);
  provide symbolizer (module Symbolizer)
