open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Bap_service

module type S = sig
  type t
  val empty : t
  val of_image : image -> t
  val service : Bap_service.service
  module Factory : Bap_disasm_source.Factory with type t = t
end

let brancher =
  Provider.declare
    ~desc:"Provides internal brancher" "internal" Brancher.service

let symbolizer =
  Provider.declare
    ~desc:"Provides internal symbolizer" "internal" Symbolizer.service

let rooter =
  Provider.declare
    ~desc:"Provides internal rooter" "internal" Rooter.service

let source_of_image f =
  Stream.map Project.Info.img ~f:(fun img -> Ok (f img))

let provide provider x =
  let module S = (val x : S) in
  let source =
    Stream.map Project.Info.img ~f:(fun img -> Ok (S.of_image img)) in
  S.Factory.provide provider source

let () =
  provide brancher (module Brancher);
  provide rooter (module Rooter);
  provide symbolizer (module Symbolizer)
