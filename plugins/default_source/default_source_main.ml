open Core_kernel.Std
open Bap.Std
open Bap_future.Std

include Self ()

module type S = sig
  type t
  val empty : t
  val of_image : image -> t
  val service : Bap_service.service
  module Factory : Bap_disasm_source.Factory with type t = t
end

let internal name service =
  Bap_service.(provide service name ~require:nothing)

let brancher = internal "brancher" Brancher.service
let symbolizer = internal "symbolizer" Symbolizer.service
let rooter = internal "rooter" Rooter.service

let provide (service_name) x =
  let module S = (val x : S) in
  let source =
    Stream.map Project.Info.img ~f:(fun img -> Ok (S.of_image img)) in
  S.Factory.register name source

let () =
  provide brancher (module Brancher);
  provide rooter (module Rooter);
  provide symbolizer (module Symbolizer)
