open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Regular.Std

include Self ()

let brancher = Service.(begin
    provide brancher "edu.cmu.ece.bap/image/semantic-insn"
      ~desc:"computes destinations based on BIL semantics of a single instruction"
      ~require:[
        product required loader;
        product required lifter;
      ]
  end)

let rooter = Service.(begin
    provide rooter "edu.cmu.ece.bap/image/symtab"
      ~desc:"extracts functions starts from the image symtab"
      ~require:[
        product required loader;
      ]
  end)

let symbolizer = Service.(begin
    provide symbolizer "edu.cmu.ece.bap/image/symtab"
      ~desc:"extracts symbol names from the image symtab"
      ~require:[
        product required loader;
      ]
  end)

module type S = sig
  type t
  val of_image : image -> t
  module Factory : Bap_disasm_source.Factory with type t = t
end

let register_source (module S : S) =
  let of_image img = Ok (S.of_image img) in
  Stream.map Project.Info.img ~f:of_image |>
  S.Factory.register name

let () =
  register_source (module Brancher);
  register_source (module Rooter);
  register_source (module Symbolizer)
