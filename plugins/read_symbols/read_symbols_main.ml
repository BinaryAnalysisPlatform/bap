open Core_kernel.Std
open Regular.Std
open Bap_future.Std
open Bap.Std
open Bap_service

include Self()

let extract name arch =
  let blk_of_sexp x = [%of_sexp:string*int64*int64] x in
  let width = Arch.addr_size arch |> Size.in_bits in
  let addr = Addr.of_int64 ~width in
  In_channel.with_file name ~f:(fun ch ->
      Sexp.input_sexps ch |> List.map ~f:blk_of_sexp |>
      List.map ~f:(fun (n,s,e) -> n,addr s,addr e))


module type Target = sig
  type t
  val of_blocks : (string * addr * addr) seq -> t
  module Factory : Source.Factory.S with type t = t
end


let input_filename = Config.(begin
    param (some non_dir_file) "from" ~docv:"FILE"
      ~doc:"Use this file as symbols source"
  end)

let provide service = Service.(begin
    provide service "edu.cmu.ece.bap/file" [
      parameter input_filename;
    ]
      ~desc:"reads information from an external file"
  end)

let rooter = provide Service.rooter
let symbolizer = provide Service.symbolizer
let reconstructor = provide Service.reconstructor


let register_sources filename =
  let register_source provider (module T : Target) =
    Stream.observe (Service.inputs provider) @@ fun _have_inputs ->
    T.Factory.register name @@
    Stream.map Project.Info.arch (fun arch ->
        Or_error.try_with (fun () ->
            extract filename arch |>
            Seq.of_list |> T.of_blocks))  in
  register_source rooter (module Rooter);
  register_source symbolizer (module Symbolizer);
  register_source reconstructor (module Reconstructor)


let () = Config.when_ready (fun {Config.get=(!!)} ->
    Option.iter !!input_filename ~f:register_sources)
;;

Config.manpage [
  `S "DESCRIPTION";
  `P "Read symbol information from an external file and provide \
      rooter, symbolizer and a reconstructor, based on this \
      information. See the $(b,FILE FORMAT) section for the
          description of supported file formats.";
  `S "FILE FORMATS";
  `P "TBD";
  `S "SEE ALSO";
  `P "$(b,bap-plugin-objdump)(1), $(b,bap-plugin-byteweight)(1), $(b,bap-plugin-ida)(1)";
]
