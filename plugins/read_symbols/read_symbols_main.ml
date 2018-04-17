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
  val service : service
  module Factory : Source.Factory.S with type t = t
end

let provider service =
  Provider.declare "file"
    ~desc:"Read symbols from a file" service

let register syms =
  let register name (module T : Target) =
    let source = Stream.map Project.Info.arch (fun arch ->
        Or_error.try_with (fun () ->
            extract syms arch |>
            Seq.of_list |> T.of_blocks)) in
    let provider = provider T.service in
    T.Factory.provide provider source;
    Product.provide ~digest:(sprintf "file %s" syms) provider;
    info "%s product issued" name in
  register "rooter" (module Rooter);
  register "symbolizer" (module Symbolizer);
  register "reconstructor" (module Reconstructor)

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Read symbol information from a file and provide rooter,
    symbolizer and a reconstructor, based on this information. Once
  symbols are read, use $(b,--)$(i,SERVICE)=$(b,file) to use them.
  where $(i,SERVICE) is one of $(b,rooter), $(b,symbolizer) or $(b,reconstructor).
";
      `S "SEE ALSO";
      `P "$(b,bap-plugin-objdump)(1), $(b,bap-plugin-byteweight)(1), $(b,bap-plugin-ida)(1)";
    ] in
  let symsfile = Config.(param (some non_dir_file) "from" ~docv:"SYMS"
                           ~doc:"Use this file as symbols source") in
  Config.when_ready (fun {Config.get=(!)} ->
      match !symsfile with
      | Some file -> register file
      | None -> () )
