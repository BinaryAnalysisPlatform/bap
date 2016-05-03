open Core_kernel.Std
open Regular.Std
open Bap_future.Std
open Bap.Std

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
  module Factory : sig
    val register : string -> t source -> unit
  end
end

let register syms =
  let name = "file" in
  let register (module T : Target) =
    let source = Stream.map Project.Info.arch (fun arch ->
        Or_error.try_with (fun () ->
            extract syms arch |>
            Seq.of_list |> T.of_blocks)) in
    T.Factory.register name source in
  register (module Rooter);
  register (module Symbolizer);
  register (module Reconstructor)

let man = [
  `S "DESCRIPTION";
  `P "Read symbol information from a file and provide rooter,
    symbolizer and a reconstructor, based on this information."
]

module Main = struct
  open Cmdliner
  let info = Term.info name ~version ~doc ~man

  let symsfile : string option Term.t =
    let doc = "Use this file as symbols source" in
    Arg.(value & opt (some non_dir_file) None &
         info ["from";] ~doc ~docv:"SYMS")

  let () =
    match Term.eval ~argv (symsfile,info) with
    | `Ok (Some file) -> register file
    | `Ok None -> ()
    | `Help | `Version -> exit 0
    | `Error _ -> exit 1
end
