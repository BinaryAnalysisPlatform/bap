open Core_kernel.Std
open Regular.Std
open Bap_future.Std
open Bap.Std
open Bap_ida.Std

open Format
open Result.Monad_infix

include Self()

module Symbols = Data.Make(struct
    type t = (string * int64 * int64) list
    let version = "0.1"
  end)

module type Target = sig
  type t
  val of_blocks : (string * addr * addr) seq -> t
  module Factory : sig
    val register : string -> t source -> unit
  end
end


let extract path arch =
  let id =
    Data.Cache.digest ~namespace:"ida" "%s" (Digest.file path) in
  let syms = match Symbols.Cache.load id with
    | Some syms -> syms
    | None -> match Ida.(with_file path get_symbols) with
      | [] ->
        warning "didn't find any symbols";
        info "this plugin doesn't work with IDA Free";
        []
      | syms -> Symbols.Cache.save id syms; syms in
  let size = Arch.addr_size arch in
  let width = Size.in_bits size in
  let addr = Addr.of_int64 ~width in
  List.map syms ~f:(fun (n,s,e) -> n, addr s, addr e) |>
  Seq.of_list


let register_source (module T : Target) =
  let source =
    let open Project.Info in
    let extract file arch = Or_error.try_with (fun () ->
        extract file arch |> T.of_blocks) in
    Stream.merge file arch ~f:extract in
  T.Factory.register name source



let main () =
  register_source (module Rooter);
  register_source (module Symbolizer);
  register_source (module Reconstructor)

module Main = struct
  open Cmdliner
  let man = [
    `S "DESCRIPTION";
    `P "This plugin provides rooter, symbolizer and reconstuctor services.";
    `P "If IDA instance is found on the machine, or specified by a
  user, it will be queried for the specified information."

  ]


  let path : string option Term.t =
    let doc = "Use IDA to extract symbols from file. \
               You can optionally provide path to IDA executable,\
               or executable name." in
    Arg.(value & opt (some string) None & info ["path"] ~doc)

  let info = Term.info name ~version ~doc ~man

  let () =
    let run = Term.(const main $ const ()) in
    match Term.eval ~argv ~catch:false (run,info) with
    | `Ok () -> ()
    | `Help | `Version -> exit 0
    | `Error _ -> exit 1
end
