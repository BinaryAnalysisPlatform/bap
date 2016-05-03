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


let extract ida path arch =
  debug "extracting from %a-%s" Arch.pp arch path;
  let id =
    let ida = match ida with
      | None -> "default"
      | Some ida -> ida in
    Data.Cache.digest ~namespace:"ida" "%s%s" (Digest.file path) ida in
  let syms = match Symbols.Cache.load id with
    | Some syms -> syms
    | None -> match Ida.(with_file ?ida path get_symbols) with
      | [] ->
        warning "didn't find any symbols";
        info "this plugin doesn't work with IDA Free";
        []
      | syms ->
        eprintf "We're here\n";
        debug "got non-empty set of symbols";
        Symbols.Cache.save id syms; syms in
  let size = Arch.addr_size arch in
  let width = Size.in_bits size in
  let addr = Addr.of_int64 ~width in
  List.map syms ~f:(fun (n,s,e) -> n, addr s, addr e) |>
  Seq.of_list


let register_source (module T : Target) ida =
  let source =
    let open Project.Info in
    let extract file arch = Or_error.try_with (fun () ->
        extract ida file arch |> T.of_blocks) in
    Stream.Variadic.(apply (args file $ arch) ~f:extract) in
  T.Factory.register name source

let register ida =
  info "IDA is found, providing services";
  register_source (module Rooter) ida;
  register_source (module Symbolizer) ida;
  register_source (module Reconstructor) ida


let main ida =
  if Ida.exists ?ida () then register ida
  else match ida with
    | None ->
      info "can't find IDA instance, IDA plugin is disabled";
      info "advice - if you have IDA installed, provide a path to it"
    | Some _ ->
      error "IDA was request, but we failed to find it@.";
      info "advice - if you have IDA installed, provide a path to it";
      exit 1

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
    let run = Term.(const main $path) in
    match Term.eval ~argv ~catch:false (run,info) with
    | `Ok () -> ()
    | `Help | `Version -> exit 0
    | `Error _ -> exit 1
end
