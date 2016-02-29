open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_ida.Std

open Cmdliner
open Format
open Option.Monad_infix

include Self()

let path : string option Term.t =
  let doc = "Use IDA to extract symbols from file. \
             You can optionally provide path to IDA executable,\
             or executable name." in
  Arg.(value & opt (some string) None & info ["path"] ~doc)

module Symbols = Data.Make(struct
    type t = (string * int64 * int64) list
    let version = "0.1"
  end)


let register ida =
  let make_id ida path =
    let ida = match ida with
      | None -> "default"
      | Some ida -> ida in
    Digest.(to_hex (string(file path ^ string ida))) in
  let syms_of_ida ida path =
    try Some Ida.(with_file ?ida path get_symbols) with
      exn ->
      eprintf "Warning: IDA failed: %a@." Exn.pp exn; None  in
  let syms ida path =
    let id = make_id ida path in
    match Symbols.Cache.load id with
    | Some syms -> Some syms
    | None -> match syms_of_ida ida path with
      | None -> None
      | Some syms ->
        Symbols.Cache.save id syms;
        Some syms in
  let extract img =
    let arch = Image.arch img in
    let size = Arch.addr_size arch in
    let width = Size.in_bits size in
    let addr = Addr.of_int64 ~width in
    let ida = match size with
      | `r32 -> ida
      | `r64 -> match ida with
        | None -> Some "idaq64"
        | Some ida when
            String.is_suffix ida "64" ||
            String.is_suffix ida "64.exe" -> Some ida
        | _some_other_ida ->
          eprintf "Warning: use 64 bit IDA with 64-bit binaries@.";
          ida in
    Image.filename img >>= syms ida >>|
    List.map ~f:(fun (n,s,e) -> n, addr s, addr e) >>| Seq.of_list in
  let rooter img = extract img >>| Rooter.of_blocks in
  let symbolizer img = extract img >>| Symbolizer.of_blocks in
  let reconstructor img = extract img >>| Reconstructor.of_blocks in
  Rooter.Factory.register Source.Binary name rooter;
  Symbolizer.Factory.register Source.Binary name symbolizer;
  Reconstructor.Factory.register Source.Binary name reconstructor

let main ida =
  if Ida.exists ?ida () then register ida
  else match ida with
    | None -> ()
    | Some _ ->
      eprintf "Error: IDA was request, but we failed to find it@.";
      exit 1

let man = [
  `S "DESCRIPTION";
  `P "This plugin provides rooter, symbolizer and reconstuctor services"
]

let info = Term.info name ~version ~doc ~man

let () =
  let run = Term.(const main $path) in
  match Term.eval ~argv ~catch:false (run,info) with
  | `Ok () -> ()
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
