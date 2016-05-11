open Core_kernel.Std
open Bap_bundle.Std
open Bap.Std
include Self()
open Cabs
open Option.Monad_infix

type options = {
  add_headers : string list;
  use_header : string option
}

(* let main bundle options proj = *)
(*   let prog = Project.program proj in *)
(*   let arch = Project.arch proj in *)
(*   let args = protos_of_bundle bundle in *)
(*   let args = match options.use_header with *)
(*     | Some file -> merge_args args (protos_of_file file) *)
(*     | None -> args in *)
(*   fill_args arch args prog |> *)
(*   Project.with_program proj *)

let main _ _ p = p

module Cmdline = struct
  include Cmdliner

  let man = [
    `S "DESCRIPTION";
    `P "Use API definition to annotate subroutines. The API is
    specified using C syntax, extended with GNU attributes. The plugin
    has an embedded knowledge of a big subset of POSIX standard, but
    new interfaces can be added.";
    `P "The plugin will insert arg terms, based on the C function declarations
    and definitions. Known gnu attributes will be mapped to
    corresponding $(b,Sub) attributes, e.g., a subroutine annotated with
    $(b,__attribute__((noreturn))) will have IR attribute
    $(b,Sub.noreturn).";
    `S "NOTES";
    `P "A file with API shouldn't contain preprocessor directives, as
    preproccessing is not run. If it has, then consider running cpp
    manually. Also, all types and structures must be defined. An
    undefined type will result in a parsing error."
  ]

  let add_headers : string list Term.t =
    let doc =
      "Add C header with function prototypes to the plugin database, \
       which will be used by default if file_header is not specified." in
    Arg.(value & opt (list file) [] & info ["add"] ~doc)

  let file_header : string option Term.t =
    let doc =
      "Use specified API. The file is not added
       to the plugin database of headers." in
    Arg.(value & opt (some string) None & info ["file"] ~doc)

  let process_args add_headers use_header = {add_headers; use_header}

  let parse argv =
    let info = Term.info ~doc ~man name in
    let spec = Term.(pure process_args $add_headers $file_header) in
    match Term.eval ~argv (spec,info) with
    | `Ok res -> res
    | `Error err -> exit 1
    | `Version | `Help -> exit 0
end


let add_headers bundle file =
  let name = "api/" ^ Filename.basename file in
  try
    (* todo call a parser and check the result *)
    (* protos_of_file file |> ignore_protos; *)
    Bundle.insert_file ~name bundle (Uri.of_string file)
  with
  | Parsing.Parse_error ->
    printf "Could not add header file: parse error."; exit 1

let () =
  let bundle = main_bundle () in
  let options = Cmdline.parse argv in
  match options.add_headers with
  | [] ->  Project.register_pass ~autorun:true (main bundle options)
  | headers -> List.iter headers ~f:(add_headers bundle);
    exit 0
