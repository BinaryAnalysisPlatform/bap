open Core_kernel.Std
open Bap.Std
open Frontend
open Format
open Bap_options

module type With_factory = sig
  module Factory : sig
    val list : unit -> string list
  end
end

let enum_processors (module T : With_factory) =
  fun () -> T.Factory.list () |> List.map ~f:(fun x -> x,x)

let map_snd f s =
  fun () -> f () |> List.map ~f:(fun (x,y) -> x,s y)

let filename : string Config.param =
  let doc = "Input filename." in
  Config.(pos non_dir_file ~docv:"FILE" ~doc 0)

let brancher : string option Config.param =
  let f () = enum_processors (module Brancher) ()
             |> List.map ~f:(fun (x,y) -> x,Some y)
             |> List.cons ("", None) in
  let doc = "Use specified brancher" in
  Config.(post_check ~f (param (some string) "brancher" ~doc))

let symbolizers : string list Config.param =
  let f = enum_processors (module Symbolizer) in
  let doc = "Use a specified symbolizer. If an option is specified
      several times, then symbolizers are merged." in
  Config.(post_check_all ~f (param_all string "symbolizer"
                               ~default:["internal"] ~doc))

let rooters : string list Config.param =
  let f = enum_processors (module Rooter) in
  let doc = "Use a rooter with a given $(docv). If an \
             option is specified several times, then rooters are \
             merged." in
  Config.(post_check_all ~f (param_all string "rooter"
                               ~docv:"NAME" ~doc))

let reconstructor : string option Config.param =
  let f () = enum_processors (module Reconstructor) ()
             |> List.map ~f:(fun (x,y) -> x,Some y)
             |> List.cons ("", None) in
  let doc = "Use a specified reconstructor" in
  Config.(post_check ~f (param (some string) "reconstructor" ~doc))

let loader : string Config.param =
  let f () = Project.Input.available_loaders () |>
             List.map ~f:(fun x -> x,x) in
  let doc = "Backend name for an image loader" in
  Config.(post_check ~f (param string "loader" ~default:"llvm" ~doc))

let disassembler : string Config.param =
  let f () = Disasm_expert.Basic.available_backends () |>
             List.map ~f:(fun x -> x,x) in
  let doc = "Disassembler backend" in
  Config.(post_check ~f (param string "disassembler"
                           ~default:"llvm" ~doc))

let symbols : string list Config.param =
  let rooters = enum_processors (module Rooter) in
  let sybolzs = enum_processors (module Symbolizer) in
  let f () = rooters () @ sybolzs () in
  let doc =
    "Use $(docv) as rooter and symbolizer. This is a shortcut for
     --rooter=$(docv) --symbolizer=$(docv). You can specify this
     option several times to mix different sources of information." in
  Config.(post_check_all ~f (param_all string "symbols"
                               ~docv:"NAME" ~doc))

let list_formats, list_formats_doc =
  let doc =
    "Print detailed information about available project printers" in
  Config.(flag "list-formats" ~doc), doc

let dump_formats : Bap_fmt_spec.t list Config.param =
  let f () = Project.available_writers () |>
             List.filter_map ~f:(fun (n,_,_) ->
                 match Bap_fmt_spec.parse n with
                 | `Ok x -> Some (n, x)
                 | `Error _ -> None) in
  let doc =
    "Print a project using the designated format to a specified
    destination. If format and destination is not specified, then a
    program will be printed in the IR form to the standard
    output. The argument consists of format specification, and an
    optional destination, separated from the format by a colon (:)
    symbol. The format specification, consists of format name and
    optional version number, separated from the name by a dash
    symbol, e.g., `<fmt>` or `<fmt>-<ver>`.  If version is omitted
    then the latest version of the given format will be used. If
    destination is not omitted, then data will to the specified
    destination, otherwise it will be printed to the standard
    output. The full argument grammar is `fmt ::=
    <fmt>[-<ver>][:<dst>]'. Examples: `bir', `bir-0.1`,
    `sexp-0.9:data.sexp`, etc. Use `--list-formats' command line
    option to get the detailed information about available formats and
    corresponding versions. This option can be specified several
    times, to dump the project is several formats (and possibly
    destinations) simultaneously." in
  let as_flag = Bap_fmt_spec.as_flag in
  let fmt_param = Config.(param_all Bap_fmt_spec.converter "dump"
                            ~synonyms:["d"] ~as_flag ~doc) in
  Config.(post_check_all ~f fmt_param)

let source_type : source Config.param =
  let doc = "Defines a format of the input file. It can be either a
            `binary' that denotes a structured binary file in a a
            format that is supported by a loader. It can also be a
            `<arch>-code' (e.g., `arm-code', `x86-code') if the binary
            is a raw code for the given <arch>. If the $(docv) is
            `project`, then the input file must be project data
            serialized with some format available for project data
            type. The format can be encoded in the extension. If
            needed, then a version number can be specifed, separated
            from the format by a dash, e.g., `myproj.marshal`,
            `myproj.sexp-1.0', etc. If the format is not specified,
            then the default reader will be used. The name can be also
            of the form `<name>-custom', where <name> should be a name
            of factory method registered for a file source in project
            factory." in
  Config.(param Bap_source_type.converter "source-type"
            ~docv:"NAME" ~doc)

let verbose : bool Config.param =
  let doc = "Print verbose output to log file" in
  Config.(flag "verbose" ~doc)

