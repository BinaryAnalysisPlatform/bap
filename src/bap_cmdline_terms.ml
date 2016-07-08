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
             |> List.map ~f:(fun (x,y) -> x,Some y) in
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
             |> List.map ~f:(fun (x,y) -> x,Some y) in
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
  let doc = "Print verbose output" in
  Config.(flag "verbose" ~doc)

let load, load_doc =
  let doc =
    "Dynamically loads file $(i,PATH).plugin. A plugin must be
     compiled with $(b,bapbuild) tool using $(b,bapbuild PATH.plugin)
     command." in
  Config.(param_all string "l" ~docv:"PATH" ~doc), doc

let load_path, load_path_doc =
  let doc =
    "Add $(i,PATH) to a set of search paths. Plugins found in the
    search paths will be loaded automatically." in
  Config.(param_all string "L" ~synonyms:["load-path"]
            ~docv:"PATH" ~doc), doc

let list_plugins, list_plugin_doc =
  let doc = "List available plugins" in
  Config.(flag "list-plugins" ~doc), doc

let disable_plugin, disable_plugin_doc =
  let doc = "Don't load $(i,PLUGIN) automatically" in
  Config.(param_all string "disable-plugin" ~doc), doc

let no_auto_load, no_auto_load_doc =
  let doc = "Disable auto loading of plugins" in
  Config.(flag "disable-autoload" ~doc), doc

let loader_options = [
  "-l"; "-L"; "--list-plugins"; "--disable-plugin"; "--disable-autoload"
]

let common_loader_options = [
  `S "PLUGIN OPTIONS";
  `I ("$(b,-l) $(i,PATH)", load_doc);
  `I ("$(b,-L)$(i,PATH)", load_path_doc);
  `I ("$(b,--list-plugins)", list_plugin_doc);
  `I ("$(b,--disable-plugin)", disable_plugin_doc);
  `I ("$(b,--disable-autoload)", no_auto_load_doc);
  `I ("$(b,--no-)$(i,PLUGIN)", disable_plugin_doc);
]


let options_for_passes = [
  `S "OPTIONS FOR PASSES";
  `I begin
    "$(b,--)$(i,PASS)",
    "Runs a program $(i,PASS). The $(i,PASS) should be registered in the\
     system, usually by loading or installing corresponding plugin."
  end;
  `I begin
    "$(b,--)$(i,PASS)-$(i,OPTION)",
    "Passes $(i,OPTION) to a $(i,PASS). The option will be passed as\
     $(b,--OPTION). If $(i,OPTION) is followed by an argument\
     (i.e., a token that doesn't start with a dash), then it will be\
     also passed to the $(i,PASS). Note: it is assumed that all\
     options to passes consumes and optional argument. Make sure,\
     that your flags are followed by some option, otherwise the\
     argument that follows the flag might be consumed by a $(i,PASS)"
  end;
]
