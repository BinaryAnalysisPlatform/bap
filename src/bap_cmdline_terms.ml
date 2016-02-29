open Core_kernel.Std
open Bap.Std
open Cmdliner
open Format
open Bap_options

module type With_factory = sig
  module Factory : sig
    val list : 'a Source.t -> string list
  end
end

let enum_processors (source : source) (module T : With_factory) =
  let ps = match source with
    | `Memory _ -> T.Factory.list Source.Memory
    | `Binary   -> T.Factory.list Source.Binary
    | `File _   -> T.Factory.list Source.File in
  List.map ps ~f:(fun x -> x,x)

let filename : string Term.t =
  let doc = "Input filename." in
  Arg.(required & pos 0 (some non_dir_file) None &
       info [] ~doc ~docv:"FILE")

let brancher source : string option Term.t =
  match enum_processors source (module Brancher) with
  | [] | [_] -> Term.const None
  | names ->
    let doc = sprintf "Use specified brancher, should be %s" @@
      Arg.doc_alts_enum names in
    Arg.(value & opt (some (enum names)) None & info ["brancher"] ~doc)

let symbolizers src : string list Term.t =
  match enum_processors src (module Symbolizer) with
  | [] | [_] -> Term.const []
  | names ->
    let doc =
      sprintf "Use a specified symbolizer. If an option is specified
      several times, then symbolizers are merged. Possible values
      are: %s" @@ Arg.doc_alts_enum names in
    Arg.(value & opt_all (enum names) [] & info ["symbolizer"] ~doc)

let rooters src : string list Term.t =
  match enum_processors src (module Rooter) with
  | [] | [_] -> Term.const []
  | names ->
    let doc = sprintf "Use a rooter with a given $(docv) . If an
    option is specified several times, then rooters are
    merged. Possible values: %s. Since the internal rooter will work
    only on binaries, that have debugging information, then if no
    rooter is specified, then it will default to the first rooter in a
    list of rooters. If you want to disable this behavior and use
    internal rooter, then specify this explicitly by
    `--rooter=internal`" @@
      Arg.doc_alts_enum names in
    Arg.(value & opt_all (enum names) [] &
         info ["rooter"] ~doc ~docv:"NAME")

let reconstructor source : string option Term.t =
  match enum_processors source (module Reconstructor) with
  | []  -> Term.const None
  | names ->
    let doc = "Use a specified reconstructor" in
    Arg.(value & opt (some (enum names)) None & info ["reconstructor"] ~doc)

let loader source : string Term.t =
  Image.available_backends () |>
  List.map ~f:(fun x -> x,x) |> function
  | [ ]   -> Term.const "<no-loaders-available>"
  | [x,_] -> Term.const x
  | _ when source <> `Binary -> Term.const "nil"
  | backends ->
    let doc = sprintf
        "Backend name for an image loader, should be %s" @@
      Arg.doc_alts_enum backends in
    Arg.(value & opt (enum backends) "llvm" & info ["loader"] ~doc)

let disassembler () : string Term.t =
  Disasm_expert.Basic.available_backends () |>
  List.map ~f:(fun x -> x,x) |> function
  | [] -> Term.const "<no-disassemblers-available>"
  | [x,_] -> Term.const x
  | backends ->
    let doc = sprintf
        "Disassembler backend, should be %s" @@
      Arg.doc_alts_enum backends in
    Arg.(value & opt (enum backends) "llvm" & info ["disassembler"] ~doc)


let symbols source : string list Term.t =
  let rooters = enum_processors source (module Rooter) in
  let sybolzs = enum_processors source (module Symbolizer) in
  match List.filter sybolzs ~f:(List.mem rooters) with
  | [] | [_] -> Term.const []
  | names ->
    let doc =
      sprintf "Use $(docv) as rooter and symbolizer. This is a
    shortcut for --rooter=$(docv) --symbolizer=$(docv). You can
    specify this option several times to mix different sources of
    information. Possible values %s" @@
      Arg.doc_alts_enum names in
    Arg.(value & opt_all (enum names) [] &
         info ["symbols"] ~doc ~docv:"NAME")

let list_formats, list_formats_doc =
  let doc =
    "Print detailed information about available project printers" in
  Arg.(value & flag & info ["list-formats"] ~doc), doc

let dump_formats () : Bap_fmt_spec.t list Term.t =
  let fmts = Project.available_writers () |>
             List.map ~f:(fun (n,_,_) -> n,n) in
  let doc = sprintf
      "Print a project using designated format to a specified
       destination. If format and destination is not specified, then a
       program will be printed in the IR form to the standard
       output. The argument consists of format specification, and an
       optional destination, separated from the format by a colon (:)
       symbol. The format specification, consists of format name and
       optional version number, separated from the name by a dash
       symbol, e.g., `<fmt>` or `<fmt>-<ver>`.  Acceptable format
       names are %s.  If version is omitted then the latest version of
       the given format will be used. If destination is not omitted,
       then data will to the specified destination, otherwise it will
       be printed to the standard output. The full argument grammar is
       `fmt ::= <fmt>[-<ver>][:<dst>]'. Examples: `bir', `bir-0.1`,
       `sexp-0.9:data.sexp`, etc. Use `--list-formats' command line
       option to get the detailed information about available formats
       and corresponding versions. This option can be specified
       several times, to dump the project is several formats (and
       possibly destinations) simultaneously." @@
    Arg.doc_alts_enum fmts in
  let vopt = `stdout,"bir",None in
  Arg.(value & opt_all ~vopt Bap_fmt_spec.t [] &
       info ["dump"; "d"] ~doc)

let source_type : source Term.t =
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
  Arg.(value & opt Bap_source_type.t `Binary & info ["source-type"]
         ~doc ~docv:"NAME")

let verbose : bool Term.t =
  let doc = "Print verbose output" in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

let load_doc =
  "Dynamically loads file $(i,PATH).plugin. A plugin must be compiled
   with $(b,bapbuild) tool using $(b,bapbuild PATH.plugin) command."
let load : string list Term.t =
  Arg.(value & opt_all string [] & info ["l"] ~doc:load_doc ~docv:"PATH")

let load_path_doc =
  "Add $(i,PATH) to a set of search paths. Plugins found in the search
  paths will be loaded automatically."

let load_path : string list Term.t =
  Arg.(value & opt_all string [] &
       info ["load-path"; "L"] ~doc:load_path_doc ~docv:"PATH")

let list_plugins, list_plugin_doc =
  let doc = "List available plugins" in
  Arg.(value & flag & info ["list-plugins"] ~doc), doc

let disable_plugin, disable_plugin_doc =
  let doc = "Don't load $(i,PLUGIN) automatically" in
  Arg.(value & opt_all string [] &
       info ["disable-plugin"] ~doc),doc

let no_auto_load, no_auto_load_doc =
  let doc = "Disable auto loading of plugins" in
  Arg.(value & flag & info ["disable-autoload"] ~doc), doc


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
    "Runs a program $(i,PASS). The $(i,PASS) should be registered in the
         system, usually by loading or installing corresponding plugin."
  end;
  `I begin
    "$(b,--)$(i,PASS)-$(i,OPTION)",
    "Passes $(i,OPTION) to a $(i,PASS). The option will be passed as
         $(b,--OPTION). If $(i,OPTION) is followed by an argument
        (i.e., a token that doesn't start with a dash), then it will be
        also passed to the $(i,PASS). Note: it is assumed that all
        options to passes consumes and optional argument. Make sure,
        that your flags are followed by some option, otherwise the
        argument that follows the flag might be consumed by a $(i,PASS)"
  end;
]
