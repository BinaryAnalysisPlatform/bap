(** Builds BAP Annotated Reference . *)

open Core_kernel
open Poly
open Bap_plugins.Std


module Filename = Caml.Filename

let libraries = [
  "Core libraries", [
    "bap-main", "Bap_main", "", "the entry point to BAP";
    "bap-knowledge", "Bap_knowledge.Knowledge", "Bap_knowledge", "The Knowledge Representation library";
    "bap-core-theory", "Bap_core_theory", "", "The Core Theory Library";
    "bap", "Bap.Std", "", "The Standard Library";
    "bap-taint", "Bap_taint.Std", "Bap_taint", "The Taint Analysis Framework";
    "bap-primus", "Bap_primus.Std", "Bap_primus", "The Microexecution Framework";
  ];

  "Foundation Libraries", [
    "monads", "Monads.Std", "Monads", "The Monads library";
    "regular", "Regular.Std", "Regular", "Regular Data Library";
    "graphlib", "Graphlib.Std", "Graphlib", "Algorithms on graphs";
    "bitvec", "Bitvec", "", "Bitvectors and modular arithmetic";
    "bap-future", "Bap_future.Std", "Bap_future", "Futures and Streams";
    "ogre", "Ogre", "", "A sexp-based NoSQL database";
  ];

  "Hardware Specific Libraries", [
    "bap-arm", "ARM", "", "ARM-specific definitions";
    "bap-x86-cpu", "X86_cpu", "", "x86/x86-64 specific definitions";
  ];

  "Language and API/ABI Specific Libraries", [
    "bap-abi", "Bap_abi", "", "Interface for specifying ABI";
    "bap-api", "Bap_api", "", "Interface for defining API";
    "bap-c", "Bap_c.Std", "Bap_c", "Basic definitions of the C language";
  ];

  "Utility Libraries", [
    "bap-bml", "Bap_bml", "", "writing term transformations";
    "bare", "Bare", "", "writing rules for matching for Primus observations";
    "bap-bundle", "Bap_bundle.Std", "Bap_bundle", "creating and opening bundles";
    "bap-byteweight", "Bap_byteweight", "", "interface to the Byteweight subsystem";
    "bap-demangle", "Bap_demangle.Std", "Bap_demangle", "writing name demanglers";
    "bap-dwarf", "Bap_dwarf.Std", "Bap_dwarf", "a native DWARF parser";
    "bap-ida", "Bap_ida.Std", "Bap_ida","an interface to IDA Pro";
    "bap-llvm", "Bap_llvm.Std", "Bap_llvm", "an inteface to LLVM disassemblers and loaders";
    "bap-plugins", "Bap_plugins.Std", "Bap_plugins", "loading plugins";
    "bap-recipe", "Bap_recipe", "", "loading recipes (packs of command line arguments)";
    "bap-strings", "Bap_strings.Std", "Bap_strings", "various text utilities";
    "bap-traces", "Bap_traces.Std", "Bap_traces", "working with execution traces";
    "text-tags", "Text_tags", "", "Extension of Format's semantic tags";
  ];
]

let frontends = [
  "bap", "bap main frontend";
  "bap-byteweight", "create, obtain and evaluate byteweight signatures";
]

let introduction = {|
{2 Introduction}

The Carnegie Mellon University Binary Analysis Platform (CMU BAP) is a
suite of utilities and libraries that enables analysis of programs in
their machine representation. BAP is written in OCaml, relies on
dynamically loaded plugins for extensibility, and is widely used for
security analysis, program verification, and reverse engineering.

The framework consists of a bunch of libraries, plugins and
frontends. The libraries provide code reusability, plugins facilitate
extensibility and frontends serve as entry points.

Frontends come with comprehensive manuals,
that can be accessed by using [--help] command line options, or via
the [man] command, if the manpath is configured correctly.
Finally, you can access a man page for a plugin using
[--<PLUGIN>-help] command line option of a frontend, e.g., [bap --map-terms-help].

|}

let packages =
  List.concat_map ~f:(fun (_,ps) -> List.map ~f:(fun (p,_,_,_) -> p) ps) libraries

let mkdir path =
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o770

let run cmd =
  let res = Sys.command cmd in
  if res <> 0 then
    failwith ("Command: '" ^ cmd ^ "' failed")

let render_entry (_,entry,short,desc) =
  let name_to_display = match short with
    | "" -> entry
    | _  -> short in
  sprintf "- {{!%s}%s} - %s" entry name_to_display desc

let render_section (name,entries) =
  sprintf "{1 %s}\n%s" name
    (String.concat ~sep:"\n" @@ List.map ~f:render_entry entries)

type info = {
  man : string -> string;
  help : string -> string;
}

let program = {
  man  = ident;
  help = sprintf "%s --help=groff"
}

let plugin = {
  man = sprintf "bap-plugin-%s";
  help = sprintf "bap --%s-help=groff";
}

let generate_man3_redirection lib =
  mkdir "man3";
  let redirection = sprintf {|
    <!doctype html>
    <html>
    <head>
    <meta http-equiv="refresh" content="0; url='../odoc/%s/index.html'" />
    </head>
    <body></body>
    </html> |} lib in
  let name = sprintf "man3/%s.3.html" lib in
  Out_channel.write_all name ~data:redirection

let connect_with_pipes cmds = String.concat cmds ~sep:" | "
let dump_to_file f cmd = sprintf "%s > %s" cmd f

let repair_links = {|sed "s/\\\N'45'/-/g"|}
let redirect_index = sprintf {|sed "s#../index.html#../odoc/index.html#g"|}
let man2html = sprintf "man2html -r"


let build_manual {man; help} tool =
  mkdir "man1";
  match Sys.command @@ sprintf "%s >/dev/null" (help tool) with
  | 0 ->
    let out = sprintf "man1/%s.1.html" (man tool) in
    run @@ dump_to_file out @@ connect_with_pipes [
      help tool;
      repair_links;
      man2html;
      redirect_index;
    ]
  | _ ->
    eprintf "Warning: can't render manpage for %s\n" tool;
    eprintf "Called as %S, got:\n" (help tool);
    ignore(Sys.command @@ sprintf "%s >&2" (help tool));
    eprintf "\n%!"

let generate_manual () =
  let _frontends =
    List.iter frontends ~f:(fun (p,_) -> build_manual program p) in
  let _plugins =
    Plugins.list () |> List.iter ~f:(fun p ->
        build_manual plugin (Plugin.name p))in
  List.iter packages ~f:generate_man3_redirection

let odig_pkgs () =
  let f = Filename.temp_file "odig" ".pkgs" in
  run (sprintf "odig pkg > %s" f);
  let pkgs =
    In_channel.read_lines f |>
    List.filter_map ~f:(fun s ->
        match String.split s ~on:'?' with
        | x :: _ -> Some (String.strip x)
        | _ -> None) in
  Sys.remove f;
  pkgs

let plugins =
  let by_plugin_name p1 p2 =
    String.compare (Plugin.name p1) (Plugin.name p2) in
  Plugins.list () |> List.sort ~compare:by_plugin_name |>
  List.fold ~init:"" ~f:(fun s p ->
      Format.sprintf "%s%-24s %s\n" s (Plugin.name p) (Plugin.desc p))

let plugins_index =
  sprintf "\n\n{1 Plugins}\n{[%s]}\n" plugins

let library_index =
  sprintf "\n{2 Libraries}\n%s"
    (List.map ~f:render_section libraries
     |> String.concat ~sep:"\n\n")

let remove_unresolved pkgs =
  let pkgs_known_to_odig = odig_pkgs () in
  List.filter pkgs ~f:(fun p ->
      List.mem pkgs_known_to_odig p ~equal:(=))

let generate () =
  let intro,out = Filename.open_temp_file "intro" ".mld" in
  Out_channel.output_string out introduction;
  Out_channel.output_string out library_index;
  Out_channel.output_string out plugins_index;
  Out_channel.close out;
  let pkgs = remove_unresolved packages |> String.concat ~sep:" " in
  run @@ sprintf
    {|odig odoc --index-title="BAP API" --no-tag-index --index-intro=%s %s|}
    intro pkgs;
  run @@ "ln -s $(odig cache path)/html odoc";
  Sys.remove intro

let install_handwritten_manpages () =
  mkdir "man1";
  run "cp ../man/* man1/"

let install_lisp_documentation () =
  mkdir "lisp";
  run "bap /bin/true --primus-lisp-documentation > lisp/index.org";
  run "emacs lisp/index.org --batch --eval '(org-html-export-to-html)'"

let is_installed x = Sys.command (sprintf "which %s" x) = 0

let check_installed x =
  if not (is_installed x) then
    failwith (sprintf "%s is not installed\n" x)

let check () =
  check_installed "bap";
  check_installed "emacs";
  check_installed "man2html";
  check_installed "odig";
  check_installed "sed"

let () =
  check ();
  generate_manual ();
  install_handwritten_manpages ();
  install_lisp_documentation ();
  generate ()
