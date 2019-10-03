let man = {|
# DESCRIPTION

A frontend to the Binary Analysis Platform library.  The tool allows
you to inspect binary programs by printing them in different
representations including assembly, BIL, BIR, XML, HTML, JSON,
Graphviz dot graphs and so on.

The tool is extensible via a plugin system. There're several
extension points, that allows you:

      - write your own analysis;
      - add new serialization formats;
      - adjust printing formats;
      - add new program loaders (i.e. to handle new file formats);
      - add your own symbolizer, rooter or reconstructor;
      - provide ABI information;
      - tackle with disassembler, lifter and even architecture;
      - provide your own disassembler.

The following example shows how to write a simple analysis plugin
(called a pass in our parlance):

```
      $ cat mycode.ml
```

# BUILDING PLUGINS

Building is easy with our $(b,bapbuild) tool:

```
  bapbuild mycode.plugin
  bapbundle install mycode.plugin
```

User plugins have access to all the program state, and can change it
and communicate with other plugins, or just store their results in
whatever place you like.

- Note: The $(b,bapbuild) tool is just an $(b,ocamlbuild)
extended with our rules. It is not needed (but still can be used)
to build your standalone applications, or to build BAP itself.

# BUGS

Report bugs to
  https://github.com/BinaryAnalysisPlatform/bap/issues

# SEE ALSO

$(b,bapbundle)(1), $(b,bapbuild)(1), $(b,bap)(3)"

|}
open Bap.Std
open Core_kernel
open Bap_main.Extension

module type unit = sig end

(* to preserve backward compatibility we automatically add
   the disassemble command if the first argument is a file.

   However, we don't want to shadow possible commands with
   files in the current folder, so the filename should be
   explicit, e.g., `bap configure` will be treated as command
   even if there is a file `configure` in the current folder,
   while `bap ./configure` will be interpreted as
   ```
   bap disassemble ./configure
   ```
*)
let is_explicit_file file =
  Sys.file_exists file &&
  not (Sys.is_directory file) &&
  (String.contains file '/')

let infer_command args = match Array.to_list args with
  | name :: file :: args when is_explicit_file file ->
    Array.of_list (name :: "disassemble" :: file :: args)
  | _ -> args

let pp_info ppf infos =
  List.iter infos ~f:(fun info ->
      Format.fprintf ppf "  %-24s %s@\n"
        (Context.name info)
        (Context.doc info))

let admin_commands ctxt = Context.commands ctxt
let commands ctxt = Context.commands ctxt
let plugins ctxt = Context.plugins ctxt


let print_info ctxt =
  Result.return @@
  Format.printf {|
Usage:
  bap <COMMAND> [<OPTIONS>]

Common options:
  --version                prints the version number and exits;
  --plugin-path <DIR>      adds <DIR> to the plugins search path;
  --logdir <DIR>           creates a log file in <DIR>;
  --recipe <VAL>           extracts command line arguments from the recipe.

Management commands:
%a

Commands:
%a

Plugins:
%a

If no command is specified and the first parameter is a file,
then the `disassemble' command is assumed.

Run 'bap <COMMAND> --help' for more information a command.
Run 'bap --<PLUGIN>-help for more information about a plugin.
Run 'bap --help' for the detailed manual.
|}
    pp_info (admin_commands ctxt)
    pp_info (commands ctxt)
    pp_info (plugins ctxt)





let () =
  let _unused : (module unit) = (module Bap.Std) in
  let argv = infer_command Sys.argv in
  match Bap_main.init ~default:print_info ~name:"bap" ~man ~argv () with
  | Ok () -> ()
  | Error (Error.Exit_requested code) -> exit code
  | Error err -> Format.eprintf "%a@\n%!" Error.pp err;
    exit 1
