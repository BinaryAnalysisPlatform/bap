open Bap.Std
open Bap_primus.Std
open Format
include Self()

module Lisp_config = Primus_lisp_config

let load_program paths features project =
  match Primus.Lisp.Load.program ~paths project features with
  | Ok prog -> prog
  | Error err ->
    eprintf "%a@\n" Primus.Lisp.Load.pp_error err;
    exit 2

let dump_program prog =
  let margin = get_margin () in
  set_margin 64;
  printf "%a@\n%!" Primus.Lisp.Load.pp_program prog;
  set_margin margin

let main dump paths features project =
  let prog = load_program paths features project in
  if dump then dump_program prog;

  let module Loader(Machine : Primus.Machine.S) = struct
    module Lisp = Primus.Lisp.Make(Machine)
    open Machine.Syntax

    let print_message msg =
      Machine.return (info "%s" msg)

    let init () = Machine.sequence [
        Lisp.link_program prog;
        Primus.Lisp.message >>> print_message;
      ]
  end in
  Primus.Machine.add_component (module Loader)

let () =
  Config.manpage [
    `S "DESCRIPTION";
    `P "Installs and registers the Primus Lisp library. The library
      implements stdlib interface in Lisp. Only $(i,init) module is
      loaded automatically.";
    `P
      "The plugin also provides an interface through which it is
  possible to load new libraries and modules, or to load existing
  modules.";

    `S "SEE ALSO";
    `P "$(b,bap-primus)(3) $(b,bap-run)(1)"
  ];

  let dump =
    Config.(flag ~doc:"dumps generated AST" "dump") in

  let libs =
    Config.(param (list dir) ~doc:"paths to lisp libraries" "add") in

  let features =
    Config.(param (list string) ~doc:"load specified module" "load"
              ~default:["posix"]) in

  Config.when_ready (fun {Config.get=(!)} ->
      let paths = [Filename.current_dir_name] @ !libs @ [Lisp_config.library] in
      let features = "init" :: !features in
      Project.register_pass' (main !dump paths features))
