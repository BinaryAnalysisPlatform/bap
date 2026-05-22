open Printf
open Base
open Stdio
open Sexplib

module Buf = Stdlib.Buffer
module Arg = Stdlib.Arg
module Cfg = Configurator.V1
module Sys = Stdlib.Sys
module Format = Stdlib.Format

type cls =
  | Help
  | With
  | Comp of bool

module Key = struct
  type t = string [@@deriving sexp]
  let normalize =
    String.map ~f:(function
        | '_' | ' ' -> '-'
        | c -> c)

  let sexp_of_t x =
    sexp_of_string @@ String.lowercase @@ normalize x

  let hash x = String.Caseless.hash @@ normalize x
  include Comparable.Make(struct
      type t = string
      let sexp_of_t = sexp_of_t
      let compare x y = String.Caseless.compare
          (normalize x)
          (normalize y)
    end)
end

type cfg = string Hashtbl.M(Key).t [@@deriving sexp]

let filename = ref ""
let init = ref false
let args = [
  "-filename", Arg.Set_string filename, "NAME the name of the file";
  "-init", Arg.Set init, "BOOL creates the configuration file"
]

let () = Cfg.main ~args ~name:"bap-configurator" @@ fun self ->
  let ocamlc ?name var =
    Option.value name ~default:var,
    Cfg.ocaml_config_var_exn self var in

  let prefix =
    let stdlib = Cfg.ocaml_config_var_exn self "standard_library" in
    String.chop_suffix_exn stdlib "/lib/ocaml" in

  let build_id =
    try Cfg.Process.run_capture_exn self "git" [
        "rev-parse";
        "--verify";
        "--quiet";
        "--short=7";
        "HEAD"
      ] |> String.strip
    with _ -> "" in

  let name = "bap-common" in


  let site name =
    let sites = Bap_common.sites name in
    "site_" ^ name,
    List.map sites ~f:(sprintf "%S") |>
    String.concat ~sep:"; " in

  if init.contents then begin
    let vars = Hashtbl.of_alist_exn (module Key) [
        "pkg_name", name;
        "pkg_version", "2.6.0-alpha";
        "prefix", prefix;
        "exec_prefix", "$prefix";
        "bindir", "$exec_prefix/bin";
        "sbindir", "$exec_prefix/sbin";
        "libexecdir", "$exec_prefix/libexec";
        "sysconfdir", "$prefix/etc";
        "sharedstatedir", "$prefix/com";
        "localstatedir", "$prefix/var";
        "libdir", "$exec_prefix/lib";
        "datarootdir", "$prefix/share";
        "datadir", "$datarootdir";
        "infodir", "$datarootdir/info";
        "localedir", "$datarootdir/locale";
        "mandir", "$datarootdir/man";
        "docdir", "$datarootdir/doc/$pkg_name";
        "htmldir", "$docdir";
        "dvidir", "$docdir";
        "pdfdir", "$docdir";
        "psdir", "$docdir";
        "plugindir", "$libdir/$pkg_name/plugins";
        "debug", "true";
        "profile", "false";
        "build_id", build_id;
        "llvm-config", "llvm-config";
        "cxxfilt_paths", "[]";
        "ida_path", "";
        "ida_headless", "false";
        "objdump_paths", "[]";
        site "api";
        site "lisp";
        site "plugins";
        site "primus";
        site "semantics";
        site "signatures";
        site "site_lisp";
        ocamlc ~name:"ocaml_version" "version";
        ocamlc "os_type";
        ocamlc "system";
        ocamlc "architecture";
        ocamlc "host";
        ocamlc "target";
        ocamlc "standard_library";
        ocamlc ~name:"suffix_program" "ext_exe";
      ] in

    let classes = [
      "--enable-",  Comp true;
      "--disable-", Comp false;
      "--help", Help;
      "-help", Help;
      "--with-", With;
    ] in

    let add_variable spec =
      match String.split_on_chars spec ~on:['='] with
      | [key;data] ->
        Hashtbl.set vars ~key ~data
      | _ ->
        Cfg.die "An invalid variable specification %S\n" spec in

    let classify arg = List.find_map classes ~f:(fun ((prefix,_) as cls) ->
        Option.some_if (String.is_prefix ~prefix arg) cls) in

    Arg.read_arg "config.status.in" |> Array.iter ~f:(fun arg ->
        match classify arg with
        | Some (_,Help) -> Stdlib.exit 0
        | Some (_,Comp _) -> ()
        | Some (pref,With) ->
          add_variable (String.subo arg ~pos:(String.length pref))
        | None when String.is_prefix arg ~prefix:"--" ->
          add_variable (String.subo ~pos:2 arg)
        | None ->
          eprintf "Invalid ./configure argument %S\n" arg);
    Format.printf "%a@." Sexp.pp_hum (sexp_of_cfg vars);
    Stdlib.exit 0;
  end;

  let vars =
    cfg_of_sexp @@
    Sexp.of_string @@ In_channel.read_all "config.status" in

  let rewrite_var var =
    match Hashtbl.find vars var with
    | None ->
      eprintf "Warning %s: unknown variable %S in %s\n"
        Sys.argv.(0) var !filename;
      var
    | Some s -> s in

  let substitute_one str =
    let buf = Buffer.create (String.length str) in
    Buf.add_substitute buf rewrite_var str;
    Buf.contents buf in

  let rec substitute str =
    let res1 = substitute_one str in
    let res2 = substitute_one res1 in
    if String.equal res1 res2 then res2
    else substitute res2 in

  In_channel.iter_lines In_channel.stdin ~f:(fun str ->
      printf "%s\n" (substitute str))
