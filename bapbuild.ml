open Ocamlbuild_pack
open Ocamlbuild_plugin
open Core_kernel.Std

let syntax_packages = List.map [
    "sexplib";
    "comparelib";
    "fieldslib";
    "variantslib";
    "bin_prot";
    "enumerate";
    "herelib";
    "pa_ounit";
  ] ~f:(fun pkg -> pkg ^ "." ^ "syntax")

let default_pkgs = ["bap"; "bap.plugins"; "core_kernel"]

(* there is no way to figure out what libraries were linked into
   the bap executable in the runtime. Later we can try to figure
   this out from the _oasis file for example, but right now I will
   hardcode the dependencies here.
*)
let bap_packages = [
  "bitstring";
  "camlp4";
  "cmdliner";
  "core_kernel";
  "curl";
  "dynlink";
  "ezjsonm";
  "ezjsonm";
  "fileutils";
  "fileutils.str";
  "findlib";
  "ocamlgraph";
  "ocamlgraph";
  "re";
  "re.posix";
  "uri";
  "uuidm";
  "zarith";
  "FrontC";
]

let packages = default_pkgs @ syntax_packages @ bap_packages

let default_tags = [
  "thread";
  "debug";
  "annot";
  "bin_annot";
  "short_paths"
]

let set_default_options () : unit =
  Options.(begin
      use_ocamlfind := true;
      ocaml_syntax := Some "camlp4o";
      ocaml_pkgs := packages;
      tags := default_tags;
    end)

let extern_deps_link_flags () =
  let interns = packages |>
                List.map ~f:Findlib.query |>
                Findlib.topological_closure in
  !Options.ocaml_pkgs |>
  List.map ~f:Findlib.query |>
  Findlib.topological_closure |>
  List.filter ~f:(fun pkg -> not (List.mem interns pkg)) |>
  Findlib.link_flags_native

let symlink env =
  if Options.make_links.contents then
    Cmd (S [A"ln"; A"-sf";
            P (env (!Options.build_dir / "%.plugin"));
            A Pathname.parent_dir_name])
  else Nop

let plugin () =
  rule "bap: cmxa & a -> plugin"
    ~prods:["%.plugin"]
    ~deps:["%.cmxa"; "%" -.- !Options.ext_lib]
    (fun env _ ->
       Seq [Cmd (S [
           !Options.ocamlopt;
           A "-linkpkg";
           A "-shared";
           S [extern_deps_link_flags ()];
           P (env "%.cmxa");
           A "-o"; Px (env "%.plugin")
         ]);
          symlink env
         ])

let main () =
  set_default_options ();
  flag ["ocaml"; "library"; "link"]  (A"-linkall");
  Command.jobs := 4;
  Ocamlbuild_plugin.dispatch (function
      | Before_rules -> plugin ()
      | _ -> ());
  Ocamlbuild_unix_plugin.setup ();
  Main.main ()

let () = main ()
