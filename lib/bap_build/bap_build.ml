module Plugin_rules = struct
  module Fl = Findlib

  open Ocamlbuild_plugin
  open Core_kernel.Std
  module Ocamlbuild = Ocamlbuild_pack


  let (/) = Pathname.concat
  let () =
    let libs = Bap_config.standard_library / "compiler-libs" in
    Unix.putenv "OCAMLFIND_IGNORE_DUPS_IN" libs

  let packages = ["bap"; "core_kernel"; "ppx_jane"]

  let default_predicates = [
    "custom_ppx";
    "ppx_driver";
  ]

  let default_tags = [
    "thread";
    "debug";
    "short_paths";
    "custom";
    "pp(ppx-jane -dump-ast -inline-test-drop)"
  ] @ List.map default_predicates ~f:(sprintf "predicate(%s)")

  let needs_threads ~predicates pkgs =
    let deps = Fl.package_deep_ancestors predicates pkgs in
    List.mem deps "threads" ~equal:String.equal

  let infer_thread_predicates ~predicates pkg =
    if needs_threads ~predicates pkg
    then "mt" :: "mt_posix" :: predicates
    else predicates

  let bap_predicates ~native =
    let code = if native then "native" else "byte" in
    let predicates = code :: default_predicates in
    infer_thread_predicates ~predicates packages

  let pkg_predicates ~native =
    infer_thread_predicates ~predicates:(bap_predicates ~native)
      !Options.ocaml_pkgs

  let topological_closure ~predicates pkgs =
    Fl.package_deep_ancestors predicates pkgs

  let set_default_options () : unit =
    Command.jobs := 4;
    Options.(begin
        use_ocamlfind := true;
        ocaml_pkgs := packages;
        tags := default_tags;
        recursive := true;
      end)

  let interns () =
    topological_closure
      ~predicates:(bap_predicates ~native:true) packages

  let findlibs
      ?(native=true)
      ?(predicates=pkg_predicates ~native)
      ~dynamic pkg =
    try
      let preds = if dynamic
        then "plugin" :: predicates
        else predicates in
      let arch,preds = Fl.package_property_2 preds pkg "archive" in
      let base = Fl.package_directory pkg in
      if dynamic && not (List.mem ~equal:Polymorphic_compare.equal preds (`Pred "plugin"))
      then raise Not_found;
      String.split ~on:' ' arch |>
      List.map ~f:(Fl.resolve_path ~base)
    with Not_found -> []

  let externals pkgs =
    let interns = interns () in
    pkgs |>
    topological_closure ~predicates:(pkg_predicates ~native:true) |>
    List.filter ~f:(fun dep -> not (List.mem ~equal:String.equal interns dep))

  let packages () = externals !Options.ocaml_pkgs

  let symlink env =
    if Options.make_links.contents then
      Cmd (S [A"ln"; A"-sf";
              P (env (!Options.build_dir / "%.plugin"));
              A Pathname.parent_dir_name])
    else Nop


  let link_shared_bytecode ~src ~dst =
    Cmd (S [
        !Options.ocamlc;
        A "-linkall"; A "-a";
        P src; A "-o"; Px dst
      ])

  let link_shared_native ~src ~dst =
    Cmd (S [
        !Options.ocamlopt;
        A "-shared";
        A "-linkall";
        A "-ccopt"; A "-L";
        A "-ccopt"; A (Filename.dirname src);
        P src; A "-o"; Px dst])

  let generate_cmxs_of_lib lib =
    let dst = Filename.(basename (chop_extension lib) ^ ".cmxs") in
    link_shared_native ~src:lib ~dst

  let generate_cma_of_lib lib =
    let dst = Filename.(basename (chop_extension lib) ^ ".cma") in
    link_shared_bytecode ~src:lib ~dst

  let generate_plugin_for_package code name =
    let native = code = `native in
    let linker = match code with
      | `native -> generate_cmxs_of_lib
      | `byte -> generate_cma_of_lib in
    match findlibs ~native ~dynamic:true name with
    | [] -> findlibs ~native ~dynamic:false name |> List.map ~f:linker
    | xs ->
      List.map xs ~f:(fun src -> cp src Pathname.current_dir_name)

  let generate_plugins_for_packages () =
    packages () |>
    List.concat_map ~f:(fun name ->
        List.concat_map [`native; `byte] ~f:(fun code ->
            generate_plugin_for_package code name))

  let make_list_option option = function
    | [] -> N
    | xs -> S [A option; A (String.concat ~sep:"," xs)]

  let is_cmx file = Filename.check_suffix file ".cmx"

  let bundle env =
    let requires =
      packages () |> List.concat_map ~f:(fun pkg ->
          findlibs ~dynamic:false pkg |>
          List.map ~f:(fun path ->
              let name = path |>
                         Filename.chop_extension |>
                         Filename.basename in
              name^"="^name^".cmxs,"^
              name^"="^name^".cma")) |>
      make_list_option "-requires" in
    let provides = Sys.readdir Pathname.current_dir_name |>
                   Array.to_list |>
                   List.filter ~f:is_cmx |>
                   List.map ~f:Filename.chop_extension |>
                   make_list_option "-provides" in
    Cmd (S [
        A "bapbundle"; A "pack";
        T (Tags.of_list ["bundle"; "library"; "plugin"]);
        A "-name"; A (env "%");
        A "-main"; A (env "%.cmxs");
        A "-main"; A (env "%.cma");
        requires; provides;
        Px (env "%.plugin")
      ])

  let register_cmxs_of_cmxa_rule () =
    rule "bap: cmxa & a -> cmxs"
      ~prods:["%.cmxs"]
      ~deps:["%.cmxa"; "%" -.- !Options.ext_lib]
      (fun env _ ->
         link_shared_native ~src:(env "%.cmxa") ~dst:(env "%.cmxs"))

  let register_collect_bundle_rule () =
    rule "bap: cmxs & packages -> bundle"
      ~deps:["%.cmxs"]
      ~stamp:"%.bundle"
      (fun env _ -> Seq (generate_plugins_for_packages ()))

  let register_plugin_rule () =
    rule "bap: cmxs & cma & bundle -> plugin"
      ~prods:["%.plugin"]
      ~deps:["%.bundle"; "%.cmxs"; "%.cma"]
      (fun env _ -> Seq [bundle env; symlink env])

let pass_pp_to_link_phase () =
  pflag ["ocaml"; "link"] "pp" (fun s -> S [A "-pp"; A s])

  let install () =
    register_cmxs_of_cmxa_rule ();
    register_collect_bundle_rule ();
    register_plugin_rule ();
    pass_pp_to_link_phase ();
end


module Std = struct

  module Plugin_rules = Plugin_rules
  module Plugin_options = struct
    let set = Plugin_rules.set_default_options
  end
end
