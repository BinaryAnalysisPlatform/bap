open Core_kernel.Std
open Bap_plugins.Std
open Regular.Std
open Bap.Std
open Or_error
open Format
open Cmdliner
open Bap_options
open Bap_source_type

exception Failed_to_load_file of Error.t
exception Failed_to_create_project of Error.t
exception Unknown_format of string

let brancher o src args =
  Option.(o.brancher >>= fun b -> Brancher.Factory.find src b args)

let reconstructor o src args =
  Option.(o.reconstructor >>= fun b -> Reconstructor.Factory.find src b args)

let symbolizer o src args = match o.symbolizers with
  | [] -> None
  | ss ->
    List.filter_map ss ~f:(fun s -> Symbolizer.Factory.find src s args) |>
    Symbolizer.chain |>
    Option.some

let choose_default_rooter src args =
  match Rooter.Factory.list src with
  | r :: _ -> Rooter.Factory.find src r args
  | _ -> None

let union_rooters ss src args =
  List.filter_map ss ~f:(fun s ->
      Rooter.Factory.find src s args) |>
  List.reduce ~f:Rooter.union

let rooter o src args = match o.rooters with
  | [] -> choose_default_rooter src args
  | ss -> union_rooters ss src args

let rooter o src args = match rooter o src args with
  | None -> assert false
  | rooter -> rooter

let project_or_exn = function
  | Ok p -> p
  | Error e -> raise (Failed_to_create_project e)

let memory arch file =
  let width_of_arch = arch |> Arch.addr_size |> Size.in_bits in
  let addr = Addr.of_int 0 ~width:width_of_arch in
  Memory.of_file (Arch.endian arch) addr file |> function
  | Ok mem -> mem
  | Error err -> raise (Failed_to_load_file err)

let image options =
  match Image.create ~backend:options.loader options.filename with
  | Ok (img,[]) -> img
  | Ok (img,warns) ->
    if options.verbose then
      List.iter warns ~f:(fun err -> eprintf "Warning: %a" Error.pp err);
    img
  | Error err -> raise (Failed_to_load_file err)

let print_formats_and_exit () =
  Bap_format_printer.run `writers (module Project);
  exit 0

let args filename argv =
  let passes = Project.passes () |>
               List.map ~f:Project.Pass.name in
  let transparent_args =
    "-d" :: "--dump" ::
    "-v" :: "--verbose" ::
    filename ::
    List.map passes ~f:(fun p -> "--"^p) in
  let is_key = String.is_prefix ~prefix:"-" in
  let is_transparent arg =
    if is_key arg
    then List.exists transparent_args
        ~f:(fun prefix -> String.is_prefix ~prefix arg)
    else List.mem transparent_args arg in
  let inputs = String.Hash_set.of_list @@ list_loaded_units () in
  Array.iteri argv ~f:(fun i arg -> match i with
      | 0 -> ()
      | i when is_transparent arg -> ()
      | i when is_key arg -> Hash_set.add inputs arg
      | i ->
        let pre = argv.(i-1) in
        if not(is_key pre && is_transparent pre)
        then Hash_set.add inputs arg);
  String.Hash_set.sexp_of_t inputs |>
  Sexp.to_string_mach

let digest o =
  Data.Cache.digest ~namespace:"project" "%s%s"
    (Digest.file o.filename)
    (args o.filename Sys.argv)

let process options project =
  let run_passes init = List.fold ~init ~f:(fun proj pass ->
      Project.Pass.run_exn pass proj) in
  let project = Project.passes () |>
                List.filter ~f:Project.Pass.autorun |>
                run_passes project in
  let project = options.passes |>
                List.filter_map ~f:Project.find_pass |>
                run_passes project in
  List.iter options.dump ~f:(function
      | `file dst,fmt,ver ->
        Out_channel.with_file dst ~f:(fun ch ->
            Project.Io.save ~fmt ?ver ch project)
      | `stdout,fmt,ver -> Project.Io.show ~fmt ?ver project)


let create name options source arg =
  let options = {
    options with
    rooters = options.symbols @ options.rooters;
    symbolizers = options.symbols @ options.symbolizers;
  } in
  match Project.Factory.find source name arg with
  | None -> raise (Unknown_format name)
  | Some create ->
    let make cons = cons options source arg in
    create
      ~disassembler:options.disassembler
      ?brancher:(make brancher)
      ?symbolizer:(make symbolizer)
      ?rooter:(make rooter)
      ?reconstructor:(make reconstructor) () |> project_or_exn

let main o =
  let digest = digest o in
  let project = match Project.Cache.load digest with
    | Some proj ->
      Project.restore_state proj;
      proj
    | None ->
      let project = match o.source with
        | `File fmt -> create fmt o Source.File o.filename
        | `Binary -> create "builtin" o Source.Binary (image o)
        | `Memory arch ->
          let memory = memory arch o.filename, arch in
          create "builtin" o Source.Memory memory in
      Project.Cache.save digest project;
      project in
  process o project

let program_info =
  let doc = "Binary Analysis Platform" in
  let man = [
    `S "SYNOPSIS";
    `Pre "
      $(b,$mname) [PLUGIN OPTION]... --list-formats
      $(b,$mname) [PLUGIN OPTION]... [--source-type=$(i,SOURCE)] --list-plugins
      $(b,$mname) [PLUGIN OPTION]... --$(i,PLUGIN)-help
      $(b,$mname) $(i,FILE) [PLUGIN OPTION]... [OPTION]...";
    `S "DESCRIPTION";
    `P "A frontend to the Binary Analysis Platfrom library.
      The tool allows you to inspect binary programs by printing them
      in different representations including assembly, BIL, BIR,
      XML, HTML, JSON, Graphviz dot graphs and so on.";
    `P "The tool is extensible via a plugin system. There're several
       extension points, that allows you:";
    `Pre "
      - write your own analysis;
      - add new serialization formats;
      - adjust printing formats;
      - add new program loaders (i.e. to handle new file formats);
      - add your own symbolizer, rooter or reconstructor;
      - provide ABI information;
      - tackle with disassembler, lifter and even architecture;
      - provide your own disassembler.";
    `P "The following example shows how to write a simple analysis
  plugin (called a pass in our parlance)";
    `Pre "
      $(b,\\$ cat) mycode.ml
      open Bap.Std
      let main project = print_endline \"Hello, World\"
      let () = Project.register_pass' main";
    `P "Building is easy with our $(b,bapbuild) tool:";
    `Pre "
      $(b, \\$ bapbuild) mycode.plugin";
    `P "And to load into bap:";
    `Pre ("
      $(b, \\$ bap) /bin/ls -lmycode --mycode");
    `P "User plugins have access to all the program state, and can
    change it and communicate with other plugins, or just store their
    results in whatever place you like.";
    `I ("Note:", "The $(b,bapbuild) tool is just an $(b,ocamlbuild)
    extended with our rules. It is not needed (but still can be used)
    to build your standalone applications, or to build BAP itself.");
    `S "OPTIONS";
    `I ("$(b,--list-formats)", Bap_cmdline_terms.list_formats_doc)
  ] @ Bap_cmdline_terms.common_loader_options
    @ Bap_cmdline_terms.options_for_passes
    @ [
      `S "BUGS";
      `P "Report bugs to \
          https://github.com/BinaryAnalysisPlatform/bap/issues";
      `S "SEE ALSO"; `P "$(b,bap-mc)(1)"
    ] in
  Term.info "bap" ~version:Config.version ~doc ~man
let program source =
  let create
      a b c d e f g i j k l = Bap_options.Fields.create
      a b c d e f g i j k l [] in
  let open Bap_cmdline_terms in
  Term.(const create
        $filename
        $(disassembler ())
        $(loader source)
        $(dump_formats ())
        $source_type
        $verbose
        $(brancher source)
        $(symbolizers source)
        $(rooters source)
        $(symbols source)
        $(reconstructor source)),
  program_info

let parse_source argv =
  let source_type = Bap_cmdline_terms.source_type in
  match Term.eval_peek_opts ~argv source_type with
  | _,`Ok src -> src
  | Some src,(`Version|`Help) -> src
  | _ -> raise Unrecognized_source


let parse () =
  let argv,passes = Bap_plugin_loader.run_and_get_passes Sys.argv in
  let print_formats =
    Cmdliner.Term.eval_peek_opts Bap_cmdline_terms.list_formats |>
    fst |> Option.value ~default:false in
  if print_formats then print_formats_and_exit ();
  let source = parse_source argv in
  match Cmdliner.Term.eval ~argv ~catch:false (program source) with
  | `Ok opts -> { opts with Bap_options.passes }
  | _ -> exit 0

let error fmt =
  kfprintf (fun ppf -> pp_print_newline ppf (); exit 1) err_formatter fmt

let () =
  at_exit (pp_print_flush err_formatter);
  Printexc.record_backtrace true;
  try main (parse ()); exit 0 with
  | Unknown_arch arch ->
    error "Invalid arch `%s', should be one of %s." arch
      (String.concat ~sep:"," (List.map Arch.all ~f:Arch.to_string))
  | Unrecognized_source ->
    error "Invalid format of source type argument"
  | Bap_plugin_loader.Plugin_not_found name ->
    error "Can't find a plugin bundle `%s'" name
  | Failed_to_load_file err ->
    error "Failed to load a code file: %a" Error.pp err
  | Failed_to_create_project err ->
    error "Failed to create a project: %a" Error.pp err
  | Unknown_format fmt ->
    error "Format `%s' is not known" fmt
  | Project.Pass.Failed (Project.Pass.Unsat_dep (p,n)) ->
    error "Dependency `%s' of pass `%s' is not loaded"
      n (Project.Pass.name p)
  | Project.Pass.Failed (Project.Pass.Runtime_error (p,exn)) ->
    error "Pass `%s' failed at runtime with: %a"
      (Project.Pass.name p) Exn.pp exn
  | exn ->
    error "Failed with an unexpected exception: %a\nBacktrace:\n%s"
      Exn.pp exn
    @@ Exn.backtrace ()
