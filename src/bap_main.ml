open Core_kernel.Std
open Bap_plugins.Std
open Bap_future.Std
open Regular.Std
open Bap.Std
open Or_error
open Format
open Cmdliner
open Bap_options
open Bap_source_type
include Self()

exception Failed_to_create_project of Error.t
exception Pass_not_found of string

let find_source (type t) (module F : Source.Factory.S with type t = t)
    field o = Option.(field o >>= F.find)

let brancher = find_source (module Brancher.Factory) brancher
let reconstructor =
  find_source (module Reconstructor.Factory) reconstructor

(* This can be seen as an non-optimal merging strategy, as we're
   postponing the emission of the information until everyone is
   ready. Instead, we can emit the information as soon as possible,
   thus allowing the information to flow into the algorithm at early
   stages. But it will still work correctly.
*)
let merge_streams ss ~f : 'a Source.t option =
  List.reduce ss ~f:(Stream.merge ~f:(fun a b -> match a,b with
      | Ok a, Ok b -> Ok (f a b)
      | Error a, Error b -> Error (Error.of_list [a;b])
      | Error e,_|_,Error e -> Error e))

let merge_sources create field (o : Bap_options.t) ~f =  match field o with
  | [] -> None
  | names -> match List.filter_map names ~f:create with
    | [] -> assert false
    | ss -> merge_streams ss ~f

let symbolizer =
  merge_sources Symbolizer.Factory.find symbolizers ~f:(fun s1 s2 ->
      Symbolizer.chain [s1;s2])

let rooter =
  merge_sources Rooter.Factory.find rooters ~f:Rooter.union

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
                List.map ~f:(fun p -> match Project.find_pass p with
                    | Some p -> p
                    | None -> raise (Pass_not_found p)) |>
                run_passes project in
  List.iter options.dump ~f:(function
      | `file dst,fmt,ver ->
        Out_channel.with_file dst ~f:(fun ch ->
            Project.Io.save ~fmt ?ver ch project)
      | `stdout,fmt,ver -> Project.Io.show ~fmt ?ver project)

let main o =
  let digest = digest o in
  let project = match Project.Cache.load digest with
    | Some proj ->
      Project.restore_state proj;
      proj
    | None ->
      let rooter = rooter o
      and brancher = brancher o
      and reconstructor = reconstructor o
      and symbolizer = symbolizer o in
      let input =
        Project.Input.file ~loader:o.loader ~filename: o.filename in
      Project.create input ~disassembler:o.disassembler
        ?brancher ?rooter ?symbolizer ?reconstructor  |> function
      | Error err -> raise (Failed_to_create_project err)
      | Ok project ->
        Project.Cache.save digest project;
        project in
  process o project

let program_info =
  let doc = "Binary Analysis Platform" in
  let man = [
    `S "SYNOPSIS";
    `Pre "
      $(mname) [PLUGIN OPTION]... --list-formats
      $(mname) [PLUGIN OPTION]... [--source-type=$(i,SOURCE)] --list-plugins
      $(mname) [PLUGIN OPTION]... --$(i,PLUGIN)-help
      $(mname) $(i,FILE) [PLUGIN OPTION]... [OPTION]...";
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
      `S "SEE ALSO";
      `P "$(b,bap-mc)(1), $(b,bap-byteweight)(1), $(b,bap)(3)"
    ] in
  Term.info "bap" ~version:Config.version ~doc ~man
let program source =
  let create
      passopt
      a b c d e f g i j k = (Bap_options.Fields.create
                               a b c d e f g i j k []), passopt in
  let open Bap_cmdline_terms in
  let passopt : string list Term.t =
    let doc =
      "Runs passes (comma separated). This option replaces the \
       previously existing $(b,--)$(i,PASS) options which are now \
       deprecated and will soon be removed." in
    Arg.(value & opt (list string) [] &
         info ["p"; "pass"; "passes"] ~doc ~docv:"PASS") in
  Term.(const create
        $passopt
        $filename
        $(disassembler ())
        $(loader ())
        $(dump_formats ())
        $source_type
        $verbose
        $(brancher ())
        $(symbolizers ())
        $(rooters ())
        $(reconstructor ())),
  program_info

let parse_source argv =
  let source_type = Bap_cmdline_terms.source_type in
  match Term.eval_peek_opts ~argv source_type with
  | _,`Ok src -> src
  | Some src,(`Version|`Help) -> src
  | _ -> raise Unrecognized_source

let run_loader () =
  let argv,passes = Bap_plugin_loader.run_and_get_passes ["bap-frontend"] Sys.argv in
  let print_formats =
    Cmdliner.Term.eval_peek_opts Bap_cmdline_terms.list_formats |>
    fst |> Option.value ~default:false in
  if print_formats then print_formats_and_exit ();
  argv,passes

let parse passes argv =
  match Cmdliner.Term.eval ~argv ~catch:false (program source) with
  | `Ok (opts, passopt) ->
    let passes = passopt @ passes in
    { opts with Bap_options.passes }
  | `Error _ -> exit 1;
  | _ -> exit 0

let error fmt =
  kfprintf (fun ppf -> pp_print_newline ppf (); exit 1) err_formatter fmt

let () =
  let () =
    try if Sys.getenv "BAP_DEBUG" <> "0" then
        Printexc.record_backtrace true
    with Not_found -> () in
  Log.start ();
  at_exit (pp_print_flush err_formatter);
  let argv,passes = run_loader () in
  try main (parse passes argv); exit 0 with
  | Unknown_arch arch ->
    error "Invalid arch `%s', should be one of %s." arch
      (String.concat ~sep:"," (List.map Arch.all ~f:Arch.to_string))
  | Unrecognized_source ->
    error "Invalid format of source type argument"
  | Bap_plugin_loader.Plugin_not_found name ->
    error "Can't find a plugin bundle `%s'" name
  | Failed_to_create_project err ->
    error "Failed to create a project: %a" Error.pp err
  | Project.Pass.Failed (Project.Pass.Unsat_dep (p,n)) ->
    error "Dependency `%s' of pass `%s' is not loaded"
      n (Project.Pass.name p)
  | Pass_not_found p -> error "Failed to find pass: %s" p
  | Project.Pass.Failed
      (Project.Pass.Runtime_error
         (p, Exn.Reraised (backtrace, (Invalid_argument msg | Failure msg)))) ->
    error "Pass `%s' failed at runtime with %s\nBacktrace:\n%s"
      (Project.Pass.name p) msg backtrace
  | Project.Pass.Failed
      (Project.Pass.Runtime_error (p, Exn.Reraised (backtrace, exn))) ->
    error "Pass `%s' failed at runtime with: %a\nBacktrace:\n%s"
      (Project.Pass.name p) Exn.pp exn backtrace
  | exn ->
    error "Failed with an unexpected exception: %a\nBacktrace:\n%s"
      Exn.pp exn
    @@ Exn.backtrace ()
