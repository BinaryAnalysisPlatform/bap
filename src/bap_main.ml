open Bap_knowledge

open Core_kernel
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

module Recipe = Bap_recipe

exception Failed_to_create_project of Error.t [@@deriving sexp]
exception Pass_not_found of string [@@deriving sexp]


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
    else List.mem ~equal:String.equal transparent_args arg in
  let inputs = String.Hash_set.of_list @@ list_loaded_units () in
  Array.iteri argv ~f:(fun i arg -> match i with
      | 0 -> ()
      | _ when is_transparent arg -> ()
      | _ when is_key arg -> Hash_set.add inputs arg
      | i ->
        let pre = argv.(i-1) in
        if not(is_key pre && is_transparent pre)
        then Hash_set.add inputs arg);
  String.Hash_set.sexp_of_t inputs |>
  Sexp.to_string_mach

let digest ~namespace o =
  Data.Cache.digest ~namespace "%s%s"
    (Caml.Digest.(file o.filename |> to_hex))
    (args o.filename Sys.argv)

let run_passes base init = List.foldi ~init ~f:(fun i proj pass ->
    report_progress
      ~stage:(i+base)
      ~note:(Project.Pass.name pass) ();
    Project.Pass.run_exn pass proj)

let process options project =
  let autoruns = Project.passes () |>
                 List.filter ~f:Project.Pass.autorun in

  let passes = options.passes |>
               List.map ~f:(fun p -> match Project.find_pass p with
                   | Some p -> p
                   | None -> raise (Pass_not_found p)) in
  let autos = List.length autoruns in
  let total = List.length passes + autos in
  report_progress ~note:"analyzing" ~total ();
  let project = run_passes 0 project autoruns in
  let project = run_passes autos project passes in
  List.iter options.dump ~f:(function
      | `file dst,fmt,ver ->
        Out_channel.with_file dst ~f:(fun ch ->
            Project.Io.save ~fmt ?ver ch project)
      | `stdout,fmt,ver ->
        Project.Io.show ~fmt ?ver project)

let knowledge_cache () =
  let reader = Data.Read.create
      ~of_bigstring:Knowledge.of_bigstring () in
  let writer = Data.Write.create
      ~to_bigstring:Knowledge.to_bigstring () in
  Data.Cache.Service.request reader writer

let project_state_cache () =
  let module State = struct
    type t = Project.state [@@deriving bin_io]
  end in
  let of_bigstring = Binable.of_bigstring (module State) in
  let to_bigstring = Binable.to_bigstring (module State) in
  let reader = Data.Read.create ~of_bigstring () in
  let writer = Data.Write.create ~to_bigstring () in
  Data.Cache.Service.request reader writer

let import_knowledge_from_cache digest =
  let digest = digest ~namespace:"knowledge" in
  info "looking for knowledge with digest %a"
    Data.Cache.Digest.pp digest;
  let cache = knowledge_cache () in
  match Data.Cache.load cache digest with
  | None -> ()
  | Some state ->
    info "importing knowledge from cache";
    Toplevel.set state

let load_project_state_from_cache digest =
  let digest = digest ~namespace:"project" in
  let cache = project_state_cache () in
  Data.Cache.load cache digest

let save_project_state_to_cache digest state =
  let digest = digest ~namespace:"project" in
  let cache = project_state_cache () in
  Data.Cache.save cache digest state

let store_knowledge_in_cache digest =
  let digest = digest ~namespace:"knowledge" in
  info "caching knowledge with digest %a"
    Data.Cache.Digest.pp digest;
  let cache = knowledge_cache () in
  Toplevel.current () |>
  Data.Cache.save cache digest


let main ({filename; loader; disassembler} as opts) =
  let digest = digest opts in
  import_knowledge_from_cache digest;
  let state = load_project_state_from_cache digest in
  let proj_of_input input =
    Project.create ?state input ~disassembler |> function
    | Error err -> raise (Failed_to_create_project err)
    | Ok project -> project in
  let project =
    match opts.source with
    | `Project -> failwith "Unsupported feature: project of file"
    | `Memory arch ->
      proj_of_input @@
      Project.Input.binary arch ~filename
    | `Binary ->
      proj_of_input @@
      Project.Input.file ~loader ~filename in
  if Option.is_none state then begin
    store_knowledge_in_cache digest;
    save_project_state_to_cache digest (Project.state project);
  end;
  process opts project

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
    `P "A frontend to the Binary Analysis Platform library.
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
    @ Bap_cmdline_terms.recipe_doc
    @ [
      `S "BUGS";
      `P "Report bugs to \
          https://github.com/BinaryAnalysisPlatform/bap/issues";
      `S "SEE ALSO";
      `P "$(b,bap-mc)(1), $(b,bap-byteweight)(1), $(b,bap)(3)"
    ] in
  Term.info "bap" ~version:Config.version ~doc ~man

let program _source =
  let create
      passopt
      _ _ a b c d e f = (Bap_options.Fields.create
                           a b c d e f []), passopt in
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
        $recipe
        $logdir
        $filename
        $(disassembler ())
        $(loader ())
        $(dump_formats ())
        $source_type
        $verbose),
  program_info

let parse_source argv =
  let source_type = Bap_cmdline_terms.source_type in
  match Term.eval_peek_opts ~argv source_type with
  | _,`Ok src -> src
  | Some src,(`Version|`Help) -> src
  | _ -> raise Unrecognized_source

let get_logdir argv =
  match Cmdliner.Term.eval_peek_opts ~argv
          Bap_cmdline_terms.logdir with
  | _,`Ok r -> r
  | _ -> None


let recipe_paths = [
  Filename.current_dir_name;
  Config.datadir
]

let eval_recipe name =
  match Recipe.load ~paths:recipe_paths name with
  | Ok r ->
    at_exit (fun () -> Recipe.cleanup r);
    Array.concat [Sys.argv; Recipe.argv r]
  | Error err ->
    eprintf "Failed to load recipe %s: %a\n%!" name
      Recipe.pp_error err;
    exit 1


let print_recipe r =
  printf "DESCRIPTION@\n@\n%s@\n@\n" (Recipe.descr r);
  let params = Recipe.params r in
  if params <> [] then begin
    printf "PARAMETERS@\n@\n";
    List.iter params ~f:(printf "- %a@\n" Recipe.pp_param);
    printf "@\n";
  end;
  let args = Recipe.argv r in
  let sep = if Array.length args > 4 then " \\\n" else " " in
  printf "COMMAND LINE@\n@\n%s@\n" (String.concat_array ~sep args)

let summary str =
  match String.index str '\n' with
  | None -> str
  | Some p -> String.subo ~len:p str

let print_recipes_and_exit () =
  let (/) = Filename.concat in
  List.iter recipe_paths ~f:(fun dir ->
      if Sys.file_exists dir &&
         Sys.is_directory dir
      then Array.iter (Sys.readdir dir) ~f:(fun entry ->
          let file = dir / entry in
          if Filename.check_suffix file ".recipe"
          then
            let name = Filename.chop_suffix entry ".recipe" in
            match Recipe.load ~paths:recipe_paths name with
            | Ok r ->
              printf "%-32s %s\n" (Filename.basename name)
                (summary (Recipe.descr r));
              Recipe.cleanup r
            | Error err ->
              eprintf "Malformed recipe %s: %a@\n%!" file
                Recipe.pp_error err));
  exit 0

let handle_recipes = function
  | None -> print_recipes_and_exit ()
  | Some name -> match Recipe.load ~paths:recipe_paths name with
    | Ok r ->
      print_recipe r;
      Recipe.cleanup r;
      exit 0
    | Error err ->
      eprintf "Malformed recipe: %a@\n%!" Recipe.pp_error err;
      exit 1



let is_specified opt ~default =
  Cmdliner.Term.eval_peek_opts opt |>
  fst |> Option.value ~default

let run_loader argv =
  let argv,passes = Bap_plugin_loader.run_and_get_passes ["bap-frontend"] argv in
  let print_formats = is_specified Bap_cmdline_terms.list_formats ~default:false in
  let print_recipes = is_specified Bap_cmdline_terms.list_recipes ~default:None  in
  if print_formats then print_formats_and_exit ();
  Option.iter print_recipes ~f:handle_recipes;
  argv,passes

let parse passes argv =
  match Cmdliner.Term.eval ~argv ~catch:false (program source) with
  | `Ok (opts, passopt) ->
    let passes = passopt @ passes in
    { opts with Bap_options.passes }
  | `Error _ -> exit 1;
  | _ -> exit 0

let error fmt =
  kasprintf (fun str ->
      error "%s" str;
      exit 1) fmt

let load_recipe () =
  match Cmdliner.Term.eval_peek_opts Bap_cmdline_terms.recipe with
  | _,`Ok (Some r) -> eval_recipe r
  | _ -> Sys.argv

let nice_pp_error fmt er =
  let module R = Info.Internal_repr in
  let rec pp_sexp fmt = function
    | Sexp.Atom x -> Format.fprintf fmt "%s\n" x
    | Sexp.List xs -> List.iter ~f:(pp_sexp fmt) xs in
  let rec pp fmt r =
    let open R in
    match r with
    | With_backtrace (r, backtrace) ->
      Format.fprintf fmt "%a\n" pp r;
      Format.fprintf fmt "Backtrace:\n%s" @@ String.strip backtrace
    | String s -> Format.fprintf fmt "%s" s
    | r -> pp_sexp fmt (R.sexp_of_t r) in
  Format.fprintf fmt "%a" pp (R.of_info (Error.to_info er))

let setup_gc () =
  let opts = Caml.Gc.get () in
  info "Setting GC parameters";
  Caml.Gc.set {
    opts with
    window_size = 20;
    minor_heap_size = 1024 * 1024;
    major_heap_increment = 64 * 1024 * 1024;
    space_overhead = 200;
  }

let has_env var = match Sys.getenv var with
  | exception _ -> false
  | _ -> true

let () =
  let () =
    try if Sys.getenv "BAP_DEBUG" <> "0" then
        Printexc.record_backtrace true
    with Caml.Not_found -> () in
  if not (has_env "OCAMLRUNPARAM" || has_env "CAMLRUNPARAM")
  then setup_gc ()
  else info "GC parameters are overriden by a user";
  Sys.(set_signal sigint (Signal_handle exit));
  let argv = load_recipe () in
  Log.start ?logdir:(get_logdir argv)();
  at_exit (pp_print_flush err_formatter);
  let argv,passes = run_loader argv in
  try main (parse passes argv); exit 0 with
  | Unknown_arch arch ->
    error "Invalid arch `%s', should be one of %s." arch
      (String.concat ~sep:"," (List.map Arch.all ~f:Arch.to_string))
  | Unrecognized_source ->
    error "Invalid format of source type argument"
  | Bap_plugin_loader.Plugin_not_found name ->
    error "Can't find a plugin bundle `%s'" name
  | Failed_to_create_project err ->
    error "Failed to create a project: %a" nice_pp_error err
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
    @@ Exn.to_string exn
