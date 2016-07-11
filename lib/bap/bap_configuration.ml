open Core_kernel.Std
open Bap_bundle.Std
open Bap_plugins.Std
open Bap_future.Std
open Cmdliner
open Format

module PluginLoader = struct

  let load =
    let doc =
      "Dynamically loads file $(docv).plugin. A plugin must be
     compiled with $(b,bapbuild) tool using $(b,bapbuild PATH.plugin)
     command." in
    Arg.(value @@ opt_all string [] @@ info ["l"] ~docv:"PATH" ~doc)

  let load_path =
    let doc =
      "Add $(docv) to a set of search paths. Plugins found in the
    search paths will be loaded automatically." in
    Arg.(value @@ opt_all string []
         @@ info ["L"; "load-path"] ~docv:"PATH" ~doc)

  let list_plugins =
    let doc = "List available plugins" in
    Arg.(value @@ flag @@ info ["list-plugins"] ~doc)

  let disable_plugin =
    let doc = "Don't load $(docv) automatically" in
    Arg.(value @@ opt_all string []
         @@ info ["disable-plugin"] ~docv:"PLUGIN" ~doc)

  let no_auto_load =
    let doc = "Disable auto loading of plugins" in
    Arg.(value @@ flag @@ info ["disable-autoload"] ~doc)

  let global_term =
    let combine _ _ _ _ _ = () in
    Term.(const combine
          $ load
          $ load_path
          $ list_plugins
          $ disable_plugin
          $ no_auto_load)

  exception Plugin_not_found of string

  let open_plugin_exn name =
    let name = name ^ ".plugin" in
    if Sys.file_exists name then Plugin.of_path name
    else raise (Plugin_not_found name)

  let open_plugin ~verbose name =
    try
      let p = open_plugin_exn name in
      if verbose then
        eprintf "Loader: opened plugin %s\n" @@ Plugin.name p;
      Some p
    with
    | Plugin_not_found name ->
      eprintf "Loader: can't find plugin %s\n" name;
      None
    | exn when verbose ->
      eprintf "Loader: can't open plugin %s: %a\n"
        name Exn.pp exn;
      None
    | exn ->
      eprintf "Loader: can't open plugin %s\n" name; None

  (* We will not ignore errors on plugins loaded explicitly
      by a user with `-l` option. *)
  let load_plugin p = ok_exn (Plugin.load p)


  let autoload_plugins ~library ~verbose ~exclude =
    Plugins.run ~library ~exclude ()
      ~don't_setup_handlers:true
  (* we don't want to fail the whole platform if some
     plugin has failed, we will just emit an error message.  *)


  let get_opt ~default argv opt  =
    Option.value (fst (Term.eval_peek_opts ~argv opt)) ~default

  let excluded argv =
    let module Pat = String.Search_pattern in
    let pat = Pat.create "--no-" in
    Array.fold ~init:[] argv ~f:(fun plugins opt ->
        match Pat.index pat opt with
        | Some 0 -> Pat.replace_first pat opt "" :: plugins
        | _ -> plugins)

  let print_plugins_and_exit excluded plugins =
    List.iter plugins ~f:(fun p ->
        let status = if List.mem excluded (Plugin.name p)
          then "[-]" else "[+]" in
        printf "  %s %-16s %s@." status (Plugin.name p) (Plugin.desc p));
    exit 0

  let verbose =
    let doc = "Print verbose output" in
    Arg.(value @@ flag @@ info ["verbose"] ~doc)

  type action = Keep | Drop

  let filter_option options prefix =
    let is_long = String.is_prefix ~prefix:"--" in
    let is_our = String.is_prefix ~prefix in
    let is_key = String.is_prefix ~prefix:"-" in
    List.fold options ~init:([],Keep) ~f:(fun (opts,act) opt ->
        match act with
        | _ when prefix = opt -> (opts,Keep)
        | _ when is_our opt && is_long opt ->
          if String.mem opt '=' then (opts,Keep) else (opts,Drop)
        | _ when is_our opt -> (opts,Keep)
        | Drop when is_key opt && not(is_our opt) -> (opt::opts,Keep)
        | Keep -> (opt::opts,Keep)
        | Drop -> (opts,Keep)) |>
    fst |> List.rev

  let filter_options ~known_plugins ~argv =
    let opts = Array.to_list argv in
    filter_option opts "--no" |> Array.of_list

  let run_and_get_argv argv : string array =
    let verbose = get_opt argv verbose ~default:false in
    let library = get_opt argv load_path ~default:[] in
    let plugins = get_opt argv load ~default:[] in
    let exclude = get_opt argv disable_plugin ~default:[] in
    let exclude = exclude @ excluded argv in
    let list = get_opt argv list_plugins ~default:false in
    let plugins = List.filter_map plugins ~f:(open_plugin ~verbose) in
    let known_plugins = Plugins.list ~library () @ plugins in
    if list then print_plugins_and_exit exclude known_plugins;
    List.iter plugins ~f:load_plugin;
    let noautoload = get_opt argv no_auto_load ~default:false in
    if not noautoload then autoload_plugins ~library ~verbose ~exclude;
    let known_plugins = List.map known_plugins ~f:Plugin.name in
    filter_options ~known_plugins ~argv

end

module Config' = struct
  type 'a param = string * 'a future
  type 'a parser = string -> [ `Ok of 'a | `Error of string ]
  type 'a printer = Format.formatter -> 'a -> unit
  module Converter = struct
    type 'a t = {
      parser : 'a parser;
      printer : 'a printer;
      default : 'a;
    }

    let t parser printer default : 'a t = {parser; printer; default}
    let to_arg conv : 'a Arg.converter = conv.parser, conv.printer
    let default conv = conv.default

    let deprecation_wrap ~converter ?deprecated ~name ~is_plugin ~plugin_name =
      let warn_if_deprecated () =
        match deprecated with
        | Some msg ->
          if is_plugin () then
            eprintf "WARNING: %S option of plugin %S is deprecated. %s\n"
              name (plugin_name ()) msg
          else eprintf "WARNING: %S option is deprecated. %s\n"
              name msg
        | None -> () in
      {converter with parser=(fun s -> warn_if_deprecated ();
                               converter.parser s)}

    let of_arg (conv:'a Arg.converter) (default:'a) : 'a t =
      let parser, printer = conv in
      t parser printer default
  end
  type 'a converter = 'a Converter.t
  let converter = Converter.t

  type manpage_block = [
    | `I of string * string
    | `Noblank
    | `P of string
    | `Pre of string
    | `S of string
  ]

  module Converters = struct
    let of_arg = Converter.of_arg

    let bool = of_arg Arg.bool false
    let char = of_arg Arg.char '\x00'
    let int = of_arg Arg.int 0
    let nativeint = of_arg Arg.nativeint Nativeint.zero
    let int32 = of_arg Arg.int32 Int32.zero
    let int64 = of_arg Arg.int64 Int64.zero
    let float = of_arg Arg.float 0.
    let string = of_arg Arg.string ""
    let enum x =
      let _, default = List.hd_exn x in
      of_arg (Arg.enum x) default
    let file = of_arg Arg.file ""
    let dir = of_arg Arg.dir ""
    let non_dir_file = of_arg Arg.non_dir_file ""
    let list ?sep x = of_arg (Arg.list ?sep (Converter.to_arg x)) []
    let array ?sep x =
      let default = Array.empty () in
      of_arg (Arg.array ?sep (Converter.to_arg x)) default
    let pair ?sep x y =
      let default = Converter.(default x, default y) in
      of_arg Converter.(Arg.pair ?sep (to_arg x) (to_arg y)) default
    let t2 = pair
    let t3 ?sep x y z =
      let a = Converter.to_arg x in
      let b = Converter.to_arg y in
      let c = Converter.to_arg z in
      let default = Converter.(default x, default y, default z) in
      of_arg (Arg.t3 ?sep a b c) default
    let t4 ?sep w x y z =
      let a = Converter.to_arg w in
      let b = Converter.to_arg x in
      let c = Converter.to_arg y in
      let d = Converter.to_arg z in
      let default = Converter.(default w, default x, default y,
                               default z) in
      of_arg (Arg.t4 ?sep a b c d) default
    let some ?none x = of_arg (Arg.some ?none (Converter.to_arg x)) None
  end
end

module Command = struct

  type t = {
    name : string;
    main : unit Term.t ref;
    plugin_grammar : bool;
    man : Config'.manpage_block list option ref;
    doc : string ref;
    is_default : bool;
    future : unit future;
    promise : unit promise;
  }

  let t ~is_default ?(plugin_grammar=true) ~doc name =
    let future, promise = Future.create () in
    {
      name = name;
      main = ref Term.(const ());
      plugin_grammar;
      man = ref None;
      doc;
      is_default;
      future;
      promise;
    }

  let equal a b =
    a.name = b.name

  let set_man cmd man : unit =
    cmd.man := Some man

end

(** [CmdlineGrammar] is a module purely internal to Bap_configuration
    and SHOULD NOT be exposed to outside world directly. It allows for
    making the single grammar possible. *)
module CmdlineGrammar : sig
  (** [plugin_help name info g] takes a grammar [g] and returns a new
      grammar that accepts help for [name] and creates a manpage using
      [info]. *)
  val plugin_help : string -> Term.info -> unit Term.t -> unit Term.t

  (** [add_plugin grammar] adds a grammar to the global grammar which can
      be used by the front end. *)
  val add_plugin : unit Term.t -> unit

  (** [when_ready_plugin f] evaluates [f ()] when the whole grammar is known
      and all arguments have been parsed. Should be called by plugins
      only. *)
  val when_ready_plugin : (unit -> unit) -> unit

  (** [when_ready_frontend cmd f] adds the command [cmd] and calls [f]
      when ready. Should be called by frontends only. *)
  val when_ready_frontend : Command.t -> (unit -> unit) -> unit

  (** Does any pre-processing (if necessary) on the argv, along with
      recognizing options like [--no-PLUGIN] etc. and then starts off
      the loading of plugins, and command line processing. *)
  val start : unit -> unit
end = struct

  let plugin_global = ref PluginLoader.global_term
  let argv = ref Sys.argv

  let start () =
    argv := PluginLoader.run_and_get_argv Sys.argv;
    Plugins.run ()

  let eval_plugins_complete, eval_plugins_promise = Future.create ()

  let when_ready_plugin f =
    Future.upon eval_plugins_complete f

  let plugin_help plugin_name terminfo grammar : unit Term.t =
    let formats = List.map ~f:(fun x -> x,x) ["pager"; "plain"; "groff"] in
    let name = plugin_name ^ "-help" in
    let doc = "Show help for " ^
              plugin_name ^
              " plugin in format $(docv), (pager, plain or groff)" in
    let help = Arg.(value @@
                    opt ~vopt:(Some "pager") (some (enum formats)) None @@
                    info [name] ~doc ~docv:"FMT") in
    Term.(const (fun h () ->
        match h with
        | None -> ()
        | Some v ->
          match eval ~argv:[|plugin_name;
                             "--help";
                             v
                           |] (grammar, terminfo) with
          | `Error _ -> exit 1
          | `Ok _ -> assert false
          | `Version -> assert false
          | `Help -> exit 0
      ) $ help $ grammar)

  let combine = Term.const (fun () () -> ())

  let add_plugin g =
    plugin_global := Term.(combine $ g $ (!plugin_global))

  let commands : Command.t list ref = ref []

  let when_ready_frontend cmd f =
    let open Command in
    Future.upon cmd.future f;
    if List.mem ~equal !commands cmd
    then invalid_argf
        "There must be only one \"when_ready\" for command %S"
        cmd.name ()
    else commands := cmd :: !commands

  let evalable (cmd:Command.t) : (Command.t Term.t * Term.info) =
    let open Command in
    let grammar = if cmd.plugin_grammar
      then Term.(combine $ !(cmd.main) $ !plugin_global)
      else !(cmd.main) in
    let grammar = Term.(const (fun () -> cmd) $ grammar) in
    let info =
      Term.(info ?man:!(cmd.man) ~doc:!(cmd.doc) cmd.name) in
    grammar, info

  let eval_choice (cmds:Command.t list) =
    let open Command in
    let default_cmd =
      match List.filter ~f:(fun cmd -> cmd.is_default) cmds with
      | [x] -> evalable x
      | _ -> assert false in
    let remaining_cmds =
      match List.filter ~f:(fun cmd -> not cmd.is_default) cmds with
      | [] -> assert false
      | _ as xs -> List.map ~f:evalable xs in
    Term.eval_choice ~argv:!argv default_cmd remaining_cmds

  let evaluate_terms () : unit =
    let open Command in
    let result = match !commands with
      | [] -> assert false
      | [x] -> Term.eval ~argv:!argv (evalable x)
      | _ as xs -> eval_choice xs in
    match result with
    | `Error _ -> exit 1
    | `Ok cmd ->
      let command_callback () =
        Promise.fulfill cmd.Command.promise () in
      let plugin_callback = fun () ->
        if cmd.plugin_grammar
        then Promise.fulfill eval_plugins_promise ()
        else () in
      plugin_callback(); command_callback ()
    | `Version | `Help -> exit 0

  let () =
    Bap_log.start ();
    Future.upon Plugins.loaded evaluate_terms
end

module Config = struct
  let bundle () = main_bundle ()

  let executable_name () =
    let base = Filename.basename Sys.executable_name in
    try Filename.chop_extension base with _ -> base

  let manifest () =
    try Bundle.manifest (bundle ())
    with exn -> Manifest.create (executable_name ())

  let doc () = Manifest.desc (manifest ())

  let is_plugin () =
    Manifest.name (manifest ()) <> executable_name ()

  let must_use_frontend () =
    invalid_arg "Must use the Frontend interface for frontends"

  let plugin_name () =
    if is_plugin ()
    then Manifest.name (manifest ())
    else must_use_frontend ()

  include Config'
  include Bap_config

  let deprecated =
    if is_plugin () then "Please refer to --" ^ plugin_name () ^ "-help"
    else "Please refer to --help."

  let plugin_grammars = String.Table.create ()

  let plugin_grammar () =
    String.Table.find_or_add plugin_grammars (plugin_name ())
      ~default:(fun () -> ref Term.(const ()))

  let conf_file_options : (string, string) List.Assoc.t =
    let conf_filename =
      let (/) = Filename.concat in
      let confdir =
        if is_plugin () then
          let (/) = Filename.concat in
          confdir / plugin_name ()
        else confdir in
      confdir / "config" in
    let string_splitter str =
      let str = String.strip str in
      match String.split str ~on:'=' with
      | k :: _ when String.prefix k 1 = "#" -> None
      | [""] | [] -> None
      | [k] -> invalid_argf
                 "Maybe comment out %S using # in config file?" k ()
      | k :: vs -> Some (String.strip k,
                         String.strip (String.concat ~sep:"=" vs)) in
    let split_filter = List.filter_map ~f:string_splitter in
    try
      In_channel.with_file
        conf_filename ~f:(fun ch -> In_channel.input_lines ch
                                    |> split_filter)
    with Sys_error _ -> []

  let get_from_conf_file name =
    List.Assoc.find conf_file_options ~equal:String.Caseless.equal name

  let get_from_env name =
    let name = if is_plugin () then plugin_name () ^ "_" ^ name else name in
    let name = String.uppercase (executable_name () ^ "_" ^ name) in
    try
      Some (Sys.getenv name)
    with Not_found -> None

  let get_param ~(converter) ~default ~name =
    let value = default in
    let str = get_from_conf_file name in
    let str = match get_from_env name with
      | Some _ as v -> v
      | None -> str in
    let parse str =
      let parse, _ = converter in
      match parse str with
      | `Error err ->
        invalid_argf "Could not parse %S for parameter %S: %s"
          str name err ()
      | `Ok v -> v in
    let value = match str with
      | Some v -> parse v
      | None -> value in
    value

  let check_deprecated doc deprecated =
    match deprecated with
    | Some _ -> "DEPRECATED. " ^ doc
    | None -> doc

  let complete_param name =
    if is_plugin () then plugin_name () ^ "-" ^ name
    else name

  let preprocess ?deprecated name converter doc =
    let name = complete_param name in
    let converter = Converter.deprecation_wrap
        ~converter ?deprecated ~name ~is_plugin ~plugin_name in
    let doc = check_deprecated doc deprecated in
    name, converter, doc

  let param' main (future, promise) converter ?deprecated ?default
      ?as_flag ?(docv="VAL") ?(doc="Undocumented") ?(synonyms=[])
      name =
    let name, converter, doc = preprocess ?deprecated
        name converter doc in
    let default =
      match default with
      | Some x -> x
      | None -> Converter.default converter in
    let converter = Converter.to_arg converter in
    let param = get_param ~converter ~default ~name in
    let t =
      Arg.(value
           @@ opt ?vopt:as_flag converter param
           @@ info (name::synonyms) ~doc ~docv) in
    main := Term.(const (fun x () ->
        Promise.fulfill promise x) $ t $ (!main));
    name, future

  let param
      converter ?deprecated ?default ?as_flag ?docv
      ?doc ?synonyms name =
    if is_plugin () then
      param' (plugin_grammar ()) (Future.create ())
        converter ?deprecated ?default ?as_flag ?docv
        ?doc ?synonyms name
    else must_use_frontend ()

  let param_all' main (future, promise) (converter:'a converter)
      ?deprecated ?(default=[]) ?as_flag ?(docv="VAL")
      ?(doc="Uncodumented") ?(synonyms=[]) name : 'a list param =
    let name, converter, doc = preprocess ?deprecated
        name converter doc in
    let converter = Converter.to_arg converter in
    let param = get_param ~converter:(Arg.list converter) ~default ~name in
    let t =
      Arg.(value
           @@ opt_all ?vopt:as_flag converter param
           @@ info (name::synonyms) ~doc ~docv) in
    main := Term.(const (fun x () ->
        Promise.fulfill promise x) $ t $ (!main));
    name, future

  let param_all
      converter ?deprecated ?default ?as_flag ?docv ?doc ?synonyms
      name =
    if is_plugin () then
      param_all' (plugin_grammar ()) (Future.create ())
        converter ?deprecated ?default ?as_flag ?docv ?doc ?synonyms
        name
    else must_use_frontend ()

  let flag' main (future, promise) ?deprecated ?(docv="VAL")
      ?(doc="Undocumented") ?(synonyms=[]) name : bool param =
    let name, converter, doc = preprocess ?deprecated
        name (Converter.of_arg Arg.bool false) doc in
    let converter = Converter.to_arg converter in
    let param = get_param ~converter ~default:false ~name in
    let t =
      Arg.(value @@ flag @@ info (name::synonyms) ~doc ~docv) in
    main := Term.(const (fun x () ->
        Promise.fulfill promise (param || x)) $ t $ (!main));
    name, future

  let flag
      ?deprecated ?docv ?doc ?synonyms name =
    if is_plugin () then
      flag' (plugin_grammar ()) (Future.create ())
        ?deprecated ?docv ?doc ?synonyms name
    else must_use_frontend ()

  let const x : 'a param =
    "", Future.return x

  let pos' main (future, promise) converter ?default ?(docv="VAL")
      ?(doc="Undocumented") n =
    let converter = Converter.to_arg converter in
    let t =
      match default with
      | Some default ->
        Arg.(value
             @@ pos n converter default
             @@ info [] ~doc ~docv)
      | None ->
        Arg.(required
             @@ pos n (some converter) None
             @@ info [] ~doc ~docv) in
    main := Term.(const (fun x () ->
        Promise.fulfill promise x) $ t $ (!main));
    docv ^ " argument", future

  let pos_all' main (future, promise) (converter:'a converter)
      ?default ?(docv="VAL") ?(doc="Undocumented") n
    : 'a list param =
    let converter = Converter.to_arg converter in
    let t =
      match default with
      | Some default ->
        Arg.(value
             @@ pos_all converter default
             @@ info [] ~doc ~docv)
      | None ->
        Arg.(non_empty
             @@ pos_all converter []
             @@ info [] ~doc ~docv) in
    main := Term.(const (fun x () ->
        Promise.fulfill promise x) $ t $ (!main));
    docv ^ " argument", future

  let doc_enum = Arg.doc_alts_enum

  let post_check ~f p =
    fst p, Future.(
        snd p >>= (fun x ->
            let values = f () in
            let allowed = List.map ~f:snd values in
            if List.mem allowed x then
              return x
            else
              invalid_argf "Unrecognized value for %s. Must be %s"
                (fst p) (doc_enum values) ()))

  let post_check_all ~f ps =
    fst ps, Future.(
        snd ps >>= (fun xs ->
            let values = f () in
            let allowed = List.map ~f:snd values in
            if List.for_all xs ~f:(fun x -> List.mem allowed x) then
              return xs
            else
              invalid_argf "Unrecognized value for %s. Must be %s"
                (fst ps) (doc_enum values) ()))

  let term_info =
    ref (Term.info ~doc:(doc ()) (if is_plugin () then plugin_name ()
                                  else executable_name ()))

  let manpage (man:manpage_block list) : unit =
    if is_plugin () then
      term_info := Term.info ~doc:(doc ()) ~man (plugin_name ())
    else must_use_frontend ()

  let determined (p:'a param) : 'a future = snd p

  type reader = {get : 'a. 'a param -> 'a}
  let when_ready f : unit =
    if is_plugin () then
      let open CmdlineGrammar in
      let grammar = plugin_help (plugin_name ()) !term_info
          !(plugin_grammar ()) in
      add_plugin grammar;
      when_ready_plugin (fun () ->
          f {get = (fun p -> Future.peek_exn (snd p))})
    else must_use_frontend ()

  include Config'.Converters

end

module Frontend = struct
  module Config = struct
    include Config

    let cannot_use_frontend () =
      invalid_argf "Cannot use Frontend interface for plugin %S"
        (plugin_name ()) ()

    type command = Command.t
    let command ?plugin_grammar ~doc name =
      if is_plugin () then cannot_use_frontend () else
        Command.t ~is_default:false ?plugin_grammar
          ~doc:(ref doc) name

    let default_command = Command.t ~is_default:true
        ~plugin_grammar:true ~doc:(ref "description not provided")
        (executable_name ())

    let manpage cmd man =
      if is_plugin () then cannot_use_frontend ()
      else Command.set_man cmd man

    let descr doc =
      default_command.Command.doc := doc

    let param ?(commands=[default_command])
        converter ?deprecated ?default ?as_flag ?docv
        ?doc ?synonyms name =
      if is_plugin () then cannot_use_frontend () else
        let result =
          let future, promise = Future.create () in
          commands |>
          List.map ~f:(fun c ->
              param' c.Command.main (future, promise)
                converter ?deprecated ?default ?as_flag ?docv
                ?doc ?synonyms name) |>
          List.hd in
        match result with
        | Some x -> x
        | None -> invalid_arg "Cannot call param without any commands"

    let param_all ?(commands=[default_command])
        converter ?deprecated ?default ?as_flag ?docv ?doc ?synonyms
        name =
      if is_plugin () then cannot_use_frontend () else
        let result =
          let future, promise = Future.create () in
          commands |>
          List.map ~f:(fun c ->
              param_all' c.Command.main (future, promise)
                converter ?deprecated ?default ?as_flag ?docv ?doc ?synonyms
                name) |>
          List.hd in
        match result with
        | Some x -> x
        | None -> invalid_arg "Cannot call param_all without any commands"

    let flag ?(commands=[default_command])
        ?deprecated ?docv ?doc ?synonyms name =
      if is_plugin () then cannot_use_frontend () else
        let result =
          let future, promise = Future.create () in
          commands |>
          List.map ~f:(fun c ->
              flag' c.Command.main (future, promise)
                ?deprecated ?docv ?doc ?synonyms name) |>
          List.hd in
        match result with
        | Some x -> x
        | None -> invalid_arg "Cannot call flag without any commands"

    let pos ?(commands=[default_command])
        converter ?default ?docv ?doc n =
      if is_plugin () then cannot_use_frontend () else
        let result =
          let future, promise = Future.create () in
          commands |>
          List.map ~f:(fun c ->
              pos' c.Command.main (future, promise)
                converter ?default ?docv ?doc n) |>
          List.hd in
        match result with
        | Some x -> x
        | None -> invalid_arg "Cannot call pos without and commands"

    let pos_all ?(commands=[default_command])
        converter ?default ?docv ?doc () =
      if is_plugin () then cannot_use_frontend () else
        let result =
          let future, promise = Future.create () in
          commands |>
          List.map ~f:(fun c ->
              pos_all' c.Command.main (future, promise)
                converter ?default ?docv ?doc ()) |>
          List.hd in
        match result with
        | Some x -> x
        | None -> invalid_arg "Cannot call pos without and commands"

    let when_ready (cmd:command) f : unit =
      if is_plugin () then cannot_use_frontend () else
        CmdlineGrammar.when_ready_frontend cmd (fun () ->
            f {get = (fun p -> Future.peek_exn (snd p))})

  end

  let start () =
    CmdlineGrammar.start ()

end
