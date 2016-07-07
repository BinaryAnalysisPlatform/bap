open Core_kernel.Std
open Bap_bundle.Std
open Bap_plugins.Std
open Bap_future.Std
open Cmdliner
open Format

module Config' = struct
  type 'a param = 'a future
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
    doc : string;
    is_default : bool
  }

  let t ~is_default ?(plugin_grammar=true) ~doc name = {
    name = name;
    main = ref Term.(const ());
    plugin_grammar;
    man = ref None;
    doc;
    is_default
  }

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

end = struct

  let plugin_global = ref Term.(const ())

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

  let commands : (Command.t, unit -> unit) List.Assoc.t ref = ref []

  let when_ready_frontend cmd f =
    commands := List.Assoc.add !commands cmd f

  let evalable (cmd:Command.t) : (Command.t Term.t * Term.info) =
    let open Command in
    let grammar = if cmd.plugin_grammar
      then Term.(combine $ !(cmd.main) $ !plugin_global)
      else !(cmd.main) in
    let grammar = Term.(const (fun () -> cmd) $ grammar) in
    let info =
      Term.(info ?man:!(cmd.man) ~doc:cmd.doc cmd.name) in
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
    Term.eval_choice default_cmd remaining_cmds

  let evaluate_terms () =
    let open Command in
    let result = match !commands with
      | [] -> assert false
      | [x,_] -> Term.eval (evalable x)
      | _ as xs -> eval_choice (List.map ~f:fst xs) in
    match result with
    | `Error _ -> exit 1
    | `Ok cmd ->
      List.Assoc.find_exn !commands cmd ();
      if cmd.plugin_grammar
      then Promise.fulfill eval_plugins_promise ()
      else ()
    | `Version | `Help -> exit 0

  let () =
    Future.upon Plugins.loaded evaluate_terms
end

module Config = struct
  let bundle = main_bundle ()

  let executable_name () =
    let base = Filename.basename Sys.executable_name in
    try Filename.chop_extension base with _ -> base

  let manifest =
    try Bundle.manifest bundle
    with exn -> Manifest.create (executable_name ())

  let doc () = Manifest.desc manifest

  let is_plugin () =
    Manifest.name manifest <> executable_name ()

  let must_use_frontend () =
    invalid_arg "Must use the Frontend interface for frontends"

  let plugin_name () =
    if is_plugin ()
    then Manifest.name manifest
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
    future

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
    future

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
    future

  let flag
      ?deprecated ?docv ?doc ?synonyms name =
    if is_plugin () then
      flag' (plugin_grammar ()) (Future.create ())
        ?deprecated ?docv ?doc ?synonyms name
    else must_use_frontend ()

  let pos' main (future, promise) converter ?default ?(docv="VAL")
      ?(doc="Undocumented") n =
    let default =
      match default with
      | Some x -> x
      | None -> Converter.default converter in
    let converter = Converter.to_arg converter in
    let t =
      Arg.(value
           @@ pos n converter default
           @@ info [] ~doc ~docv) in
    main := Term.(const (fun x () ->
        Promise.fulfill promise x) $ t $ (!main));
    future

  let pos_all' main (future, promise) (converter:'a converter)
      ?(default=[]) ?(docv="VAL") ?(doc="Undocumented") n
    : 'a list param =
    let converter = Converter.to_arg converter in
    let t =
      Arg.(value
           @@ pos_all converter default
           @@ info [] ~doc ~docv) in
    main := Term.(const (fun x () ->
        Promise.fulfill promise x) $ t $ (!main));
    future

  let term_info =
    ref (Term.info ~doc:(doc ()) (if is_plugin () then plugin_name ()
                                  else executable_name ()))

  let manpage (man:manpage_block list) : unit =
    if is_plugin () then
      term_info := Term.info ~doc:(doc ()) ~man (plugin_name ())
    else must_use_frontend ()

  let determined (p:'a param) : 'a future = p

  type reader = {get : 'a. 'a param -> 'a}
  let when_ready f : unit =
    if is_plugin () then
      let open CmdlineGrammar in
      let grammar = plugin_help (plugin_name ()) !term_info
          !(plugin_grammar ()) in
      add_plugin grammar;
      when_ready_plugin (fun () ->
          f {get = (fun p -> Future.peek_exn p)})
    else must_use_frontend ()

  let doc_enum = Arg.doc_alts_enum

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
        Command.t ~is_default:false ?plugin_grammar ~doc name

    let default_command = Command.t ~is_default:true
        ~plugin_grammar:true ~doc:(doc ()) (executable_name ())

    let manpage cmd man =
      if is_plugin () then cannot_use_frontend ()
      else Command.set_man cmd man

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
            f {get = (fun p -> Future.peek_exn p)})

  end
end
