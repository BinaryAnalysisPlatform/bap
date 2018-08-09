open Core_kernel.Std
open Bap_bundle.Std
open Bap_future.Std
open Bap_plugins.Std
open Format
open Cmdliner

module Event = Bap_event

module Create() = struct
  let bundle = main_bundle ()

  let main =
    let base = Filename.basename Sys.executable_name in
    try Filename.chop_extension base with _ -> base


  let manifest =
    try Bundle.manifest bundle
    with _exn -> Manifest.create main

  let name = Manifest.name manifest
  let version = Manifest.version manifest
  let doc = Manifest.desc manifest

  let has_verbose =
    Array.exists ~f:(function "--verbose" | _ -> false)

  let report_progress ?task ?note ?stage ?total () =
    let task = match task with
      | None -> name
      | Some subtask -> sprintf "%s/%s" name subtask in
    let task = if String.(name = main) then task
      else sprintf "%s/%s" main task in
    Event.Log.progress ?note ?stage ?total task

  let filter_args name =
    let prefix = "--" ^ name ^ "-" in
    let is_key = String.is_prefix ~prefix:"-" in
    Array.fold (Plugin.argv ()) ~init:([],`drop) ~f:(fun (args,act) arg ->
        let take arg = ("--" ^ arg) :: args in
        if arg = Sys.argv.(0) then (name::args,`drop)
        else match String.chop_prefix arg ~prefix, act with
          | None,`take when is_key arg -> args,`drop
          | None,`take -> arg::args,`drop
          | None,`drop -> args,`drop
          | Some arg,_ when String.mem arg '=' -> take arg,`drop
          | Some arg,_ -> take arg,`take) |>
    fst |> List.rev |> Array.of_list

  let argv =
    if name = main then Sys.argv
    else filter_args name

  let has_var v = match Sys.getenv ("BAP_" ^ String.uppercase v) with
    | exception Not_found -> false
    | "false" | "0" -> false
    | _ -> true

  let is_verbose = has_verbose argv ||
                   has_var ("DEBUG_"^name) ||
                   has_var ("DEBUG")

  open Event.Log

  let debug = (); match is_verbose with
    | false -> fun fmt -> ifprintf std_formatter fmt
    | true ->  fun fmt -> message Debug ~section:name fmt

  let info f = message Info ~section:name f
  let warning f = message Warning ~section:name f
  let error f = message Error ~section:name f

  let make_formatter (f : ('a, formatter, unit) format -> 'a) =
    let buf = Buffer.create 512 in
    let output = Buffer.add_subbytes buf in
    let flush () =
      f "%s" (Buffer.contents buf);
      Buffer.clear buf in
    let fmt = make_formatter output flush in
    let out = pp_get_formatter_out_functions fmt () in
    let out = {out with out_newline = flush} in
    pp_set_formatter_out_functions fmt out;
    fmt

  let debug_formatter = make_formatter debug
  let info_formatter = make_formatter info
  let warning_formatter = make_formatter warning
  let error_formatter = make_formatter error

  module Config = struct
    let plugin_name = name
    include Bap_config

    (* Discourage access to directories of other plugins *)
    let confdir =
      let (/) = Filename.concat in
      confdir / plugin_name

    type 'a param = 'a future
    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = formatter -> 'a -> unit

    let warn_if_deprecated msg name is_used = match msg with
      | Some msg when is_used ->
        eprintf "WARNING: %S option of plugin %S is deprecated. %s\n"
          name plugin_name msg
      | _ -> ()

    module Converter = struct
      type 'a t = {
        parser  : 'a parser;
        printer : 'a printer;
        default : 'a;
      }

      let t parser printer default : 'a t = {parser; printer; default}
      let to_arg conv : 'a Arg.converter = conv.parser, conv.printer
      let default conv = conv.default

      let deprecation_wrap ~converter ?deprecated ~name =
        {converter with
         parser=(fun s -> warn_if_deprecated deprecated name true;
                  converter.parser s)}

      let of_arg (conv:'a Arg.converter) (default:'a) : 'a t =
        let parser, printer = conv in
        t parser printer default
    end

    type 'a converter = 'a Converter.t
    let converter = Converter.t

    let deprecated = "Please refer to --help."

    let main = ref Term.(const ())

    let conf_file_options : (string, string) List.Assoc.t =
      let conf_filename =
        let (/) = Filename.concat in
        Bap_config.confdir / "config" in
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
      let name = "BAP_" ^ String.uppercase (plugin_name ^ "_" ^ name) in
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

    let param converter ?deprecated ?default ?as_flag ?(docv="VAL")
        ?(doc="Undocumented") ?(synonyms=[]) name =
      let converter = Converter.deprecation_wrap
          ~converter ?deprecated ~name in
      let doc = check_deprecated doc deprecated in
      let future, promise = Future.create () in
      let default =
        match default with
        | Some x -> x
        | None -> Converter.default converter in
      let converter = Converter.to_arg converter in
      let param = get_param ~converter ~default ~name in
      let warn x = match as_flag with
        | None -> ()
        | Some y ->
          warn_if_deprecated deprecated name (phys_equal x y) in
      let t =
        Arg.(value
             @@ opt ?vopt:as_flag converter param
             @@ info (name::synonyms) ~doc ~docv) in
      main := Term.(const (fun x () ->
          warn x;
          Promise.fulfill promise x) $ t $ (!main));
      future

    let param_all (converter:'a converter) ?deprecated ?(default=[]) ?as_flag
        ?(docv="VAL") ?(doc="Uncodumented") ?(synonyms=[]) name : 'a list param =
      let converter = Converter.deprecation_wrap
          ~converter ?deprecated ~name in
      let doc = check_deprecated doc deprecated in
      let future, promise = Future.create () in
      let converter = Converter.to_arg converter in
      let param = get_param ~converter:(Arg.list converter) ~default ~name in
      let t =
        Arg.(value
             @@ opt_all ?vopt:as_flag converter param
             @@ info (name::synonyms) ~doc ~docv) in
      let warn x = match x, as_flag with
        | [x], Some y ->
          warn_if_deprecated deprecated name (phys_equal x y)
        | _ -> () in
      main := Term.(const (fun x () ->
          warn x;
          Promise.fulfill promise x) $ t $ (!main));
      future

    let flag ?deprecated ?(docv="VAL") ?(doc="Undocumented")
        ?(synonyms=[]) name : bool param =
      let converter = Converter.deprecation_wrap
          ~converter:(Converter.of_arg Arg.bool false) ?deprecated ~name in
      let doc = check_deprecated doc deprecated in
      let future, promise = Future.create () in
      let converter = Converter.to_arg converter in
      let param = get_param ~converter ~default:false ~name in
      let t =
        Arg.(value @@ flag @@ info (name::synonyms) ~doc ~docv) in
      let warn = warn_if_deprecated deprecated name in
      main := Term.(const (fun x () ->
          warn (param || x);
          Promise.fulfill promise (param || x)) $ t $ (!main));
      future

    let term_info = ref (Term.info ~doc plugin_name)

    type manpage_block = [
      | `I of string * string
      | `Noblank
      | `P of string
      | `Pre of string
      | `S of string
    ]

    let extract_section name man =
      let _, sec, man =
        List.fold ~f:(fun (to_pick, picked, remained) -> function
            | `S x when x = name -> true, `S x :: picked, remained
            | `S x -> false, picked, `S x :: remained
            | x ->
              if to_pick then to_pick, x :: picked, remained
              else to_pick, picked, x :: remained)
          ~init:(false,[],[]) man in
      List.rev sec, List.rev man

    let insert_tags man =  match Manifest.tags manifest with
      | [] -> man
      | tags ->
        let default_see_also =
          let h = "www:bap.ece.cmu.edu" in
          [`S "SEE ALSO"; `P (sprintf "$(b,home:) $(i,%s)" h)] in
        let see_also, man = match extract_section "SEE ALSO" man with
          | [], man -> default_see_also, man
          | x -> x in
        let tags = [
          `P (String.concat tags ~sep:", " |>
              sprintf "$(b,tags:) %s") ] in
        man @ see_also @ tags

    let manpage man =
      let man = insert_tags man in
      let man = (man :> Manpage.block list) in
      term_info := Term.info ~doc ~man plugin_name

    let determined (p:'a param) : 'a future = p

    type reader = {get : 'a. 'a param -> 'a}
    let when_ready f : unit =
      let evaluate_cmdline_args () =
        match Term.eval ~argv (!main, !term_info) with
        | `Error _ -> exit 1
        | `Ok _ -> f {get = (fun p -> Future.peek_exn p)}
        | `Version | `Help -> exit 0 in
      Stream.watch Plugins.events (fun subscription -> function
          | `Errored (name,_) when plugin_name = name ->
            Stream.unsubscribe Plugins.events subscription
          | `Loaded p when Plugin.name p = plugin_name ->
            evaluate_cmdline_args ();
            Stream.unsubscribe Plugins.events subscription
          | _ -> () )

    let doc_enum = Arg.doc_alts_enum

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
      let default = [| |] in
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
