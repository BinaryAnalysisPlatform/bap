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
    with exn -> Manifest.create main

  let name = Manifest.name manifest
  let version = Manifest.version manifest
  let doc = Manifest.desc manifest

  let has_verbose =
    Array.exists ~f:(function "--verbose" | _ -> false)

  let filter_args name =
    let prefix = "--" ^ name ^ "-" in
    let is_key = String.is_prefix ~prefix:"-" in
    Array.fold Sys.argv ~init:([],`drop) ~f:(fun (args,act) arg ->
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

  module Config = struct
    let plugin_name = name
    include Bap_config

    (* Discourage access to directories of other plugins *)
    let confdir =
      let (/) = Filename.concat in
      confdir / plugin_name

    type 'a param = 'a future
    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = Format.formatter -> 'a -> unit
    type 'a converter = 'a parser * 'a printer * 'a

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

    let get_conv : 'a converter -> 'a Arg.converter =
      fun (parser, printer, _) -> parser, printer

    let default : 'a converter -> 'a =
      fun (_, _ , default) -> default

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

    let param converter ?default ?(docv="VAL") ?(doc="Undocumented") name =
      let future, promise = Future.create () in
      let default =
        let _, _, converter_default = converter in
        match default with
        | Some x -> x
        | None -> converter_default in
      let converter = get_conv converter in
      let param = get_param ~converter ~default ~name in
      let t =
        Arg.(value @@ opt converter param @@ info [name] ~doc ~docv) in
      main := Term.(const (fun x () ->
          Promise.fulfill promise x) $ t $ (!main));
      future

    let param_all (converter:'a converter) ?(default=[]) ?(docv="VAL")
        ?(doc="Uncodumented") name : 'a list param =
      let future, promise = Future.create () in
      let converter = get_conv converter in
      let param = get_param ~converter:(Arg.list converter) ~default ~name in
      let t =
        Arg.(value @@ opt_all converter param @@ info [name] ~doc ~docv) in
      main := Term.(const (fun x () ->
          Promise.fulfill promise x) $ t $ (!main));
      future

    let flag ?(docv="VAL") ?(doc="Undocumented") name : bool param =
      let future, promise = Future.create () in
      let param = get_param ~converter:Arg.bool ~default:false ~name in
      let t =
        Arg.(value @@ flag @@ info [name] ~doc ~docv) in
      main := Term.(const (fun x () ->
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

    let manpage (man:manpage_block list) : unit =
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

    let converter : 'a Arg.converter -> 'a -> 'a converter =
      fun (parser, printer) default -> parser, printer, default

    let unspecified_default = invalid_arg "default unspecified"

    let bool = converter Arg.bool false
    let char = converter Arg.char '\x00'
    let int = converter Arg.int 0
    let nativeint = converter Arg.nativeint Nativeint.zero
    let int32 = converter Arg.int32 Int32.zero
    let int64 = converter Arg.int64 Int64.zero
    let float = converter Arg.float 0.
    let string = converter Arg.string ""
    let enum x = converter (Arg.enum x) unspecified_default
    let file = converter Arg.file unspecified_default
    let dir = converter Arg.dir unspecified_default
    let non_dir_file = converter Arg.non_dir_file unspecified_default
    let list ?sep x = converter (Arg.list ?sep (get_conv x)) []
    let array ?sep x = converter (Arg.array ?sep (get_conv x)) (Array.empty ())
    let pair ?sep x y =
      converter (Arg.pair ?sep (get_conv x) (get_conv y)) (default x, default y)
    let t2 = pair
    let t3 ?sep x y z =
      let a = get_conv x in
      let b = get_conv y in
      let c = get_conv z in
      converter (Arg.t3 ?sep a b c) (default x, default y, default z)
    let t4 ?sep w x y z =
      let a = get_conv w in
      let b = get_conv x in
      let c = get_conv y in
      let d = get_conv z in
      converter (Arg.t4 ?sep a b c d) (default w, default x, default y, default z)
    let some ?none x = converter (Arg.some ?none (get_conv x)) None

  end

end
