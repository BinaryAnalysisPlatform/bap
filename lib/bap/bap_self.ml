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
    type 'a converter = 'a parser * 'a printer

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

    let get_param ~(converter:'a converter) ~default ~name =
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

    let param converter ~default ?(docv="VAL") ?(doc="Undocumented") ~name =
      let future, promise = Future.create () in
      let param = get_param ~converter ~default ~name in
      let t =
        Arg.(value @@ opt converter param @@ info [name] ~doc ~docv) in
      main := Term.(const (fun x () ->
          Promise.fulfill promise x) $ t $ (!main));
      future

    let flag ?(docv="VAL") ?(doc="Undocumented") ~name : bool param =
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
        match Term.eval (!main, !term_info) with
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


    let bool = Arg.bool
    let char = Arg.char
    let int = Arg.int
    let nativeint = Arg.nativeint
    let int32 = Arg.int32
    let int64 = Arg.int64
    let float = Arg.float
    let string = Arg.string
    let enum = Arg.enum
    let file = Arg.file
    let dir = Arg.dir
    let non_dir_file = Arg.non_dir_file
    let list = Arg.list
    let array = Arg.array
    let pair = Arg.pair
    let t2 = Arg.t2
    let t3 = Arg.t3
    let t4 = Arg.t4

  end

end
