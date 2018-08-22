open Core_kernel.Std
open Regular.Std
open Bap_bundle.Std
open Bap_future.Std
open Bap_plugins.Std
open Format
open Cmdliner

module Event = Bap_event

type 'a parser = string -> [ `Ok of 'a | `Error of string ]
type 'a printer = formatter -> 'a -> unit

type 'a converter= {
  parser : 'a parser;
  printer : 'a printer;
  default : 'a;
  digest : 'a -> digest;
}

type 'a param = {
  ready : 'a signal;
  value : 'a stream;
  converter : 'a converter;
  mutable current : 'a;
  ident : string;
  space : string;
  descr : string;
}

module Param = struct
  open Future.Syntax
  let values p = p.value
  let digest p = p.converter.digest
  let current p = p.current
end

let main_grammar = ref Term.(const ())
let grammars = String.Table.create ()
let manpages = String.Table.create ()

let connect_param_signals param =
  Stream.observe param.value (fun x -> param.current <- x)

let newparam ~namespace ~doc converter ident =
  let value,ready = Stream.create () in
  let param = {
    value; ready; ident; descr=doc; space=namespace; converter;
    current=converter.default
  } in
  connect_param_signals param;
  param

let register param value term =
  main_grammar := Term.(const (fun x () ->
      Signal.send param.ready (value x)) $ term $ (!main_grammar));
  Hashtbl.update grammars param.space ~f:(function
      | None -> Term.(const ignore $ term)
      | Some t -> Term.(const (fun _ _ -> ()) $ term $ t))


let register_manpage info =
  Hashtbl.set manpages ~key:(Term.name info) ~data:info

let cycles,start = Stream.create ()

let term_info = ref (Term.info ~doc:"" ~man:[] "main")

let current_name () =
  main_bundle () |>
  Bundle.manifest |>
  Manifest.name

let is_host_program () =
  Bundle.is_host @@ main_bundle ()

type error = Failed


module type Namespace = sig
  val name : string
end


module Input = struct
  (* a crude and slow implementation, we will fix it later *)
  module IO = struct
    let read_stdin () =
      try
        Result.return @@
        Bigstring.of_string @@
        In_channel.(input_all stdin)
      with Sys_error msg -> Error (`Msg msg)

    let digest input =
      Data.Cache.digest ~namespace:"input" "%s" @@
      match input with
      | `Data data -> Bigstring.to_string data
      | `Path path -> Digest.file path
  end

  let parse_spec = function
    | "-" -> Result.(IO.read_stdin () >>| fun x -> `Data x)
    | path when Sys.file_exists path -> Ok (`Path path)
    | path ->
      let msg = "non existent or unreadable path - " ^ path in
      Error (`Msg msg)

  let print_spec ppf = function
    | `Path p -> fprintf ppf "%s" p
    | `Data _ -> fprintf ppf "-"

  let arg_converter = Arg.conv (parse_spec, print_spec)
  let default = `Path "a.out"

  let old_parser_interface parser x = match parser x with
    | Ok x -> `Ok x
    | Error (`Msg x) -> `Error x

  let param =
    let value,ready = Stream.create () in
    let converter = {
      parser = old_parser_interface parse_spec;
      printer = print_spec;
      default;
      digest = IO.digest;
    } in
    let doc = "a path to input or $(b,-) if it should be read
      from the standard input" in
    let param = {
      ready; value; converter;
      current = default;
      ident = "input";
      space = "bap";
      descr = doc;
    } in
    let term = Arg.(value & pos 0 arg_converter default &
                    info [] ~doc ~docv:"FILE") in
    register param ident term;
    connect_param_signals param;
    param
end


let run ?(options=[]) ?(argv=Sys.argv) ?input () =
  Option.iter input ~f:(Signal.send Input.param.ready);
  let extra_args =
    Array.of_list @@ List.map options ~f:(fun (key,value) ->
        sprintf "--%s=%s" key value) in
  let argv = if Array.length argv = 0
    then [| current_name () |] else argv in
  let argv = Array.append argv extra_args in
  match Term.eval ~argv (!main_grammar, !term_info) with
  | `Error _ -> Error Failed
  | `Ok () -> Ok (Signal.send start ())
  | `Version | `Help -> exit 0



module Converter(Current : Namespace) = struct
  let namespace = Current.name
  type 'a t = 'a converter


  let digestf s = Data.Cache.digest ~namespace s
  let digest (type t) (module T : Stringable with type t = t) v =
    digestf "%s" (T.to_string v)

  let create ?digest parser printer default : 'a t =
    let digest = match digest with
      | Some digest -> digest
      | None -> fun x ->
        let value = Format.asprintf "%a" printer x in
        digestf "%s" value in
    {parser; printer; default; digest}


  let to_arg conv : 'a Arg.converter = conv.parser, conv.printer
  let default conv = conv.default

  let deprecation_wrap ~converter ?deprecated ~name =
    let warn_if_deprecated () =
      match deprecated with
      | Some msg ->
        eprintf "WARNING: %S option of plugin %S is deprecated. %s\n"
          name namespace msg
      | None -> () in
    {converter with parser=(fun s -> warn_if_deprecated ();
                             converter.parser s)}

  let of_arg (parser,printer) default digest : 'a t =
    create parser printer ~digest default



  module Converters = struct
    let of_arg = of_arg


    let bool = of_arg Arg.bool false (digestf "%b")
    let char = of_arg Arg.char '\x00' (digestf "%c")
    let int = of_arg Arg.int 0 (digestf "%d")
    let nativeint =
      of_arg Arg.nativeint Nativeint.zero (digest (module Nativeint))
    let int32 = of_arg Arg.int32 Int32.zero (digestf "%ld")
    let int64 = of_arg Arg.int64 Int64.zero (digestf "%Ld")
    let float = of_arg Arg.float 0. (digestf "%h")
    let string = of_arg Arg.string "" (digestf "%s")
    let enum variants =
      if List.is_empty variants
      then invalid_argf "An empty list of variants was provided \
                         to a command line option in %s" namespace ();
      let _, default = List.hd_exn variants in
      of_arg (Arg.enum variants) default @@ fun choice ->
      List.find_map_exn variants ~f:(fun (_,value) ->
          if phys_equal choice value then Some (digestf "%s" namespace)
          else None)

    let unavailable = digestf "unavailable"
    let empty_folder = digestf "empty-folder"

    let digest_file path =
      if Sys.file_exists path && not (Sys.is_directory path)
      then try digestf "%s" (Digest.file path)
        with _ -> unavailable
      else unavailable

    let rec digest_folder path =
      if Sys.file_exists path && Sys.is_directory path
      then
        let content = try Sys.readdir path with _ -> [||] in
        Array.fold content ~init:empty_folder ~f:(fun digest entry ->
            let path = Filename.concat path entry in
            Data.Cache.Digest.concat digest @@ digest_path path)
      else unavailable
    and digest_path path =
      if Sys.file_exists path then
        if Sys.is_directory path
        then digest_folder path
        else digest_file path
      else unavailable

    let file = of_arg Arg.file "" digest_path
    let dir = of_arg Arg.dir "" digest_folder
    let non_dir_file = of_arg Arg.non_dir_file "" digest_file


    let digest_fold fold digest_of_arg args =
      let init = digestf "empty" in
      fold args ~init ~f:(fun digest arg ->
          Data.Cache.Digest.concat digest @@ digest_of_arg arg)

    let list ?sep x =
      of_arg (Arg.list ?sep (to_arg x)) []
        (digest_fold List.fold x.digest)

    let array ?sep x =
      let default = [| |] in
      of_arg (Arg.array ?sep (to_arg x)) default
        (digest_fold Array.fold x.digest)

    let pair ?sep x y =
      let default = (default x, default y) in
      let d1,d2 = (x.digest, y.digest) in
      of_arg (Arg.pair ?sep (to_arg x) (to_arg y)) default
      @@ fun (x,y) -> Data.Cache.Digest.concat (d1 x) (d2 y)

    let t2 = pair
    let t3 ?sep x y z =
      let a = to_arg x in
      let b = to_arg y in
      let c = to_arg z in
      let default = (default x, default y, default z) in
      of_arg (Arg.t3 ?sep a b c) default @@ fun (a,b,c) ->
      List.reduce_exn ~f:Data.Cache.Digest.concat [
        x.digest a;
        y.digest b;
        z.digest c;
      ]

    let t4 ?sep w x y z =
      let a = to_arg w in
      let b = to_arg x in
      let c = to_arg y in
      let d = to_arg z in
      let default = (default w, default x, default y, default z) in
      of_arg (Arg.t4 ?sep a b c d) default @@ fun (a,b,c,d) ->
      List.reduce_exn ~f:Data.Cache.Digest.concat [
        w.digest a;
        x.digest b;
        y.digest c;
        z.digest d;
      ]
    let some ?none x =
      of_arg (Arg.some ?none (to_arg x)) None @@ function
      | None -> digestf "None"
      | Some thing -> Data.Cache.Digest.concat
                        (digestf "Some")
                        (x.digest thing)


  end

end

module Create() = struct
  let bundle = main_bundle ()
  let manifest = Bundle.manifest bundle
  let name = Manifest.name manifest
  let version = Manifest.version manifest
  let doc = Manifest.desc manifest
  let is_host_program = is_host_program ()
  let argv = [||]

  let has_verbose =
    Array.exists ~f:(function "--verbose" | _ -> false)

  let report_progress ?task ?note ?stage ?total () =
    let task = match task with
      | None -> name
      | Some subtask -> sprintf "%s/%s" name subtask in
    let task = if is_host_program then task
      else sprintf "%s/%s" name task in
    Event.Log.progress ?note ?stage ?total task

  let env_var var = String.concat ~sep:"_" [
      "BAP"; String.uppercase name; String.uppercase var
    ]

  let prefix_var var = sprintf "%s-%s" name var

  let env_is_set var =
    match Sys.getenv var with
    | exception Not_found -> false
    | "false" | "0" -> false
    | _ -> true

  let is_verbose = has_verbose Sys.argv ||
                   env_is_set "BAP_DEBUG" ||
                   env_is_set (env_var "DEBUG")

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
    let namespace = name
    include Bap_config
    module Namespace = struct
      let name = namespace
    end

    type nonrec 'a param = 'a param
    let input = Input.param
    module Converter = Converter(Namespace)

    (* Discourage access to directories of other plugins *)
    let confdir =
      let (/) = Filename.concat in
      confdir / namespace

    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = formatter -> 'a -> unit
    type 'a converter = 'a Converter.t
    let converter = Converter.create

    let deprecated = "Please refer to --help."

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
      let name = "BAP_" ^ String.uppercase (namespace ^ "_" ^ name) in
      try
        Some (Sys.getenv name)
      with Not_found -> None

    let get_param ~converter ~default ~name =
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
      let name = prefix_var name in
      let converter = Converter.deprecation_wrap
          ~converter ?deprecated ~name in
      let doc = check_deprecated doc deprecated in
      let default =
        match default with
        | Some x -> x
        | None -> Converter.default converter in
      let result = newparam ~namespace ~doc converter name in
      let converter = Converter.to_arg converter in
      let param = get_param ~converter ~default ~name in
      let t =
        Arg.(value
             @@ opt ?vopt:as_flag converter param
             @@ info (name::synonyms) ~doc ~docv) in
      register result ident t;
      result

    let param_all (type a)
        (converter:a converter) ?deprecated ?(default=[]) ?as_flag
        ?(docv="VAL") ?(doc="Uncodumented") ?(synonyms=[]) name
      : a list param =
      let name = prefix_var name in
      let arg_converter = Converter.deprecation_wrap
          ~converter ?deprecated ~name in
      let lst_converter = Converter.Converters.list arg_converter in
      let doc = check_deprecated doc deprecated in
      let result = newparam ~namespace ~doc lst_converter name in
      let converter = Converter.to_arg arg_converter in
      let param : a list = get_param
          ~converter:(Converter.to_arg lst_converter)
          ~default ~name in
      let t : a list Term.t =
        Arg.(value
             @@ opt_all ?vopt:as_flag converter param
             @@ info (name::synonyms) ~doc ~docv) in
      register result ident t;
      result

    let digest_bool = Data.Cache.digest ~namespace:name "%b"

    let flag ?deprecated ?(docv="VAL") ?(doc="Undocumented")
        ?(synonyms=[]) name : bool param =
      let name = prefix_var name in
      let converter = Converter.deprecation_wrap
          ~converter:(Converter.of_arg Arg.bool false digest_bool)
          ?deprecated ~name in
      let doc = check_deprecated doc deprecated in
      let result = newparam ~namespace ~doc converter name in
      let converter = Converter.to_arg converter in
      let param = get_param ~converter ~default:false ~name in
      let t = Arg.(value @@ flag @@ info (name::synonyms) ~doc ~docv) in
      register result (fun x -> param || x) t;
      result




    let term_info = ref (Term.info ~doc namespace)

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

    let insert_tags man = match Manifest.tags manifest with
      | [] -> man
      | _ ->
        let default_see_also =
          let h = "www:bap.ece.cmu.edu" in
          [`S "SEE ALSO"; `P (sprintf "$(b,home:) $(i,%s)" h)] in
        let see_also, man = match extract_section "SEE ALSO" man with
          | [], man -> default_see_also, man
          | x -> x in
        let tags = [
          `P (Manifest.tags manifest |>
              String.concat ~sep:", " |>
              sprintf "$(b,tags:) %s") ] in
        man @ see_also @ tags

    let manpage man =
      let man = insert_tags man in
      let man = (man :> Manpage.block list) in
      register_manpage (Term.info ~doc ~man namespace)

    let determined (p:'a param) : 'a future = Stream.hd p.value

    type reader = {get : 'a. 'a param -> 'a}
    let when_ready f : unit =
      Stream.observe cycles @@ fun () ->
      f {get = (fun p -> p.current)}

    let doc_enum = Arg.doc_alts_enum

    include Converter.Converters
  end
end
