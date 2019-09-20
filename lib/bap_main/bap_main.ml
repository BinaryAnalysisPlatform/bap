(* Base kills exn, so we have to do it before opening *)
type error = exn = ..

open Base
open Stdio
open Bap_future.Std
open Bap_plugins.Std

module Format = Caml.Format

let fail fmt = Printf.ksprintf failwith fmt
let sprintf = Printf.sprintf


(** A simple config file parser.
    A config file is a list of <key> = <value> lines,
    with '#' being the comment symbol
    (everything after the comment character is ignored)
    empty lines are also ignored.

*)
module ConfigFile : sig
  (** [read filename] reads configuration from
      a file with the given [filename].

      Fails, if the file doesn't exist or is malformed.
  *)
  val read_or_fail : string -> (string * string) list
end = struct
  let is_empty =
    String.for_all ~f:Char.is_whitespace

  let drop_comment str =
    match String.lsplit2 str ~on:'#' with
    | None -> str
    | Some (str,_) -> str

  exception Malformed_line of int * string

  let parse_config_entry line str =
    let str = drop_comment str in
    if is_empty str then None
    else match String.lsplit2 ~on:'=' str with
      | None -> raise (Malformed_line (line,str))
      | Some (k,v) ->
        Some (String.strip k, String.strip v)

  let read_or_fail filename =
    try In_channel.with_file filename ~f:(fun ch ->
        In_channel.input_lines ch |>
        List.filter_mapi ~f:parse_config_entry)
    with
    | Sys_error msg ->
      fail "can't access config file %S: %s\n%!" filename msg
    | Malformed_line (linenum,str) ->
      fail "File %S, line %d, characters %d-%d\n\
            Syntax error: expects <parameter> = <value>"
        filename (linenum+1) 1 (String.length str + 1)
end

module Type = struct
  open Cmdliner
  type 'a parser = string -> [ `Ok of 'a | `Error of string ]
  type 'a printer = Format.formatter -> 'a -> unit
  type 'a t = {
    parser  : 'a parser;
    printer : 'a printer;
    default : 'a;
  }

  let atom x = x
  let create parser printer default = {parser; printer; default}
  let define ~parse ~print default =
    let parser x =
      try `Ok (parse x) with exn -> `Error (Caml.Printexc.to_string exn) in
    let printer ppf x =
      Format.pp_print_string ppf (print x) in
    create parser printer default

  let converter conv : 'a Arg.converter = conv.parser, conv.printer
  let default conv = conv.default
  let wrap (parser,printer) default = {parser; printer; default}

  (* lift cmdliner into our converters *)
  let bool = wrap Arg.bool false
  let char = wrap Arg.char '\x00'
  let int = wrap Arg.int 0
  let nativeint = wrap Arg.nativeint Nativeint.zero
  let int32 = wrap Arg.int32 Int32.zero
  let int64 = wrap Arg.int64 Int64.zero
  let float = wrap Arg.float 0.
  let string = wrap Arg.string ""
  let enum x =
    let _, default = List.hd_exn x in
    wrap (Arg.enum x) default
  let file = wrap Arg.file ""
  let dir = wrap Arg.dir ""
  let non_dir_file = wrap Arg.non_dir_file ""
  let list ?sep x = wrap (Arg.list ?sep (converter x)) []
  let array ?sep x =
    let default = [| |] in
    wrap (Arg.array ?sep (converter x)) default
  let pair ?sep x y =
    let default = x.default,y.default in
    wrap (Arg.pair ?sep (converter x) (converter y)) default
  let t2 = pair
  let t3 ?sep x y z =
    let a = converter x in
    let b = converter y in
    let c = converter z in
    let default = (default x, default y, default z) in
    wrap (Arg.t3 ?sep a b c) default
  let t4 ?sep w x y z =
    let a = converter w in
    let b = converter x in
    let c = converter y in
    let d = converter z in
    let default = (default w, default x, default y, default z) in
    wrap (Arg.t4 ?sep a b c d) default
  let some ?none x = wrap (Arg.some ?none (converter x)) None
end


module Error = struct
  type t = error = ..
  type t += Configuration
  type t += Invalid of string

  let register_printer = Caml.Printexc.register_printer
  let pp ppf e =
    Format.pp_print_string ppf (Caml.Printexc.to_string e)

end

type manpage_block = [
  | `I of string * string
  | `Noblank
  | `P of string
  | `Pre of string
  | `S of string
]

type ctxt = {get : 'a. 'a Future.t -> 'a}

module Grammar : sig
  open Cmdliner

  type ('a,'b) arity
  val atom : ('a,'a) arity
  val list : ('a,'a list) arity

  type ('f,'r) spec
  type 'a param

  type 'a rule = ?deprecated:string ->
    ?default:'a ->
    ?docv:string -> ?doc:string ->
    ?synonyms:string list -> string -> 'a Future.t

  (** [extend arity conv make_term] extends the grammar of the plugin
      with a new term.

      Creates a parameter specification function, taking care of
      correct naming and defaulting. The user provided [make_term]
      function is called as [make_term conv default info], where
      [conv] is properly wrapped/to_converterped cmdliner converter,
      [default] is the final default value (read from config, or env,
      or provided by a user) and [info] is the argument information
      data structure with all names properly prefixed.

      The returned value must be a term built from these parts. It
      will be eventualy added to the program cmdline grammar. *)
  val extend :
    ('a,'b) arity -> 'a Type.t ->
    ('a Arg.conv -> 'b -> Arg.info -> 'b Term.t) -> 'b rule

  val describe : Manpage.block list -> unit

  val extension : (ctxt -> unit) -> unit
  val action : ?doc:string -> string -> ('a,unit) spec -> (ctxt -> 'a) -> unit


  val args : 'a param -> ('a -> 'b,'b) spec
  val ($) : ('a, 'b -> 'c) spec -> 'b param -> ('a,'c) spec

  val rest : 'a param -> 'a list param

  val argument :
    ?docv:string ->
    ?doc:string -> 'a Type.t -> 'a param

  val param :
    ?docv:string ->
    ?doc:string ->
    ?as_flag:'a ->
    ?short:char ->
    string ->
    'a Type.t ->
    'a param

  val param_all :
    ?docv:string ->
    ?doc:string ->
    ?as_flag:'a ->
    ?short:char ->
    string ->
    'a Type.t ->
    'a list param

  val flag :
    ?docv:string ->
    ?doc:string ->
    ?short:char ->
    string -> bool param

  val eval :
    ?name:string ->
    ?version:string ->
    ?env:(string -> string option) ->
    ?help:Format.formatter ->
    ?err:Format.formatter ->
    ?argv:string array -> unit -> (unit, Error.t) Result.t
end = struct
  open Cmdliner

  type len = Fin of int | Inf
  type 'a param =
    | Pos : 'a Type.t * Arg.info -> 'a param
    | All : 'a param -> 'a list param
    | Key : 'a Term.t -> 'a param

  type ('a,'b) spec = {
    run : 'a -> 'b Term.t;
    len : len;
  }

  type plugin_ctxt = {
    name : string;
    config : (string * string) list;
  }

  type command = unit Term.t * Term.info
  type ('a,'b) arity = 'a Type.t -> 'b Type.t

  type 'a rule = ?deprecated:string ->
    ?default:'a ->
    ?docv:string -> ?doc:string ->
    ?synonyms:string list -> string -> 'a Future.t

  let unit = Term.const ()

  let plugin_specs = Hashtbl.create (module String)
  let plugin_pages = Hashtbl.create (module String)
  let plugin_codes = Hashtbl.create (module String)
  let plugin_spec = ref (fun _ -> unit)
  let plugin_page = ref []
  let plugin_code = ref None
  let commands : command list ref = ref []

  let argument (type a) ?(docv="ARG") ?doc t : a param =
    Pos (t, Arg.info ~docv ?doc [])

  let names short long = match short with
    | None -> [long]
    | Some short -> [sprintf "%c" short; long]

  let param ?docv ?doc ?as_flag:vopt ?short long t : 'a param =
    let t = Type.converter t and d = Type.default t in
    Key Arg.(value & opt ?vopt t d & info ?docv ?doc &
             names short long)

  let param_all ?docv ?doc ?as_flag:vopt ?short long t : 'a param =
    let t = Type.converter t in
    Key Arg.(value & opt_all ?vopt t [] & info ?docv ?doc &
             names short long)

  let flag ?docv ?doc ?short long : bool param =
    Key Arg.(value & flag & info ?docv ?doc & names short long)

  let rest p = All p

  let add (type a) len (b : a param) = match len,b with
    | Fin len, Pos _ -> Fin (len + 1)
    | Fin _, All _ -> Inf
    | Inf, All _ ->
      invalid_arg "can't use `rest' twice in the same list of arguments"
    | _ -> len

  let one t name p =
    let d = Type.default t
    and c = Type.converter t in
    Arg.(value & pos p c d name)

  let all t name p =
    let c = Type.converter t in
    match p with
    | 0 -> Arg.(value & pos_all c [] name)
    | p -> Arg.(value & pos_right (p-1) c [] name)

  let args : type a. a param -> (a -> 'b,'b) spec = fun a -> {
      len = add (Fin 0) a;
      run = fun f -> Term.(pure f $ match a with
        | Pos (t,i) -> one t i 0
        | All (Pos (t,i)) -> all t i 0
        | Key t -> t
        | All _ -> assert false)}


  let ($) (type a) args (b : a param) = {
    len = add args.len b;
    run = fun f -> Term.(args.run f $ match b with
      | Key t -> t
      | _ -> match args.len with
        | Inf -> assert false
        | Fin n -> match b with
          | Pos (t,i) -> one t i n
          | All (Pos (t,i)) -> all t i n
          | _ -> assert false)
  }

  let extension code =
    plugin_code := Some code

  let action ?doc name {run} command =
    let term = run @@ command {get = Future.peek_exn}
    and info = Term.info ?doc name in
    commands := (term,info) :: !commands

  let reset_plugin () =
    plugin_spec := (fun _ -> unit);
    plugin_page := [];
    plugin_code := None

  let () = Stream.observe Plugins.events @@ function
    | `Loaded p ->
      let name = Plugin.name p in
      let filename = "/dev/null" in (* TODO: obtain a proper file name *)
      let ctxt = {
        name;
        config = ConfigFile.read_or_fail filename
      } in
      let term = !plugin_spec ctxt in
      let () =
        Hashtbl.update plugin_pages name ~f:(function
            | None -> !plugin_page
            | Some men -> men @ !plugin_page) in
      Option.iter plugin_code.contents ~f:(fun code ->
          Hashtbl.add_exn plugin_codes name code);
      reset_plugin ();
      Hashtbl.add_exn plugin_specs name term;
    | `Errored _ -> reset_plugin ()
    | _ -> ()

  let prepend_deprecation_notice notice doc =
    Option.value_map notice ~default:doc ~f: (fun notice ->
        notice ^ " " ^ doc)

  let option_name ctxt name = sprintf "%s-%s" ctxt.name name

  let undashify = String.map ~f:(function
      | '-' -> '_'
      | x -> x)

  let env_var_for_option name =
    sprintf "BAP_%s" @@
    String.uppercase (undashify name)

  let get_from_env name =
    let name = env_var_for_option name in
    try Some (Caml.Sys.getenv name)
    with Caml.Not_found -> None

  let get_from_config ctxt name =
    List.Assoc.find ~equal:String.equal ctxt.config name

  let decide_default {Type.parser; default} user_default name ctxt =
    let name = option_name ctxt name in
    let parse ~where what = match parser what with
      | `Ok v -> v
      | `Error err ->
        fail "the value %S for the parameter %S is malformed in the %s - %s"
          what name where err in
    match get_from_env name with
    | Some v -> parse ~where:"process environment " v
    | None -> match get_from_config ctxt name with
      | None -> Option.value user_default ~default
      | Some v -> parse ~where:"configuration file" v


  let extend wrap conv make_term =
    fun ?deprecated:notice ?default ?(docv="VAL")
      ?(doc="Undocumented") ?(synonyms=[]) name ->
      let future, promise = Future.create () in
      let rest = !plugin_spec in
      plugin_spec := begin fun ctxt ->
        let names = List.map (name::synonyms) ~f:(option_name ctxt) in
        let doc = prepend_deprecation_notice notice doc in
        let ainfo = Arg.info names ~doc ~docv in
        let default = decide_default (wrap conv) default name ctxt in
        let conv = Type.converter conv in
        Term.(const (fun x () -> Promise.fulfill promise x) $
              make_term conv default ainfo $ rest ctxt)
      end;
      future
  and list x = Type.list x
  and atom x = x

  let describe man =
    plugin_page := man @ !plugin_page

  let concat_pages () =
    Set.of_list (module String) (Hashtbl.keys plugin_pages) |>
    Set.fold ~init:[] ~f:(fun pages plugin ->
        pages @
        `S ("Plugin " ^ plugin) ::
        Hashtbl.find_exn plugin_pages plugin)

  let (++) t1 t2 = Term.(const (fun () () -> ()) $ t1 $ t2)

  let concat_plugins () =
    let init = Term.const () in
    Hashtbl.fold plugin_specs ~init ~f:(fun ~key:_ ~data:t1 t2 ->
        t1 ++ t2)

  let progname =
    Caml.Filename.basename (Caml.Sys.executable_name)

  let eval_plugins () =
    Hashtbl.iter plugin_codes ~f:(fun code ->
        code {get=Future.peek_exn})

  let eval ?(name=progname) ?version ?env ?help ?err ?argv () =
    let man = concat_pages () in
    let main_info = Term.info ?version ~man name in
    let plugins = Term.(const eval_plugins $ concat_plugins ()) in
    let commands = List.map !commands ~f:(fun (term,info) ->
        plugins ++ term, info) in
    match Term.eval_choice ~catch:false ?env ?help ?err ?argv
            (plugins,main_info) commands with
    | `Ok () | `Version | `Help -> Ok ()
    | `Error _ -> Error Error.Configuration
end

module Extension = struct

  module Type = Type

  module Config = struct
    open Cmdliner

    type 'a param = 'a future
    type reader = ctxt = {get : 'a. 'a param -> 'a}
    type ctxt = reader
    type manpage_block = [
      | `I of string * string
      | `Noblank
      | `P of string
      | `Pre of string
      | `S of string
    ]


    let converter = Type.define
    let deprecated = "DEPRECATED."

    let get ctxt x = ctxt.get x

    let atom = Grammar.atom and list = Grammar.list
    let param ?as_flag conv =
      Grammar.extend atom conv @@ fun conv def info ->
      Arg.value @@ Arg.opt ?vopt:as_flag conv def info

    let param_all ?as_flag conv =
      Grammar.extend list conv @@ fun conv def info ->
      Arg.value @@ Arg.opt_all ?vopt:as_flag conv def info

    let flag conv =
      Grammar.extend atom conv @@ fun _ def docs ->
      Term.(const (fun x -> x || def) $ Arg.value (Arg.flag docs))
    let flag = flag Type.bool ~default:false

    let manpage man = Grammar.describe (man :> Manpage.block list)

    let determined (p:'a param) : 'a future = p

    let when_ready f : unit = Grammar.extension f

    let doc_enum = Arg.doc_alts_enum

    include Bap_main_config
  end

  module Command = struct
    type ('f,'r) t = ('f,'r) Grammar.spec
    type 'a param = 'a Grammar.param

    let declare = Grammar.action
    let ($) = Grammar.($)
    let args = Grammar.args
    let rest = Grammar.rest
    let argument = Grammar.argument
    let param = Grammar.param
    let param_all = Grammar.param_all
    let flag = Grammar.flag
  end

  module Syntax = struct
    let (-->) {get} v = get v
  end

  module Error = Error
end

type error += Already_initialized
type error += Broken_plugins of (string * Base.Error.t) list

type state =
  | Uninitialized
  | Loaded of plugin list
  | Failed of error

let state = ref Uninitialized

let enable_logging = function
  | Some (`Formatter fmt) -> Bap_main_log.process_events fmt
  | Some (`Dir logdir) -> Bap_main_log.in_directory ~logdir ()
  | None -> Bap_main_log.in_directory ()

let init ?features ?library ?argv ?env ?log ?out ?err ?name ?version () =
  match state.contents with
  | Loaded _
  | Failed _ -> Error Already_initialized
  | Uninitialized ->
    enable_logging log;
    let result = Plugins.load ?provides:features ?library () in
    Format.eprintf "finished to load plugins@\n%!";
    let plugins,failures =
      List.partition_map result ~f:(function
          | Ok p -> `Fst p
          | Error (p,e) ->
            Format.eprintf "Plugin %s failed with %a@\n%!"
              p Base.Error.pp e;
            `Snd (p,e)) in
    if List.is_empty failures
    then match Grammar.eval ?name ?version ?env ?help:out ?err ?argv () with
      | Ok () ->
        state := Loaded plugins;
        Ok ()
      | Error err ->
        state := Failed err;
        Error err
    else begin
      let problem = Broken_plugins failures in
      state := Failed problem;
      Error problem
    end
