(* Base kills exn, so we have to do it before opening *)
type error = exn = ..

open Base
open Stdio
open Bap_future.Std
open Bap_plugins.Std
open Bap_bundle.Std

module Format = Caml.Format

let fail fmt = Printf.ksprintf failwith fmt
let sprintf = Printf.sprintf


type plugin_info = {
  cons : string list;
  tags : string list;
  docs : string;
}

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
    if not (Stdlib.Sys.file_exists filename) then []
    else try In_channel.with_file filename ~f:(fun ch ->
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

  let show {printer} x = Format.asprintf "%a" printer x
  let converter conv : 'a Arg.converter = conv.parser, conv.printer
  let default conv = conv.default
  let redefault t default = {t with default}
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
  type t += Bug of exn * string
  type t += Already_initialized
  type t += Already_failed of t
  type t += Recursive_init
  type t += Broken_plugins of (string * Base.Error.t) list
  type t += Unknown_plugin of string
  type t += Exit_requested of int
  type t += Bad_recipe of Bap_recipe.error

  let register_printer = Caml.Printexc.register_printer
  let pp ppf e =
    Format.pp_print_string ppf (Caml.Printexc.to_string e)
end

module Markdown : sig
  val to_manpage : string -> Cmdliner.Manpage.block list
end = struct
  let section s =
    let s = String.strip s in
    let len = String.length s in
    let rec scan n =
      if n < len then match s.[n] with
        | '#' -> scan (n+1)
        | _ -> if n = 0 || n = len - 1 then None
          else Some (String.(strip @@ subo ~pos:n s))
      else None in
    scan 0

  let verbatim s =
    let delimiter = "```" in
    let begs s = String.(chop_prefix (lstrip s) ~prefix:delimiter)
    and ends s = String.(chop_suffix (rstrip s) ~suffix:delimiter) in
    match begs s with
    | None ->
      begin match ends s with
        | None -> `Nocode
        | Some code -> `End code
      end
    | Some code ->
      begin match ends code with
        | None -> `Beg code
        | Some code -> `Pre code
      end

  let list_item s =
    let s = String.strip s in
    let len = String.length s in
    let rec scan_numeric n =
      if n + 1 < len then match s.[n], s.[n+1] with
        | ('.' | ')'), ' ' ->
          Some (String.subo ~len:n s,
                String.subo ~pos:(n+1) s |> String.strip)
        | d,_ when Char.is_digit d -> scan_numeric (n+1)
        | _ -> None
      else None in
    if len > 2 then match s.[0], s.[1] with
      | ('*' | '+' | '-'),' ' ->
        Some (String.subo ~len:1 s,
              String.subo ~pos:2 s)
      | x,_ -> if Char.is_digit x then scan_numeric 1 else None
    else None

  let split input =
    let len = String.length input in
    let rec scan start curr paras =
      if curr < len then match input.[curr] with
        | '\n' ->
          if curr + 2 < len then match input.[curr+1] with
            | '\n' ->
              scan (curr+2) (curr+3) @@
              String.sub ~pos:start ~len:(curr-start) input :: paras
            | _ -> scan start (curr+2) paras
          else scan start (curr+1) paras
        | _ -> scan start (curr+1) paras
      else if start < curr && curr - start <= String.length input
      then List.rev @@
        String.sub ~pos:start ~len:(curr-start) input :: paras
      else List.rev paras in
    scan 0 0 []

  let to_manpage : string -> _ list = fun input ->
    let rec build = function
      | [] -> []
      | para :: paras -> match section para with
        | Some name -> `S name :: build paras
        | None -> match verbatim para with
          | `Pre code -> `Pre code :: build paras
          | `Beg code -> pre [code] paras
          | `End para ->
            `P ("ERROR: Unbalanced ```") ::
            `P para :: build paras
          | `Nocode -> match list_item para with
            | Some (item,desc) -> `I (item,desc) :: build paras
            | None -> `P (String.strip para) :: build paras
    and pre codes = function
      | [] -> [`P ("ERROR : unbalanced ```")]
      | para :: paras -> match verbatim para with
        | `End code | `Beg code ->
          let code = String.concat ~sep:"\n\n" @@ List.rev (code::codes) in
          `Pre code :: build paras
        | _ -> pre (para::codes) paras in
    build (split input)
end

let first_paragraph = function
  | `P p :: _ | `S _ :: `P p :: _ -> Some p
  | _ -> None

let first_sentence text =
  List.hd @@ String.split_on_chars text ~on:[
    '\n'; '.'
  ]

let first_sentence_of_man man =
  Option.(first_paragraph man >>= first_sentence)

let short_description
    ?(default="no description provided") input =
  Option.(input >>| String.uncapitalize |> Option.value ~default)


module Context = struct
  type value = {
    scope : string;
    value : string;
  } [@@deriving sexp]
  type t = {
    env     : value Map.M(String).t;
    plugins : Set.M(String).t Map.M(String).t;
  }

  type info = string * string

  type builder =
    | Building of t
    | Sealed

  type error += Already_defined

  let ready,seal = Future.create ()

  let plugin_descriptions = Hashtbl.create (module String)
  let command_descriptions = Hashtbl.create (module String)

  let builder = ref @@ Building {
      env = Map.empty (module String);
      plugins = Map.empty (module String);
    }

  let update f = match builder.contents with
    | Building ctxt ->
      builder := Building (f ctxt);
      Ok ()
    | Sealed -> Error Already_defined

  let set ~scope ~key ~value = update @@ fun ctxt -> {
      ctxt with
      env = Map.add_exn ctxt.env ~key ~data:{scope; value}
    }

  let get _ x = snd (Future.peek_exn x)

  let set_plugins disabled plugins commands =
    List.iter plugins ~f:(fun (plugin,info) ->
        if not (Set.mem disabled plugin)
        then
          Hashtbl.add_exn plugin_descriptions plugin info);
    List.iter commands ~f:(fun (plugin,commands) ->
        if not (Set.mem disabled plugin)
        then Hashtbl.add_exn command_descriptions
            plugin commands);

    update @@ fun ctxt -> {
      env = Map.filter ctxt.env ~f:(fun {scope} ->
          not (Set.mem disabled scope));
      plugins =
        let init = Map.empty (module String) in
        List.fold ~init plugins ~f:(fun plugins (name,{tags}) ->
            if Set.mem disabled name then plugins
            else
              let data = Set.of_list (module String) tags in
              Map.add_exn plugins name data)
    }

  let request () =
    if Promise.is_fulfilled seal
    then Future.peek_exn ready
    else match builder.contents with
      | Sealed -> assert false
      | Building ctxt ->
        builder := Sealed;
        Promise.fulfill seal ctxt;
        ctxt

  let make_filter features exclude =
    let intersects xs =
      let xs = Set.of_list (module String) xs in
      fun tags -> not @@ Set.is_empty (Set.inter tags xs) in
    let selected = match features with
      | None -> fun _ -> true
      | Some xs -> intersects xs
    and filtered = match exclude with
      | None -> fun _ -> false
      | Some xs -> intersects xs in
    fun tags -> selected tags && not (filtered tags)

  let plugins ?features ?exclude {plugins} =
    let is_selected = make_filter features exclude in
    Map.to_sequence ~order:`Decreasing_key plugins |>
    Sequence.fold ~init:[] ~f:(fun plugins (p,tags) ->
        if is_selected tags
        then
          let info = match Hashtbl.find plugin_descriptions p with
            | None -> "no description provided"
            | Some {docs} -> docs in
          (p,info) :: plugins
        else plugins)

  let commands ?features ?exclude {plugins} =
    let is_selected = make_filter features exclude in
    Hashtbl.fold command_descriptions ~init:[] ~f:(fun ~key ~data cmds ->
        match Map.find plugins key with
        | Some tags -> if is_selected tags then data @ cmds else cmds
        | None -> data @ cmds)

  let features {plugins} =
    Set.to_list @@ Set.union_list (module String) (Map.data plugins)

  let pp ppf {env} =
    Map.iteri env ~f:(fun ~key ~data:{value} ->
        Format.fprintf ppf "%s = %s@\n" key value)

  let digest ?features ?exclude ctxt =
    let plugins = plugins ?features ?exclude ctxt |> List.map ~f:fst in
    let buffer = Buffer.create 128 in
    let ppf = Format.formatter_of_buffer buffer in
    List.iter plugins ~f:(Buffer.add_string buffer);
    let scopes = Set.of_list (module String) plugins in
    Map.iteri ctxt.env ~f:(fun ~key ~data:{scope; value} ->
        if Set.mem scopes scope
        then Format.fprintf ppf "%s = %s@\n" key value);
    Format.pp_print_flush ppf ();
    Caml.Digest.string @@
    Buffer.contents buffer

  (* the info interface *)
  let name = fst
  and doc x =
    short_description @@
    first_sentence_of_man @@
    Markdown.to_manpage (snd x)
end

type ctxt = Context.t

module Pre = struct
  open Cmdliner

  let logdir : string option Term.t =
    let doc = "A folder for log files." in
    let env = Term.env_info ~doc "BAP_LOG_DIR" in
    Arg.(value & opt (some string) None & info ["logdir"; "log-dir"] ~env ~doc)

  let plugin_locations =
    let doc = "Adds folder to the list of plugins search paths" in
    let env = Term.env_info ~doc "BAP_PLUGIN_PATH" in
    Arg.(value & opt_all string [] &
         info ~env ~doc ["L"; "plugin-path"; "load-path"])

  let recipe =
    let doc = "Load the specified recipe" in
    Arg.(value & opt (some string) None & info ~doc ["recipe"])



  let extract ?env option argv =
    fst (Term.eval_peek_opts ?env ~argv option)

  let term = Term.(const (fun _ _ _ -> Ok ()) $ logdir $plugin_locations $recipe)

end


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
    ?synonyms:string list -> string -> (ctxt * 'a) Future.t

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

  val extension :
    ?requires:string list ->
    ?provides:string list ->
    ?doc:string ->
    (ctxt -> (unit,Error.t) Result.t) -> unit

  val action :
    ?doc:string -> string ->
    ('a,ctxt -> (unit,Error.t) Result.t) spec -> 'a -> unit


  val args : ('a,'a) spec
  val ($) : ('a, 'b -> 'c) spec -> 'b param -> ('a,'c) spec

  val argument :
    ?docv:string ->
    ?doc:string -> 'a Type.t -> 'a param

  val arguments :
    ?docv:string ->
    ?doc:string -> 'a Type.t -> 'a list param

  val switch :
    ?doc:('a -> string) ->
    ('a -> string) ->
    'a list -> 'a option param

  val switches :
    ?doc:('a -> string) ->
    ('a -> string) ->
    'a list -> 'a list param

  val parameter :
    ?docv:string ->
    ?doc:string ->
    ?as_flag:'a ->
    ?short:char ->
    string ->
    'a Type.t ->
    'a param

  val parameters :
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

  val flags :
    ?docv:string ->
    ?doc:string ->
    ?short:char ->
    string -> int param

  val dictionary :
    ?docv:string ->
    ?doc:('k -> string) ->
    ?short:('k -> char) ->
    ?as_flag:('k -> 'd) ->
    ('k -> string) ->
    'k list ->
    'd Type.t -> ('k * 'd) list param

  val eval :
    ?man:string ->
    ?name:string ->
    ?version:string ->
    ?env:(string -> string option) ->
    ?help:Format.formatter ->
    ?default:(ctxt -> (unit,Error.t) Result.t) ->
    ?err:Format.formatter ->
    ?argv:string array -> unit -> (unit, Error.t) Result.t
end = struct
  open Cmdliner

  type len = Fin of int | Inf
  type 'a param =
    | Pos : 'a Type.t * Arg.info -> 'a param
    | All : 'a Type.t * Arg.info -> 'a list param
    | Key : 'a Term.t -> 'a param

  type ('a,'b) spec = {
    run : 'a -> 'b Term.t;
    len : len;
  }

  type plugin_ctxt = {
    name : string;
    config : (string * string) list;
  }

  type command = (ctxt -> (unit,Error.t) Result.t) Term.t * Term.info
  type ('a,'b) arity = 'a Type.t -> 'b Type.t

  type 'a rule = ?deprecated:string ->
    ?default:'a ->
    ?docv:string -> ?doc:string ->
    ?synonyms:string list -> string -> (ctxt * 'a) Future.t

  let unit = Term.const (Ok ())

  let plugin_specs = Hashtbl.create (module String)
  let plugin_pages = Hashtbl.create (module String)
  let plugin_codes = Hashtbl.create (module String)
  let plugin_infos = Hashtbl.create (module String)
  let plugin_cmds = Hashtbl.create (module String)
  let plugins = Hashtbl.create (module String)
  let plugin_spec = ref (fun _ -> unit)
  let plugin_page = ref []
  let plugin_code = ref (fun _ -> Ok ())
  let plugin_info = ref None
  let actions = ref []
  let commands : command list ref = ref []

  let dictionary ?(docv="VAL") ?doc ?short ?as_flag name keys data =
    let init = Term.const [] in
    Key (List.fold keys ~init ~f:(fun terms key ->
        let name = name key in
        let doc = match doc with
          | None -> sprintf "sets `%s' to %s" name docv
          | Some doc -> doc key in
        let names = match short with
          | None -> [name]
          | Some short -> [sprintf "%c" (short key); name] in
        let vopt = match as_flag with
          | None -> None
          | Some f -> Some (f key) in
        let t = Type.converter data and d = Type.default data in
        let term = Arg.(value & opt ?vopt t d & info ~docv ~doc names) in
        Term.(const (fun xs x -> (key,x) :: xs) $ terms $ term)))

  let argument (type a) ?(docv="ARG") ?doc t : a param =
    Pos (t, Arg.info ~docv ?doc [])

  let arguments (type a) ?(docv="ARG") ?doc t : a list param =
    All (t, Arg.info ~docv ?doc [])

  let switch ?doc inj opts =
    let doc = Option.value doc
        ~default:(fun s -> sprintf "Select %s" (inj s)) in
    let opts = List.map opts ~f:(fun x ->
        Some x, Arg.info ~doc:(doc x) [inj x]) in
    Key Arg.(value & vflag None opts)

  let switches ?doc inj opts =
    let doc = Option.value doc
        ~default:(fun s -> sprintf "Select %s" (inj s)) in
    let opts = List.map opts ~f:(fun x ->
        x, Arg.info ~doc:(doc x) [inj x]) in
    Key Arg.(value & vflag_all [] opts)

  let names short long = match short with
    | None -> [long]
    | Some short -> [sprintf "%c" short; long]

  let parameter ?docv ?doc ?as_flag:vopt ?short long t : 'a param =
    let t = Type.converter t and d = Type.default t in
    Key Arg.(value & opt ?vopt t d & info ?docv ?doc &
             names short long)

  let parameters ?docv ?doc ?as_flag:vopt ?short long t : 'a param =
    let t = Type.converter t in
    Key Arg.(value & opt_all ?vopt t [] & info ?docv ?doc &
             names short long)

  let flag ?docv ?doc ?short long : bool param =
    Key Arg.(value & flag & info ?docv ?doc & names short long)

  let flags ?docv ?doc ?short long : int param =
    let term = Arg.(value & flag_all & info ?docv ?doc &
                    names short long) in
    let count = Term.(const (List.count ~f:(fun x -> x)) $ term) in
    Key count

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

  let ($) (type a) args (b : a param) = {
    len = add args.len b;
    run = fun f -> Term.(args.run f $ match b with
      | Key t -> t
      | _ -> match args.len with
        | Inf -> assert false
        | Fin n -> match b with
          | Pos (t,i) -> one t i n
          | All (t,i) -> all t i n
          | _ -> assert false)
  }

  let extension ?(requires=[]) ?(provides=[]) ?(doc="") code =
    plugin_info := Option.some @@
      Option.value_map !plugin_info ~f:(fun {tags; cons; docs} -> {
            tags = tags @ provides;
            cons = cons @ requires;
            docs = docs ^ doc;
          })
        ~default:{
          tags = provides;
          cons = requires;
          docs = doc;
        };
    let code' = !plugin_code in
    plugin_code := fun ctxt -> match code ctxt with
      | Ok () -> code' ctxt
      | Error _ as err -> err


  let args = {
    len = Fin 0;
    run = Term.const
  }

  let action ?(doc="no description provided") name {run} command =
    let man = Markdown.to_manpage doc in
    let doc =
      short_description ~default:doc @@
      first_sentence_of_man man in
    let term = run @@ command
    and info = Term.info ~doc ~man name in
    actions := (name,doc) :: !actions;
    commands := (term,info) :: !commands

  let reset_plugin () =
    plugin_spec := (fun _ -> unit);
    plugin_page := [];
    plugin_code := (fun _ -> Ok ());
    plugin_info := None;
    actions := []

  let merge xs ys =
    let xs = Set.of_list (module String) xs
    and ys = Set.of_list (module String) ys in
    Set.(to_list @@ union xs ys)


  let update_from_bundle info plugin =
    let b = Plugin.bundle plugin in
    let {Manifest.tags; cons; desc} = Bundle.manifest b in
    match info with
    | None -> {docs = desc; tags; cons}
    | Some info -> {
        docs = if String.is_empty info.docs then desc else info.docs;
        tags = merge tags info.tags;
        cons = merge cons info.cons
      }

  let () = Stream.observe Plugins.events @@ function
    | `Loaded p ->
      let name = Plugin.name p in
      let (/) = Stdlib.Filename.concat in
      let filename = Bap_main_config.confdir / "config" in
      let ctxt = {
        name;
        config = ConfigFile.read_or_fail filename
      } in
      let term = !plugin_spec ctxt in
      let info = update_from_bundle !plugin_info p in
      let man = match !plugin_page with
        | [] -> Markdown.to_manpage info.docs
        | page -> page in
      Hashtbl.update plugin_pages name ~f:(function
          | None -> man
          | Some men -> men @ man);
      Hashtbl.add_exn plugin_codes name !plugin_code;
      Hashtbl.add_exn plugin_infos name info;
      Hashtbl.add_exn plugins name p;
      if not (List.is_empty !actions)
      then Hashtbl.add_exn plugin_cmds name !actions;
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

  let plugin_section _ =
    Manpage.s_common_options

  let extend wrap typ make_term =
    fun ?deprecated:notice ?default ?(docv="VAL")
      ?(doc="Undocumented") ?(synonyms=[]) name ->
      let value, ready = Future.create () in
      let rest = !plugin_spec in
      plugin_spec := begin fun ctxt ->
        let names = List.map (name::synonyms) ~f:(option_name ctxt) in
        let doc = prepend_deprecation_notice notice doc in
        let docs = plugin_section ctxt.name in
        let ainfo = Arg.info names ~docs ~doc ~docv in
        let default = decide_default (wrap typ) default name ctxt in
        let conv = Type.converter typ in
        let set_value x = function
          | Error _ as err -> err
          | Ok () ->
            Promise.fulfill ready x;
            Context.set
              ~scope:ctxt.name
              ~key:(option_name ctxt name)
              ~value:(Type.show (wrap typ) x) in
        Term.(const set_value $
              make_term conv default ainfo $ rest ctxt)
      end;
      Future.both Context.ready value

  and list x = Type.list x
  and atom x = x


  let describe man =
    plugin_page := man @ !plugin_page

  let (>>>) t1 t2 = Term.(const (fun t1 t2 -> match t1 with
      | Error _ as err -> err
      | Ok () -> t2) $ t1 $ t2)

  let concat_plugins () =
    Hashtbl.fold plugin_specs ~init:unit ~f:(fun ~key:_ ~data:t1 t2 ->
        t1 >>> t2)

  let progname =
    Caml.Filename.basename (Caml.Sys.executable_name)


  let switch_bundle name =
    let was = main_bundle () in
    let now = Hashtbl.find_exn plugins name in
    set_main_bundle (Plugin.bundle now);
    was

  let try_eval f x = try f x with
    | Invalid_argument s -> Error (Error.Invalid s)
    | exn ->
      let backtrace = Caml.Printexc.get_backtrace () in
      Error (Error.Bug (exn,backtrace))


  let eval_plugins disabled plugins_term =
    let open Result in
    plugins_term >>= fun () ->
    let disabled = Set.of_list (module String) disabled in
    let plugins = Hashtbl.to_alist plugin_infos in
    let commands = Hashtbl.to_alist plugin_cmds in
    Context.set_plugins disabled plugins commands >>= fun () ->
    let ctxt = Context.request () in
    let init = Ok () in
    Hashtbl.fold plugin_codes ~init ~f:(fun ~key:name ~data:code ->
        function Error _ as err -> err
               | Ok () ->
                 if not (Set.mem disabled name)
                 then
                   let old = switch_bundle name in
                   let res = try_eval code ctxt in
                   set_main_bundle old;
                   res
                 else Ok ())

  let no_plugin_options plugins =
    let init = Term.const [] in
    List.fold plugins ~init ~f:(fun names name ->
        let doc = "Disable the " ^ name ^ " plugin" in
        let docs = plugin_section name in
        let plugin = Arg.(value & flag &
                          info ~doc ~docs ["no-"^name]) in
        let append selected names =
          if selected then name :: names else names in
        Term.(const append $ plugin $ names))

  let help_options ?version ppf plugins =
    let init = Term.const (Ok ()) in
    let fmts = [
      "auto", `Auto;
      "pager", `Pager;
      "groff", `Groff;
      "plain", `Plain;
    ] in

    (* Cmdliner doesn't give us an argument, but a term, so we
       can't use man_format with a term of our own name *)
    let make_help_option name =
      let doc = sprintf "prints more information about the $(b,%s) plugin"
          name in
      let docs = plugin_section name in
      Arg.(value & opt ~vopt:(Some `Auto)
             (some (enum fmts)) None & info ~doc ~docs [name^"-help"]) in

    let print_manpage fmt name = match Hashtbl.find plugin_pages name with
      | None -> Error (Error.Unknown_plugin name)
      | Some manpage ->
        let left = match version with
          | None -> ""
          | Some v -> " " ^ v in
        let caption = "BAP Programmer's Manual" in
        let title = name, 3, "",left, caption in
        let subst = function
          | "tname" | "mname" -> Some name
          | _ -> None in
        Manpage.print ~subst fmt ppf (title,manpage);
        Error (Error.Exit_requested 0) in

    List.fold plugins ~init ~f:(fun served plugin ->
        let serve_manpage served requested = match served with
          | Error _ as err -> err
          | Ok () -> match requested with
            | None -> Ok ()
            | Some fmt -> print_manpage fmt plugin in
        Term.(const serve_manpage $ served $ make_help_option plugin))

  let eval ?(man="") ?(name=progname) ?version ?env
      ?(help=Format.std_formatter) ?default ?err ?argv () =
    let plugin_names = Plugins.list () |> List.map ~f:Plugin.name in
    let disabled_plugins = no_plugin_options plugin_names in
    let plugin_options = concat_plugins () in
    let man = (Markdown.to_manpage man :> Manpage.block list)  in
    let main_info = Term.info ?version ~man name in
    let helps = help_options ?version help plugin_names in
    let plugins = Pre.term >>> helps >>>
      Term.(const eval_plugins $disabled_plugins $plugin_options) in
    let commands = List.map !commands ~f:(fun (command,info) ->
        plugins >>> Term.(const (fun cmd ->
            let ctxt = Context.request () in
            cmd ctxt) $ command), info) in
    let main = match default with
      | None -> plugins
      | Some f -> Term.(const (fun p -> match p with
          | Error _ as err -> err
          | Ok () -> f @@ Context.request ()) $ plugins) in
    match Term.eval_choice ~catch:false ?env ~help ?err ?argv
            (main,main_info) commands with
    | `Ok (Ok ()) -> Ok ()
    | `Ok (Error _ as err) -> err
    | `Version | `Help -> Ok ()
    | `Error _ -> Error Error.Configuration
end

module Extension = struct

  module Type = Type
  type 'a typ = 'a Type.t
  type ctxt = Context.t

  let declare = Grammar.extension

  type manpage_block = [
    | `I of string * string
    | `Noblank
    | `P of string
    | `Pre of string
    | `S of string
  ]

  let manpage man = Grammar.describe (man :> Cmdliner.Manpage.block list)
  let documentation doc =
    manpage (Markdown.to_manpage doc)


  module Parameter = struct
    open Cmdliner

    type 'a t = (ctxt * 'a) future

    let converter = Type.define
    let deprecated = "DEPRECATED."

    let get _ x = snd (Future.peek_exn x)

    let atom = Grammar.atom and list = Grammar.list

    let declare ?as_flag conv =
      Grammar.extend atom conv @@ fun conv def info ->
      Arg.value @@ Arg.opt ?vopt:as_flag conv def info

    let declare_list ?as_flag conv =
      Grammar.extend list conv @@ fun conv def info ->
      Arg.value @@ Arg.opt_all ?vopt:as_flag conv def info

    let flag conv =
      Grammar.extend atom conv @@ fun _ def docs ->
      Term.(const (fun x -> x || def) $ Arg.value (Arg.flag docs))

    let flag = flag Type.bool ~default:false

    let determined (p:'a t) : 'a future = Future.map p ~f:snd

    let when_ready f : unit = Grammar.extension @@ fun ctxt ->
      try Ok (f ctxt) with
      | Invalid_argument s -> Error (Error.Invalid s)
      | exn ->
        let backtrace = Caml.Printexc.get_backtrace () in
        Error (Error.Bug (exn,backtrace))

    let doc_enum = Arg.doc_alts_enum

    include Bap_main_config
  end

  module Command = struct
    type ('f,'r) t = ('f,'r) Grammar.spec
    type 'a param = 'a Grammar.param
    let declare = Grammar.action
    let ($) = Grammar.($)
    let args = Grammar.args
    let argument = Grammar.argument
    let arguments = Grammar.arguments
    let switch = Grammar.switch
    let switches = Grammar.switches
    let parameter = Grammar.parameter
    let parameters = Grammar.parameters
    let flag = Grammar.flag
    let flags = Grammar.flags
    let dictionary = Grammar.dictionary
  end

  module Context = Context
  module Syntax = struct
    let (-->) ctxt v = Parameter.get ctxt v
  end
  module Error = Error
end

type state =
  | Uninitialized
  | Initializing
  | Loaded of plugin list
  | Failed of error

let state = ref Uninitialized

let enable_logging = function
  | Some (`Formatter fmt) -> Bap_main_log.process_events fmt
  | Some (`Dir logdir) -> Bap_main_log.in_directory ~logdir ()
  | None -> Bap_main_log.in_directory ()


let load_recipe recipe =
  let paths = [
    Stdlib.Filename.current_dir_name;
    Extension.Parameter.datadir] in
  match Bap_recipe.load ~paths recipe with
  | Ok r ->
    Stdlib.at_exit (fun () -> Bap_recipe.close r);
    Ok r
  | Error err -> Error (Error.Bad_recipe err)

let (>>=) x f = Result.bind x ~f


let init
    ?features
    ?requires
    ?library ?(argv=Sys.argv)
    ?env ?log ?out ?err ?man
    ?name ?(version=Bap_main_config.version)
    ?default
    () =
  match state.contents with
  | Loaded _ -> Error Error.Already_initialized
  | Failed err -> Error (Error.Already_failed err)
  | Initializing -> Error Error.Recursive_init
  | Uninitialized ->
    let argv = match Pre.(extract ?env recipe argv) with
      | None | Some None -> Ok argv
      | Some (Some spec) ->
        load_recipe spec >>= fun spec ->
        Ok (Array.append argv @@ Bap_recipe.argv spec) in
    argv >>= fun argv ->
    let log = match log with
      | Some _ -> log
      | None -> Option.(join @@ Pre.(extract ?env logdir argv) >>|
                        fun x -> `Dir x) in
    enable_logging log;
    let library = match library with
      | Some libs -> libs
      | None -> match Pre.(extract ?env plugin_locations argv) with
        | None -> []
        | Some libs -> libs in
    let result = Plugins.load ?env:features ?provides:requires ~library () in
    let plugins,failures =
      List.partition_map result ~f:(function
          | Ok p -> `Fst p
          | Error (p,e) -> `Snd (p,e)) in
    if List.is_empty failures
    then match
        Grammar.eval ?name ~version ?env ?help:out
          ?err ?man ~ argv ?default ()
      with
      | Ok () ->
        state := Loaded plugins;
        Ok ()
      | Error err ->
        state := Failed err;
        Error err
    else begin
      let problem = Error.Broken_plugins failures in
      state := Failed problem;
      Error problem
    end
