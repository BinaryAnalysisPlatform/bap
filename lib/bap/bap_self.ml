open Core_kernel
open Bap_bundle.Std
open Bap_future.Std
open Bap_plugins.Std
open Format
open Cmdliner

module Event = Bap_main_event
module Buffer = Caml.Buffer

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
    | exception Caml.Not_found -> false
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
    let output = Buffer.add_substring buf in
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
    open Bap_main.Extension
    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = Format.formatter -> 'a -> unit
    type 'a converter = 'a Type.t
    type 'a param = 'a Future.t
    type reader = {get : 'a. 'a param -> 'a}
    type manpage_block = [
      | `I of string * string
      | `Noblank
      | `P of string
      | `Pre of string
      | `S of string
    ]

    let converter parser printer default =
      Type.define default
        ~parse:(fun x -> match parser x with
            | `Ok x -> x
            | `Error err -> failwith err)
        ~print:(fun x ->
            Format.asprintf "%a" printer x)

    let prepend_deprecation dep doc =
      match dep with
      | None -> doc
      | Some dep -> sprintf "%s %s" dep doc


    let param t ?deprecated
        ?default:(d=Type.default t)
        ?as_flag
        ?(docv=Type.name t) ?(doc="Undocumented.") ?synonyms name =
      let t = Type.(docv %: t =? d) in
      let doc = prepend_deprecation deprecated doc in
      Configuration.parameter ~doc ?as_flag ?aliases:synonyms t name |>
      Configuration.determined

    let param_all t ?deprecated
        ?default
        ?as_flag
        ?(docv=Type.name t) ?(doc="Undocumented.") ?synonyms name =
      let t = Type.(docv %: t) in
      let doc = prepend_deprecation deprecated doc in
      Configuration.parameters ~doc ?as_flag ?aliases:synonyms t name |>
      Configuration.determined |>
      Future.map ~f:(fun res -> match default with
          | None -> res
          | Some default -> match res with
            | [] -> default
            | xs -> xs)

    let flag ?deprecated ?docv ?(doc="Undocumented.") ?synonyms name =
      let doc = prepend_deprecation deprecated doc in
      Configuration.flag ~doc ?aliases:synonyms name |>
      Configuration.determined

    let determined x = x
    let when_ready f = declare @@ fun _ ->
      try Ok (f {get = fun x -> Future.peek_exn x}) with
      | Invalid_argument s -> Error (Error.Invalid s)
      | exn ->
        let backtrace = Caml.Printexc.get_backtrace () in
        Error (Error.Bug (exn,backtrace))

    let manpage (ps : manpage_block list) =
      let open Format in
      let buf = Buffer.create 64 in
      let ppf = formatter_of_buffer buf in
      List.iter ps ~f:(function
          | `S name -> fprintf ppf "# %s@\n@\n" name
          | `P text -> fprintf ppf "%a@\n@\n" pp_print_text text
          | `Pre code -> fprintf ppf "```@\n%s@\n```@\n@\n" code
          | `I (item,desc) -> fprintf ppf "%s %s@\n@\n" item desc
          | `Noblank -> ());
      fprintf ppf "%!";
      documentation (Buffer.contents buf)

    let doc_enum = Cmdliner.Arg.doc_alts_enum
    let deprecated = "DEPRECATED"
    let confdir = Configuration.confdir
    let datadir = Configuration.datadir
    let libdir = Configuration.libdir
    let version = Configuration.version
    include Type
    let some ?none:_ t = some t
  end
end
