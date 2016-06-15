open Core_kernel.Std
open Bap_bundle.Std
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

  module Param = struct
    let plugin_name = name

    type 'a t = 'a ref
    type 'a parser = string -> [ `Ok of 'a | `Error of string ]
    type 'a printer = Format.formatter -> 'a -> unit
    type 'a converter = 'a parser * 'a printer

    let main = ref Term.(const ())

    let conf_file_options : (string, string) List.Assoc.t =
      let conf_filename =
        let (/) = Filename.concat in
        Bap_config.confdir / plugin_name / "config" in
      let string_splitter str =
        let str = String.strip str in
        match String.split str ~on:'=' with
        | k :: _ when String.prefix k 1 = "#" -> None
        | [""] | [] -> None
        | [k] -> invalid_argf
                   "Maybe comment out \"%s\" using # in config file?" k ()
        | k :: vs -> Some (k, String.concat ~sep:"=" vs) in
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
        | `Error err
          -> invalid_argf
               "Could not parse \"%s\" for name \"%s\"" str name ()
        | `Ok v -> v in
      let value = match str with
        | Some v -> parse v
        | None -> value in
      ref value

    let create converter ~default ?(docv="VAL") ~doc ~name : 'a t =
      let param = get_param ~converter ~default ~name in
      let t =
        Arg.(value @@ opt converter !param @@ info [name] ~doc ~docv) in
      main := Term.(const (fun x () -> param := x) $ t $ (!main));
      param

    let flag ?docv ~doc ~name : bool t = assert false
    (* TODO Implement this *)

    let term_info = ref (Term.info ~doc plugin_name)

    type manpage_block = [
        `I of string * string |
        `Noblank |
        `P of string |
        `Pre of string |
        `S of string
    ] list

    let manpage (man:manpage_block) : unit =
      term_info := Term.info ~doc ~man plugin_name

    let extract () : 'a t -> 'a =
      match Term.eval (!main, !term_info) with
      | `Error _ -> exit 1
      | `Ok _ -> (fun p -> !p)
      | `Version | `Help -> exit 0

    let bool = Arg.bool
    let int = Arg.int
    let string = Arg.string
  end

end
