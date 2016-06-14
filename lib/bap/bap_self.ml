open Core_kernel.Std
open Bap_bundle.Std
open Format

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

    let (/) = Filename.concat
    let confdir = confdir / plugin_name
    let conf_filename = confdir / "config"

    let get_env_options () =
      let prefix = "BAP_" ^ String.uppercase plugin_name ^ "_" in
      let prefix_chop_key (k, v) =
        match String.chop_prefix ~prefix k with
        | Some k -> Some (k, v)
        | None -> None in
      let plugin_filter_map str =
        match String.split str ~on:'=' with
        | k :: vs -> prefix_chop_key (k, String.concat ~sep:"=" vs)
        | [] -> None in
      Unix.environment () |>
      Array.to_list |>
      List.filter_map ~f:plugin_filter_map

    let conf_file_options () =
      let string_splitter str =
        match String.split str ~on:'=' with
        | k :: vs -> k, String.concat ~sep:"=" vs
        | [] -> invalid_arg "empty string" in
      let split_filter = List.map ~f:string_splitter in
      try
        In_channel.with_file
          conf_filename ~f:(fun ch -> In_channel.input_lines ch
                                      |> split_filter)
      with Sys_error _ -> []

    let options () =
      get_env_options () |>
      List.fold ~init:(conf_file_options ())
        ~f:(fun o (k, v) -> List.Assoc.add o k v)

    let get = options () |> List.Assoc.find ~equal:String.Caseless.equal

    let set ~name ~data =
      let name = String.lowercase name in
      let old_conf = conf_file_options () in
      let remaining_conf = List.Assoc.remove old_conf name in
      let new_conf = List.Assoc.add remaining_conf name data in
      let conf_lines = List.map new_conf ~f:(fun (k, v) -> k ^ "=" ^ v) in
      let write_lines () = Out_channel.write_lines
          conf_filename conf_lines in
      try
        write_lines ()
      with Sys_error _ ->
        let dir_permissions = 0o755 in
        let makedir_and_write_lines () =
          Unix.mkdir confdir dir_permissions; write_lines () in
        try
          makedir_and_write_lines ()
        with Unix.Unix_error (Unix.ENOENT, _, _) ->
          Unix.mkdir (Filename.dirname confdir) dir_permissions;
          makedir_and_write_lines ()
  end

end
