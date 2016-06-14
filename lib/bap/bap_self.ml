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
    include Bap_config

    let (/) = Filename.concat
    let confdir = confdir / name
    let conf_filename = confdir / "config"

    exception Improper_format of string

    let options () =
      let string_splitter str =
        match String.split str ~on:'=' with
        | [k; v] -> (k, v)
        | _ -> raise (Improper_format str)
      in
      let split_filter = List.map ~f:string_splitter in
      try
        In_channel.with_file conf_filename ~f:(fun ch -> In_channel.input_lines ch
                                                         |> split_filter)
      with Sys_error _ -> []

    let get name =
      let search_for = options () |> List.Assoc.find in
      search_for name

    let set ~name ~data =
      let old_conf = options () in
      let new_conf = List.Assoc.add old_conf name data in
      let conf_lines = List.map new_conf ~f:(fun (k, v) -> k ^ "=" ^ v) in
      let write_lines () = Out_channel.write_lines
          conf_filename conf_lines in
      try
        write_lines()
      with Sys_error _ ->
        Unix.mkdir confdir 0755; write_lines()
  end

end
