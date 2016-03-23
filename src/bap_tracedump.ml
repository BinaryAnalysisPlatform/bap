open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Result
open Cmdliner
open Bap_plugins.Std

let uri =
  let doc = "Trace resource identifier RFC3986." in
  let uri =
    let uri_parser str =
      try `Ok (Uri.of_string str) with
        exn -> `Error (Printf.sprintf "bad uri address") in
    let uri_printer fmt u =
      Uri.to_string u |> Format.pp_print_string fmt in
    uri_parser, uri_printer in
  Arg.(required & pos 0 (some uri) None & info [] ~doc ~docv:"URI")

let print_error = function
  | `Protocol_error err -> Error.pp Format.err_formatter err
  | `System_error err -> prerr_endline @@ Unix.error_message err
  | `No_provider -> prerr_endline "No provider for a given URI"
  | `Ambiguous_uri -> prerr_endline "More than one provider for a given URI"

let main uri =
  let _ = Plugins.load() in
  Trace.load uri >>|
  (fun trace ->
     Trace.meta trace |>
     Dict.data |>
     Sequence.iter ~f:(Format.printf "meta: @[%a@]@." Value.pp);
     trace) >>|
  Trace.events >>|
  Sequence.iter ~f:(Format.printf "@[%a@]@." Value.pp)

let cmd =
  let doc = "dump trace to stdout" in
  let man = [
    `S "SYNOPSIS";
    `Pre "
 $(b,$mname) [PLUGIN OPTION]... --list-formats
 $(b,$mname) [OPTION]... URI";
    `S "DESCTIPTION";
    `P "Print bap-traces trace events to stdout"
  ] @ Bap_cmdline_terms.common_loader_options in
  Term.(pure main $ uri),
  Term.info "bap-tracedump" ~doc ~man ~version:Config.version

let _main : unit =
  let argv = Bap_plugin_loader.run Sys.argv in
  match Term.eval ~argv cmd with
  | `Error _ -> exit 1
  | `Ok result ->
    begin
      match result with
      | Error e -> print_error e; exit 2
      | _ -> exit 0
    end
  | `Version
  | `Help -> exit 0
