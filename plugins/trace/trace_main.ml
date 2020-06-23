open Core_kernel
open Bap_plugins.Std
open Bap_future.Std
open Bap.Std
open Bap_traces.Std
open Format
open Result.Monad_infix
include Self()


let print_meta trace =
  Trace.meta trace |>
  Dict.data |> Seq.to_list |>
  List.sort ~compare:(fun m1 m2 ->
      if Value.is Meta.trace_stats m1 then -1
      else if Value.is Meta.trace_stats m2 then 1
      else Value.compare m1 m2) |>
  List.iter ~f:(printf "%a@." Value.pp)

let try_dump uri =
  Result.map (Trace.load uri) ~f:(fun trace ->
      print_meta trace;
      printf "@[<v2>events {@\n";
      Trace.read_events trace |> Sequence.iter
        ~f:(printf "%a@\n" Value.pp);
      printf "@]@\n}")


let dump uri = match try_dump uri with
  | Error err -> Error err
  | Ok () -> Ok `Exit

let rec load = function
  | [] -> Ok `Done
  | uri :: uris -> Trace.load uri >>= fun trace ->
    Traces.add trace;
    load uris

exception Incompatibe_args

let main dump_uri loads =
  match dump_uri,loads with
  | Some _, _ :: _ -> raise Incompatibe_args
  | Some uri,[] -> dump uri
  | None,loads -> load loads

module Cmdline = struct
  let () = Config.manpage [
      `S "SYNOPSIS";
      `Pre "
        $(b,bap) $(b,--)$(mname)$(b,-dump)=$(i,URI)
        $(b,bap) $(i,BINARY) $(b,--)$(mname)$(b,-load)=$(i,URI)...
       ";
      `S "DESCRIPTION";
      `P "Loads and prints traces. The plugin can be used in two
       modes. When called as $(b,--)$(mname)$(b,-dump) it will just dump the
       specified trace and exit. In the second mode, it will load
       specified traces, so that they can be used by
       analysis. The loaded traces must be runs of the analyzed
       $(i,BINARY). The loaded traces are accessible via the
       $(b,Traces) of the traces library.";
      `S "SEE ALSO";
      `P "$(b,bap-trace)(3)"
    ]

  let uri_of_string str =
    let uri = Uri.of_string str in
    match Uri.scheme uri with
    | None -> Uri.with_scheme uri (Some "file")
    | Some _ -> uri

  let uri =
    Config.converter (fun s -> `Ok (uri_of_string s)) Uri.pp_hum Uri.empty

  let dump : Uri.t option Config.param =
    let doc = "Dump a trace specified by $(docv)" in
    Config.(param (some uri) "dump" ~docv:"URI" ~doc)

  let load : Uri.t list Config.param =
    let doc = "Load trace from the specified $(docv). The option maybe
    used many times to load several traces" in
    Config.(param_all uri "load" ~docv:"URI" ~doc)

  let exitf fmt =
    kfprintf (fun ppf -> pp_print_newline ppf (); exit 1)
      err_formatter fmt

  let () =
    Config.when_ready (fun {Config.get=(!)} ->
        Future.upon Plugins.loaded (fun () ->
            match main !dump !load with
            | Ok `Done -> ()
            | Ok `Exit -> exit 0
            | Error e -> match e with
              | `Protocol_error err ->
                exitf "Protocol error: %a" Error.pp err
              | `System_error err ->
                exitf "System error: %s" @@ Unix.error_message err
              | `No_provider ->
                exitf "No provider for the given URI"
              | `Ambiguous_uri ->
                exitf "More than one provider for a given URI"
              | exception Incompatibe_args ->
                exitf "Incompatible arguments, see usage SYNOPSIS" ))

end
