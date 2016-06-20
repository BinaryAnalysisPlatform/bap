open Core_kernel.Std
open Bap_future.Std
open Bap_plugins.Std
open Format

let perm = 0o770
let getenv opt = try Some (Sys.getenv opt) with Not_found -> None

let rec mkdir path =
  let par = Filename.dirname path in
  if not(Sys.file_exists par) then mkdir par;
  if not(Sys.file_exists path) then
    Unix.mkdir path perm

let (/) = Filename.concat

let log_folder = match getenv "BAP_LOG_DIR" with
  | Some dir -> dir
  | None -> match getenv "XDG_STATE_HOME" with
    | Some dir -> dir / "bap"
    | None -> match getenv "HOME" with
      | None -> Filename.get_temp_dir_name () / "bap"
      | Some home -> home / ".local" / "state" / "bap"

let max_logs = match getenv "BAP_BACKLOG" with
  | None -> 99
  | Some n -> try Int.of_string n with
      exn -> invalid_argf "BAP_BACKLOG expect a number" ()

let rec rotate max_logs file =
  let next = try Scanf.sscanf file "%s@~%d" (fun name num ->
      Option.some_if (num < max_logs)
        (sprintf "%s~%d" name (num + 1)))
    with exn when max_logs > 0 -> Some (file ^ "~1")
       | exn -> None in
  match next with
  | None -> FileUtil.rm [file]
  | Some next ->
    if Sys.file_exists next then rotate max_logs next;
    FileUtil.mv file next

let string_of_level = function
  | Bap_event.Log.Debug -> "debug"
  | Bap_event.Log.Info -> "info"
  | Bap_event.Log.Warning -> "warning"
  | Bap_event.Log.Error -> "error"

let print ppf {Bap_event.Log.level; section; message} =
  fprintf ppf "%s.%s> %s@." section (string_of_level level) message

let print_message ppf msg =
  print ppf msg;
  let open Bap_event.Log in match msg.level with
  | Error -> print err_formatter msg
  | _ -> ()

let open_log_channel () =
  try
    mkdir log_folder;
    let file = log_folder / "log" in
    if Sys.file_exists file
    then rotate max_logs file;
    let ch = Out_channel.create file in
    formatter_of_out_channel ch
  with exn ->
    eprintf "log.error> unable to open log file: %a@." Exn.pp exn;
    eprintf "log.info> will continue logging to stderr@.";
    err_formatter

let log_plugin_events () =
  let open Bap_event.Log in
  let section = "loader" in
  Stream.observe Plugins.events (function
      | `Loaded p ->
        message Info ~section
          "Loaded %s from %S" (Plugin.name p) (Plugin.path p)
      | `Loading p ->
        message Debug ~section "Loading %s from %S" (Plugin.name p) (Plugin.path p)
      | `Opening p ->
        message Debug ~section "Opening bundle %s" p
      | `Errored (path,err) ->
        message Error ~section
          "Failed to load plugin %S: %a" path Error.pp err
      | `Linking lib ->
        message Debug ~section "Linking library %s" lib)


let start () =
  let ppf = open_log_channel () in
  Stream.observe Bap_event.stream (function
      | Bap_event.Log.Message message -> print_message ppf message
      | e -> fprintf ppf "%a@." Bap_event.pp e);
  log_plugin_events ()
