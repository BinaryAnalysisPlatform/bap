open Core_kernel[@@warning "-D"]
open Bap_future.Std
open Bap_plugins.Std
open Format

module Event = Bap_main_event
module Filename = Caml.Filename
module Sys = Caml.Sys
module Unix = Caml_unix

let perm = 0o770
let getenv opt = try Some (Sys.getenv opt) with Caml.Not_found -> None

let rec mkdir path =
  let par = Filename.dirname path in
  if not(Sys.file_exists par) then mkdir par;
  if not(Sys.file_exists path) then
    Unix.mkdir path perm

let (/) = Filename.concat

let log_folder user = match user with
  | Some dir -> dir
  | None -> match getenv "XDG_STATE_HOME" with
    | Some dir -> dir / "bap"
    | None -> match getenv "HOME" with
      | None -> Filename.get_temp_dir_name () / "bap.log"
      | Some home -> home / ".local" / "state" / "bap"

let max_logs = match getenv "BAP_BACKLOG" with
  | None -> 99
  | Some n -> try Int.of_string n with _ ->
    invalid_argf "BAP_BACKLOG expect a number" ()

let rec rotate max_logs file =
  let next = try Scanf.sscanf file "%s@~%d" (fun name num ->
      Option.some_if (num < max_logs)
        (sprintf "%s~%d" name (num + 1)))
    with _ when max_logs > 0 -> Some (file ^ "~1")
       | _ -> None in
  match next with
  | None -> FileUtil.rm [file]
  | Some next ->
    if Sys.file_exists next then rotate max_logs next;
    FileUtil.mv file next

let string_of_level = function
  | Event.Log.Debug -> "debug"
  | Event.Log.Info -> "info"
  | Event.Log.Warning -> "warning"
  | Event.Log.Error -> "error"

let print ppf {Event.Log.level; section; message} =
  fprintf ppf "%s.%s> %s@." section (string_of_level level) message

let print_message ppf msg =
  print ppf msg;
  let open Event.Log in match msg.level with
  | Error -> eprintf "%s@\n%!" msg.message
  | _ -> ()

let lock_filename logdir =
  let digest = Md5.digest_string logdir in
  let name = sprintf "bap-%s.lock" (Md5.to_hex digest) in
  Filename.get_temp_dir_name () / name

let lock file =
  let lock = Unix.openfile file Unix.[O_RDWR; O_CREAT] 0o666 in
  Unix.lockf lock Unix.F_LOCK 0;
  lock

let unlock lock =
  Unix.lockf lock Unix.F_ULOCK 0;
  Unix.close lock

let open_log_channel user_dir =
  try
    let log_folder = log_folder user_dir in
    let file = log_folder / "log" in
    let lock = lock (lock_filename log_folder) in
    protect ~f:(fun () ->
        mkdir log_folder;
        if Sys.file_exists file
        then rotate max_logs file)
      ~finally:(fun () -> unlock lock);
    let ch = Out_channel.create file in
    formatter_of_out_channel ch
  with exn ->
    eprintf "unable to open log file: %a@." Exn.pp exn;
    eprintf "will continue logging to stderr@.";
    err_formatter

let log_plugin_events () =
  let open Event.Log in
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




let process_events ppf =
  Stream.observe Event.stream (function
      | Event.Log.Message message -> print_message ppf message
      | _ -> ());
  log_plugin_events ()

let in_directory ?logdir () =
  process_events (open_log_channel logdir)
