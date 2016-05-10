open Core_kernel.Std
open Bap_future.Std
open Bap.Std
open Format
include Self()

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
  | Event.Log.Debug -> "debug"
  | Event.Log.Info -> "info"
  | Event.Log.Warning -> "warning"
  | Event.Log.Error -> "error"

let print ppf {Event.Log.level; section; message} =
  fprintf ppf "%s.%s> %s@." section (string_of_level level) message

let print_message ppf msg =
  print ppf msg;
  let open Event.Log in match msg.level with
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


let start () =
  let ppf = open_log_channel () in
  Stream.observe Event.stream (function
      | Event.Log.Message message -> print_message ppf message
      | e -> fprintf ppf "%a@." Event.pp e)
