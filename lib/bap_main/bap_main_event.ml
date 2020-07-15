open Core_kernel
open Bap_future.Std
open Bap_plugins.Std
open Bap_bundle.Std
open Format

module Buffer = Caml.Buffer

(* we're reusing [exn] type only because we want to use
   Printexc magic printer. It is not visible from outside,
   that the type is [exn], and has no other consequences.
   For example, it is not possible to confuse exception
   with event, or to confuse their printers.
*)
type t = exn = ..
type event = t = ..

let register_printer = Caml.Printexc.register_printer
let stream,new_event = Stream.create ()
let send (ev : t) = Signal.send new_event ev
let string_of_event = Caml.Printexc.to_string
let pp ppf e =
  pp_print_string ppf (string_of_event e)

module Log = struct
  type level =
    | Debug
    | Info
    | Warning
    | Error

  type info = {
    level : level;
    section : string;
    message : string;
  }

  type event += Message of info

  type event += Progress of {
      task  : string;
      note  : string option;
      stage : int option;
      total : int option;
    }


  let message level ~section fmt =
    let buf = Buffer.create 64 in
    let ppf = formatter_of_buffer buf in
    kfprintf (fun ppf ->
        pp_print_flush ppf ();
        let message = Buffer.contents buf in
        send (Message {
            level;
            section;
            message;
          })) ppf fmt

  let string_of_level = function
    | Debug -> "debug"
    | Info -> "info"
    | Warning -> "warning"
    | Error -> "error"

  let progress ?note ?stage ?total task = send @@ Progress {
      task; note; stage; total;
    }

  let pp ppf {level; section; message} =
    fprintf ppf "%s.%s> %s@." section (string_of_level level) message

  let () = register_printer (function
      | Message msg -> Some (asprintf "%a" pp msg)
      | _ -> None)

  module Create() = struct
    let bundle = main_bundle ()

    let main =
      let base = Filename.basename Sys.executable_name in
      try Filename.chop_extension base with _ -> base


    let manifest =
      try Bundle.manifest bundle
      with _exn -> Manifest.create main

    let name = Manifest.name manifest

    let report_progress ?task ?note ?stage ?total () =
      let task = match task with
        | None -> name
        | Some subtask -> sprintf "%s/%s" name subtask in
      let task = if String.(name = main) then task
        else sprintf "%s/%s" main task in
      progress ?note ?stage ?total task

    let has_var v = match Sys.getenv ("BAP_" ^ String.uppercase v) with
      | exception Caml.Not_found -> false
      | "false" | "0" -> false
      | _ -> true

    let is_verbose = has_var ("DEBUG_"^name) ||
                     has_var ("DEBUG")

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


  end
end
