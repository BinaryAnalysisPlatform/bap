open Core_kernel
open Regular.Std
open Bap_future.Std
open Format

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

include Printable.Make(struct
    type nonrec t = t
    let module_name = Some "Bap.Std.Event"
    let pp ppf e =
      pp_print_string ppf (string_of_event e)
  end)


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
end
