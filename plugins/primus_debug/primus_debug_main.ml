open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
open Bap_future.Std
open Format

include Self()

module Param = struct 
  open Config;;
  manpage [
    `S "DESCRIPTION";
    `P "Monitors a Lisp Machine execution."
  ]

  let monitors = param (list string) "monitor"
      ~doc:
        "A list of events to monitor. A keyword `all` can be use to
      select all events. To ignore a particular event, add `-' before
      the name. An optional + is allowed for the consistency."
end


let starts_with name x = 
  String.length name > 1 && name.[0] = x

let strip name = 
  if starts_with name '+' || starts_with name '-'
  then String.subo ~pos:1 name
  else name

let has_name name p = 
  Primus.Observation.Provider.name p = name

let remove_provider name = List.filter ~f:(Fn.non (has_name name))

let monitor_provider name ps = 
  Primus.Observation.list_providers () |>
  List.find ~f:(has_name name) |> function 
  | None -> invalid_argf "An unknown observation provider `%s'" name ()
  | Some p -> p :: ps

let parse_monitors = 
  List.fold ~init:[] ~f:(fun ps -> function
      | "all" -> ps @ Primus.Observation.list_providers ()
      | name when starts_with name '-' -> remove_provider (strip name) ps 
      | name -> monitor_provider (strip name) ps)

let print_event ev = 
  printf "%a@\n" Sexp.pp_hum ev

let start_monitoring {Config.get} = 
  let module Monitor(Machine : Primus.Machine.S) = struct 
    let init () =
      parse_monitors @@ get Param.monitors |>
      List.iter ~f:(fun m ->
          Stream.observe (Primus.Observation.Provider.data m)
            print_event);
      Machine.return ()
  end in
  ()


let () = Config.when_ready start_monitoring
