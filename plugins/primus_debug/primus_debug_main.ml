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

  let output = param (some string) "output"
      ~doc:
        "A name of a file in which to store the monitor output. If not
      specified, then outputs result into stdout"
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

let print_event out ev = 
  fprintf out "%a@\n" Sexp.pp_hum ev

let start_monitoring {Config.get=(!)} = 
  let out = match !Param.output with
    | None -> std_formatter
    | Some name -> formatter_of_out_channel (Out_channel.create name) in
  let module Monitor(Machine : Primus.Machine.S) = struct 
    let init () =
      parse_monitors !Param.monitors |>
      List.iter ~f:(fun m ->
          info "monitoring %s" (Primus.Observation.Provider.name m);
          Stream.observe (Primus.Observation.Provider.data m)
            (print_event out));
      Machine.return ()
  end in
  Primus.Machine.add_component (module Monitor)


let () = Config.when_ready start_monitoring
