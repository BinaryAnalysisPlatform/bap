open Core_kernel

open Bap_plugins_units_intf

[%%if ocaml_version < (4,08,0)]
include Bap_plugins_units_fallback
[%%else]
let name = "dynlink"

let units : reason String.Table.t = String.Table.create ()

let copy_units_from_dynlink () =
  Dynlink.all_units () |>
  List.iter ~f:(fun unit -> Hashtbl.add_exn units unit `In_core)

let init () = copy_units_from_dynlink ()
let list () = Hashtbl.keys units
let record name reason = Hashtbl.add_exn units name reason
let lookup = Hashtbl.find units
let handle_error name reason = function
  | Dynlink.Module_already_loaded _ ->
    Hashtbl.set units name reason;
    Ok ()
  | other ->
    Or_error.error_string (Dynlink.error_message other)
[%%endif]
