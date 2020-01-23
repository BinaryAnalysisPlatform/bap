open Bap_plugins_units_intf

open Core_kernel


let units : reason String.Table.t = String.Table.create ()

(* this function requires working Findlib infrastructure. *)
let unit_of_package ~findlib_is_required pkg =
  let preds = Findlib.recorded_predicates () in
  Format.eprintf "Resolving package: %s@\n%!" pkg;
  try
    Findlib.package_property preds pkg "archive" |>
    Filename.chop_extension
  with exn ->
    Format.eprintf "Skipping package: %s\n%!" pkg;
    pkg


let string_of_reason = function
  | `In_core -> "is core unit"
  | `Provided_by plugin -> "was provided by " ^ plugin
  | `Requested_by plugin -> "was loaded on request from " ^ plugin

let extract_units_from_predicates () =
  Findlib.recorded_predicates () |> List.iter ~f:(fun pred ->
      match String.chop_prefix pred ~prefix:"used_" with
      | None -> ()
      | Some lib -> Hashtbl.set units ~key:lib ~data:`In_core)


let init () =
  if not (Hashtbl.is_empty units)
  then failwith "the plugin system is already initialized";
  extract_units_from_predicates ();
  let findlib_is_required = Hashtbl.is_empty units in
  Findlib.(recorded_packages Record_core) |> List.iter ~f:(fun pkg ->
      Hashtbl.set units
        (unit_of_package ~findlib_is_required pkg)
        `In_core)


let record name reason =
  Format.eprintf "recording %s as it %s@\n%!" name (string_of_reason reason);
  Hashtbl.add_exn units name reason

let lookup = Hashtbl.find units

let list () = Hashtbl.keys units
