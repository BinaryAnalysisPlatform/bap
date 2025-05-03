open Core_kernel[@@warning "-D"]

open Bap_plugins_units_intf

module Findlib = struct
  let name = "findlib"
  let units : reason String.Table.t = String.Table.create ()

  let has_meta_information pkg =
    try ignore (Findlib.package_meta_file pkg : string); true
    with Findlib.No_such_package _ -> false

  let report_missing_meta pkg =
    failwithf
      "Package %s was used to build the host program, but its meta \
       information is not available at runtime. To be able to use \
       plugins without installed META files, either update the version \
       of OCaml to 4.08 or newer or provide them at the compilation \
       time via the `used_MOD` predicates for each `MOD` linked into the
    host binary." pkg ()


  (* this function requires working Findlib infrastructure. *)
  let unit_of_package ~findlib_is_required pkg =
    if findlib_is_required && not (has_meta_information pkg)
    then report_missing_meta pkg;
    let preds = Findlib.recorded_predicates () in
    try
      Findlib.package_property preds pkg "archive" |>
      Filename.chop_extension
    with _ -> pkg


  let string_of_reason = function
    | `In_core -> "is core unit"
    | `Provided_by plugin -> "was provided by " ^ plugin
    | `Requested_by plugin -> "was loaded on request from " ^ plugin

  let extract_units_from_predicates () =
    Findlib.recorded_predicates () |> List.iter ~f:(fun pred ->
        match String.chop_prefix pred ~prefix:"used_" with
        | None -> ()
        | Some lib -> Hashtbl.set units ~key:lib ~data:`In_core)

  let extract_units_from_packages ~findlib_is_required =
    Findlib.(recorded_packages Record_core) |> List.iter ~f:(fun pkg ->
        Hashtbl.set units
          (unit_of_package ~findlib_is_required pkg)
          `In_core)

  let init () =
    if not (Hashtbl.is_empty units)
    then failwith "the plugin system is already initialized";
    extract_units_from_predicates ();
    extract_units_from_packages ~findlib_is_required:(Hashtbl.is_empty units)

  let record name reason =
    Hashtbl.add_exn units name reason

  let lookup = Hashtbl.find units

  let list () = Hashtbl.keys units

  let handle_error _ _ err = Or_error.error_string @@ Dynlink.error_message err
end

module Dynlink = struct
  let name = "dynlink"

  let units : reason String.Table.t = String.Table.create ()

  (* see https://github.com/ocaml/ocaml/issues/9338
     this ugly clutch could only be removed after we phase out
     OCaml 4.11, as the fix (https://github.com/ocaml/ocaml/pull/9790)
     was only merged in 4.12 *)
  let init_dynlink () =
    try Dynlink.loadfile "" with _ -> ()

  let copy_units_from_dynlink () =
    init_dynlink ();
    Dynlink.all_units () |>
    List.iter ~f:(fun unit -> Hashtbl.add_exn units unit `In_core)

  let init () = copy_units_from_dynlink ()
  let list () = Hashtbl.keys units
  let record name reason = match Hashtbl.add units name reason with
    | `Ok -> ()
    | `Duplicate ->
      failwithf "bap-plugins: internal error - \
                 the unit %s is already loaded" name ()

  let lookup = Hashtbl.find units
  let handle_error name reason = function
    | Dynlink.Module_already_loaded _ ->
      Hashtbl.set units name reason;
      Ok ()
    | other ->
      Or_error.error_string (Dynlink.error_message other)
end


let is_toplevel = Stdlib.Sys.interactive.contents

module Make() =
  (val if is_toplevel
    then (module Findlib : S)
    else (module Dynlink : S))
