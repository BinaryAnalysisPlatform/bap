open Core_kernel

let name = "dynlink"

type reason = [
  | `In_core
  | `Provided_by of string
  | `Requested_by of string
]

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
