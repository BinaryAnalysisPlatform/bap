open Core_kernel.Std
open Or_error
module Seq = Sequence

type 'a or_error = 'a Or_error.t

let init_findlib = lazy (Findlib.init ())

let process_package_exn ~system name =
  let module Pkg = Fl_package_base in
  let pkg = Pkg.query name in
  let def = Pkg.(pkg.package_defs) in
  let dir = Findlib.package_directory name in
  let pre = ["plugin"; "native"] in
  let cmx = Findlib.package_property pre name "archive" in
  let sys = Fl_metascanner.lookup "plugin_system" [] def in
  if sys <> system then None
  else
    let file = Filename.chop_extension cmx ^ ".cmxs" in
    let path = Findlib.resolve_path ~base:dir file in
    Dynlink.loadfile path;
    Some (name, Ok ())

let process_package ~system name =
  try process_package_exn ~system name with
  | Not_found -> None
  | Dynlink.Error err ->
    Some (name, error_string (Dynlink.error_message err))
  | exn -> Some (name, of_exn exn)

let load ~system : (string * unit or_error) list =
  let lazy () = init_findlib in
  Fl_package_base.list_packages () |>
  List.map ~f:(process_package ~system) |>
  List.filter_opt
