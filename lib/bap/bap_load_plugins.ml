open Core_kernel.Std
open Bap_plugins.Std
open Or_error

let load pkg =
  let cmd = sprintf "#require \"%s\";;" (Plugin.name pkg) in
  Bap_toplevel.eval cmd

let () =
  Plugins.all () |>
  List.iter ~f:(fun pkg -> match load pkg with
      | Ok true -> ()
      | Ok false -> assert false
      | Error err ->
        eprintf "failed to load plugin into toplevel: (%s)%s: %s"
          (Plugin.system pkg)
          (Plugin.name pkg)
          (Error.to_string_hum err))
