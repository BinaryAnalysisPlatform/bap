open Core_kernel.Std
open Or_error

let load pkg =
  let cmd = sprintf "#require \"%s\";;" (Bap_plugin.name pkg) in
  Bap_toplevel.eval cmd

let () =
  Bap_plugins.all () |>
  List.iter ~f:(fun pkg -> match load pkg with
      | Ok true -> ()
      | Ok false -> assert false
      | Error err ->
        eprintf "failed to load plugin into toplevel: (%s)%s: %s"
          (Bap_plugin.system pkg)
          (Bap_plugin.name pkg)
          (Error.to_string_hum err))
