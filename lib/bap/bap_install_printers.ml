open Core_kernel.Std
open Bap.Std
open Or_error

let install_printers () =
  List.map (Pretty_printer.all ()) ~f:(fun printer ->
      let printer = String.substr_replace_first printer
          ~pattern:"Core"
          ~with_:"Core_kernel" in
      let cmd = sprintf "#install_printer %s;;" printer in
      Bap_toplevel.eval cmd) |>
  Or_error.all >>| List.for_all ~f:ident

let () = match install_printers () with
  | Ok true -> ()
  | Ok false -> eprintf "Problem installing printers"
  | Error err -> eprintf "Failed to install printers: %s" @@
    Error.to_string_hum err
