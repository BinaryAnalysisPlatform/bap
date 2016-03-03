open Core_kernel.Std
open Bap.Std
open Or_error
open Format


let install_printer printer =
  Topdirs.dir_install_printer std_formatter
    (Longident.parse printer)

let install_printers () =
  List.iter (Pretty_printer.all ()) ~f:(fun printer ->
      let printer =
        if String.is_prefix printer ~prefix:"Core."
        then String.substr_replace_first printer
            ~pattern:"Core"
            ~with_:"Core_kernel"
        else printer in
      install_printer printer)

let () = install_printers ()
