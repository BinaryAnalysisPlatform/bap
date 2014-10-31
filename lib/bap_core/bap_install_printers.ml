open Core_kernel.Std
open Bap_core.Std
open Or_error

let eval_exn str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase false Format.err_formatter phrase

let eval str = try_with (fun () -> eval_exn str)

let install_printers () =
  List.map (Pretty_printer.all ()) ~f:(fun printer ->
      let printer = String.substr_replace_first printer
          ~pattern:"Core"
          ~with_:"Core_kernel" in
      let cmd = sprintf "#install_printer %s;;" printer in
      eval cmd) |>
  Or_error.all >>| List.for_all ~f:ident

let () = match install_printers () with
  | Ok true -> ()
  | Ok false -> eprintf "Problem installing printers"
  | Error err -> eprintf "Failed to install printers: %s" @@
    Error.to_string_hum err
