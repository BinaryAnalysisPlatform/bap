open Core_kernel.Std
open Bap.Std
open Bap_demangle.Std
open Bap_future.Std
open Bap_plugins.Std
include Self()


let apply demangler proj =
  let prog = Project.program proj in
  Term.map sub_t prog ~f:(fun sub ->
      Sub.with_name sub (Demangler.run demangler (Sub.name sub))) |>
  Project.with_program proj

let find_demangler name =
  Demanglers.available () |>
  List.find ~f:(fun d -> Demangler.name d = name)

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Apply specified demangler to all subroutine names";
      `S "SEE ALSO";
      `P "$(b,bap-plugin-cxxfilt)(1)"
    ] in
  let demangler : string option Config.param =
    let doc = sprintf "Demangle all function names using $(docv)" in
    Config.(param (some string)) "with" ~docv:"DEMANGLER" ~doc in
  Config.when_ready (fun {Config.get=(!)} ->
      Future.upon Plugins.loaded (fun () ->
          Option.iter !demangler (fun demangler ->
              match find_demangler demangler with
              | Some d -> Project.register_pass ~autorun:true (apply d)
              | None ->
                let names =
                  Demanglers.available () |>
                  List.map ~f:Demangler.name |>
                  String.concat ~sep:", " in
                invalid_argf "Didn't find demangler %s, should be one of: %s\n"
                  demangler names ())))
