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

let main = function
  | None -> ()
  | Some demangler ->
    Project.register_pass ~autorun:true (apply demangler)


let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Apply specified demangler to all subroutine names";
      `S "SEE ALSO";
      `P "$(b,bap-plugin-cxxfilt)(1)"
    ] in
  let demangler : demangler option Config.param =
    let demanglers =
      Demanglers.available () |>
      List.map ~f:(fun d -> Demangler.name d, d) in
    let doc = sprintf "Demangle all function names using $(docv),
      that should be %s" @@ Config.doc_enum demanglers in
    Config.(param (some (enum demanglers)) "with" ~docv:"DEMANGLER" ~doc) in
  Config.when_ready (fun {Config.get=(!)} ->
      Future.upon Plugins.loaded (fun () -> main !demangler))
