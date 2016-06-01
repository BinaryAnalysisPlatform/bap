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


let () = Future.upon Plugins.loaded (fun () ->
    let open Cmdliner in
    let demangler : demangler option Term.t =
      let demanglers =
        Demanglers.available () |>
        List.map ~f:(fun d -> Demangler.name d, d) in
      let doc = sprintf "Demangle all function names using $(docv),
        that should be %s" @@
        Arg.doc_alts_enum demanglers in
      Arg.(value & opt (some (enum demanglers)) None &
           info ~doc ~docv:"DEMANGLER" ["with"]) in
    let man = [
      `S "DESCRIPTION";
      `P "Apply specified demangler to all subroutine names";
    ] in
    let info = Term.info name ~version ~doc ~man in
    let args = Term.(const main $demangler),info in
    match Term.eval ~argv args with
    | `Ok () -> ()
    | `Help | `Version -> exit 0
    | `Error _ -> exit 1)
