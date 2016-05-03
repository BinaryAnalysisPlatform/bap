open Core_kernel.Std
open Bap.Std
open Cmdliner
open Format
open Bap_future.Std

include Self()

module BW = Bap_byteweight.Bytes
module Sigs = Bap_byteweight_signatures

let create_finder path length threshold arch  =
  match Sigs.load ?path ~mode:"bytes" arch with
  | Error `No_signatures ->
    info "signature database is not available";
    info "advice - use `bap-byteweight` to install signatures";
    Or_error.errorf "no signatures"
  | Error (`Corrupted err) ->
    let path = Option.value path ~default:Sigs.default_path in
    error "signature database is corrupted: %s" err;
    info "advice - delete signatures at `%s'" path;
    info "advice - use `bap-byteweight` to install signatures";
    Or_error.errorf "corrupted database"
  | Error (`No_entry err) ->
    error "no signatures for specified compiler and architecture";
    info "advice - try to use default compiler entry";
    info "advice - create new entries with `bap-byteweight' tool";
    Or_error.errorf "no entry"
  | Error (`Sys_error err) ->
    error "signature loading was prevented by a system error: %s" err;
    Or_error.errorf "system error"
  | Ok data ->
    let bw = Binable.of_string (module BW) data in
    Ok (BW.find bw ~length ~threshold)

let main path length threshold =
  let finder arch = create_finder path length threshold arch in
  let find finder mem =
    Memmap.to_sequence mem |>
    Seq.fold ~init:Addr.Set.empty ~f:(fun roots (mem,v) ->
        Set.union roots @@ Addr.Set.of_list (finder mem)) in
  let find_roots arch mem = match finder arch with
    | Error _ as err ->
      warning "unable to provide rooter service";
      err
    | Ok finder -> match find finder mem with
      | roots when Set.is_empty roots ->
        info "no roots was found";
        info "advice - check your compiler's signatures";
        Ok (Rooter.create Seq.empty)
      | roots -> Ok (roots |> Set.to_sequence |> Rooter.create)  in
  let rooter =
    let open Project.Info in
    Stream.Variadic.(apply (args arch $ code) ~f:find_roots) in
  Rooter.Factory.register name rooter

module Cmdline = struct

  let length : int Term.t =
    let doc = "Maximum prefix length when byteweighting" in
    Arg.(value & opt int 16 & info ["length"] ~doc)

  let threshold : float Term.t =
    let doc = "Minimum score for the function start" in
    Arg.(value & opt float 0.9 & info ["threshold"] ~doc)

  let sigsfile : string option Term.t =
    let doc = "Path to the signature file. No needed by default, \
               usually it is enough to run `bap-byteweight update'." in
    Arg.(value & opt (some non_dir_file) None & info ["sigs"] ~doc)

  let t = Term.(const main $sigsfile $length $threshold)
end

let man = [
  `S "DESCRIPTION";
  `P "This plugin will provide a rooter service. The algorithm is
      described at [1].";
  `P "[1]: Bao, Tiffany, et al. \"Byteweight: Learning to recognize
    functions in binary code.\" 23rd USENIX Security Symposium (USENIX
    Security 14). 2014."
]

let info = Term.info name ~version ~doc ~man

let () =
  match Term.eval ~argv ~catch:false (Cmdline.t,info) with
  | `Ok () -> ()
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
