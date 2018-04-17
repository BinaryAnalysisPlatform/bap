open Core_kernel.Std
open Bap.Std
open Format
open Bap_future.Std
open Regular.Std
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
  | Error (`No_entry _err) ->
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

let byteweight =
  Bap_service.Provider.declare "byteweight"
    ~desc:"Provides a rooter based on byteweight algorithm"
    Rooter.service

let () =
  let open Bap_service in
  let digest = Data.Cache.Digest.to_string @@
    Data.Cache.digest ~namespace:"byteweight" "rooter" in
  Product.provide ~digest byteweight

let main path length threshold =
  let finder arch = create_finder path length threshold arch in
  let find finder mem =
    Memmap.to_sequence mem |>
    Seq.fold ~init:Addr.Set.empty ~f:(fun roots (mem,_v) ->
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
  Rooter.Factory.provide byteweight rooter

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";

      `P

        "This plugin provides a rooter (function start identification)
       service using the BYTEWEIGHT algorithm described in [1]. The
       plugin operates on a byte level. The $(b,SEE ALSO) section
       contains links for other plugins, that provides rooters";


      `P "[1]: Bao, Tiffany, et al. \"Byteweight: Learning to recognize
    functions in binary code.\" 23rd USENIX Security Symposium (USENIX
    Security 14). 2014.";
      `S "SEE ALSO";
      `P "$(b,bap-byteweight)(1), $(b,bap-plugin-ida)(1), $(b,bap-plugin-read-symbols)(1)"
    ] in
  let length : int Config.param =
    let doc = "Maximum prefix length when byteweighting" in
    Config.(param int ~default:16 "length" ~doc) in
  let threshold : float Config.param =
    let doc = "Minimum score for the function start" in
    Config.(param float ~default:0.9 "threshold" ~doc) in
  let sigsfile : string option Config.param =
    let doc = "Path to the signature file. No needed by default, \
               usually it is enough to run `bap-byteweight update'." in
    Config.(param (some non_dir_file) "sigs" ~doc) in
  Config.when_ready (fun {Config.get=(!)} -> main !sigsfile !length !threshold)
