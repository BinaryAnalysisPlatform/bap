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
    info "the signature database is not available";
    info "advice - use `bap-byteweight` to install signatures";
    Or_error.errorf "no signatures"
  | Error (`Corrupted err) ->
    let path = Option.value path ~default:Sigs.default_path in
    error "the signature database is corrupted: %s" err;
    info "advice - delete signatures at `%s'" path;
    info "advice - use `bap-byteweight` to install new signatures";
    Or_error.errorf "corrupted database"
  | Error (`No_entry _err) ->
    error "no signatures for the specified compiler and architecture";
    info "advice - try to use the default compiler entry";
    info "advice - add support for your target using `bap-byteweight' tool";
    Or_error.errorf "no entry"
  | Error (`Sys_error err) ->
    error "signature loading was prevented by a system error: %s" err;
    Or_error.errorf "system error"
  | Ok data ->
    let bw = Binable.of_string (module BW) data in
    Ok (BW.find bw ~length ~threshold)

let byteweight = Service.(begin
    provide rooter "byteweight"
      ~desc:"discovers function starts using ML classifier" [
      required loader;
      parameter Config.input;
    ]
  end)

let main path length threshold =
  let finder arch = create_finder path length threshold arch in
  let find finder mem =
    Memmap.to_sequence mem |>
    Seq.fold ~init:Addr.Set.empty ~f:(fun roots (mem,_v) ->
        Set.union roots @@ Addr.Set.of_list (finder mem)) in
  let find_roots arch mem = match finder arch with
    | Error _ as err ->
      warning "unable to provide the rooter service";
      err
    | Ok finder -> match find finder mem with
      | roots when Set.is_empty roots ->
        info "no roots were found";
        info "advice - check your compiler's signatures";
        Ok (Rooter.create Seq.empty)
      | roots -> Ok (roots |> Set.to_sequence |> Rooter.create)  in
  let rooter =
    let open Project.Info in
    Stream.Variadic.(apply (args arch $ code) ~f:find_roots) in
  Rooter.Factory.register name rooter

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";

      `P
        "This plugin provides a rooter (function start identification)
       service using the BYTEWEIGHT algorithm described in [1].
       The algorithm uses machin learning techniques to classify bytes
       that belong to a function preamble from thos that don't.
       Currently the plugin   plugin operates on a byte level
       only. The $(b,SEE ALSO) section  contains links for other
       plugins, that provides rooters";

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
