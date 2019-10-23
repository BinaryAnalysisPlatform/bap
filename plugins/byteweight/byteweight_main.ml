open Core_kernel
open Bap.Std
open Format
open Bap_future.Std

include Self()

module BW = Bap_byteweight.Bytes
module Sigs = Bap_byteweight_signatures

let create_finder path length threshold arch comp =
  match Sigs.load ?comp ~path ~mode:"bytes" arch with
  | Error `No_signatures ->
    info "function starts signatures are not available";
    info "advice - use `bap-byteweight` to install signatures";
    info "advice - alternatively, use `opam install bap-signatures'";
    Or_error.errorf "signatures are unavailable"
  | Error (`Corrupted err) ->
    error "function starts signature file is corrupted: %s" err;
    info "advice - delete signatures at `%s'" path;
    info "advice - use `bap-byteweight` to install signatures";
    info "advice - alternatively, use `opam install bap-signatures'";
    Or_error.errorf "signatures are corrupted"
  | Error (`No_entry _) ->
    error "no signatures for the specified compiler and/or architecture";
    info "advice - try to use the default compiler entry";
    info "advice - create new entries using the `bap-byteweight' tool";
    Or_error.errorf "compiler is not supported by signatures"
  | Error (`Sys_error err) ->
    error "failed to load the signatures because of a system error: %s" err;
    Or_error.errorf "system error"
  | Ok data ->
    let bw = Binable.of_string (module BW) (Bytes.to_string data) in
    Ok (BW.find bw ~length ~threshold)

let main path length threshold comp =
  let finder arch = create_finder path length threshold arch comp in
  let find finder mem =
    Memmap.to_sequence mem |>
    Seq.fold ~init:Addr.Set.empty ~f:(fun roots (mem,_) ->
        Set.union roots @@ Addr.Set.of_list (finder mem)) in
  let find_roots arch mem = match finder arch with
    | Error _ as err ->
      warning "will not provide roots";
      err
    | Ok finder -> match find finder mem with
      | roots when Set.is_empty roots ->
        info "no roots were found";
        info "advice - check your signatures";
        Ok (Rooter.create Seq.empty)
      | roots -> Ok (roots |> Set.to_sequence |> Rooter.create)  in
  if Sys.file_exists path then
    let inputs = Stream.zip Project.Info.arch Project.Info.code in
    Stream.observe inputs (fun (arch,mem) ->
        match find_roots arch mem with
        | Ok roots -> Rooter.provide roots
        | Error _ -> ())
  else begin
    warning "the signature database is not available";
    info "advice - use `bap-byteweight` to install signatures";
    info "advice - alternatively, use `opam install bap-signatures'";
  end


let () =
  let () = Config.manpage [
      `S "DESCRIPTION";

      `P
        "This plugin identifies function starts, partially \
         implementing on the BYTEWEIGHT algorithm described in \
         [1]. Only the byte level matching is implemented. The $(b,SEE \
         ALSO) section contains links for other plugins, that provides \
         rooters";

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
  let sigsfile : string Config.param =
    let doc = "Path to the signature file. Not needed by default, \
               usually it is enough to run `bap-byteweight update'." in
    let default = Sigs.default_path in
    Config.(param non_dir_file ~default "sigs" ~doc) in
  let compiler : string option Config.param =
    let doc = "Assume the input file is compiled by $(docv)" in
    Config.(param (some string) "comp" ~doc ~docv:"COMPILER") in
  Config.when_ready (fun {Config.get=(!)} ->
      main !sigsfile !length !threshold !compiler)
