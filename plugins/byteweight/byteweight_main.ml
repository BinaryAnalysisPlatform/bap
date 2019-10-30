open Core_kernel
open Bap.Std
open Format
open Bap_future.Std

include Self()

module BW = Bap_byteweight.Bytes
module Sigs = Bap_byteweight_signatures
module Stats = Bap_byteweight.Stats

let p1 m n = float m /. float (m + n)
and p0 m n = float n /. float (m + n)


let create_finder path ~min_length ~max_length threshold arch comp =
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
    let sigs = Binable.of_string (module BW) (Bytes.to_string data) in
    Result.return @@
    if threshold >= 1.0
    then BW.find_using_bayes_factor sigs ~min_length ~max_length threshold
    else BW.find_using_threshold sigs ~min_length ~max_length threshold

let main path min_length max_length threshold comp =
  let finder arch = create_finder path threshold arch comp
      ~min_length ~max_length in
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
  Config.manpage [
    `S "DESCRIPTION";
    `P {|
  Identifies function starts using a predefined sets of function
  start signatures. Each signature is a sequence of bytes equipped with
  a sample probability of occuring it as a function start. The input
  memory is scanned, and for each byte that is not yet classified as a
  function start the longest sequence of bytes is searched in the
  signatures. If one is found, then the $(b,threshold) parameter defines
  the decision procedure. If it is a value below $(b,1.0) then the
  sequence of bytes will be classified as a function start if the
  the associated probability is higher than the specified threshold.
  If the threshold is greater or equal than 1.0, then the sequence of
  bytes will be classified as a function start if the Bayes factor of
  the two competing hypothesis is greater than the specified
  threshold. The Bayes factor is the ratio between the posterior
  probabilities of the competing hypothesis. Therefore, it includes
  the prior odds of finding a function start, which makes the
  hypothesis testing more robust. The Bayes factor value is having the
  following interpretations:
|};
    `Pre "
    Bayes Factor          Strength

    1 to 3.2              Weak
    3.2 to 10             Substantial
    10 to 100             Strong
    100 and greater       Decisive;
";


    `P "This plugin is a partial implementation of the  starts, partially
        BYTEWEIGHT algorithm as described in [1]. Only the byte level
        matching is implemented. The $(b,SEE ALSO) section contains
        links to other plugins, that provide function identification services.";

    `P "[1]: Bao, Tiffany, et al. \"Byteweight: Learning to recognize
    functions in binary code.\" 23rd USENIX Security Symposium (USENIX
    Security 14). 2014.";
    `S "SEE ALSO";
    `P "$(b,bap-byteweight)(1), $(b,bap-plugin-ida)(1), $(b,bap-plugin-read-symbols)(1)"
  ];
  let open Config in
  let min_length = param int ~default:8 "min-length"
      ~doc:"The minimum length of a word, that could identify a \
            function start. Any signatures that are below that \
            length, will not be considered, affect prior \
            probabilities, etc." in
  let max_length = param int ~default:16 "max-length"
      ~synonyms:["length"]
      ~doc:"The maximum length of a word, that could identify a \
            function start. Any signatures that are greater than that \
            length, will not be considered, affect prior \
            probabilities, etc." in
  let threshold = param float ~default:10. "threshold"
      ~doc:"If greater than 1.0 then it is the Bayes factor, \
            otherwise it is a probability." in
  let sigsfile = param non_dir_file ~default:Sigs.default_path "sigs"
      ~doc:"Path to the signature file" in
  let compiler = param (some string) "comp"
      ~doc:"Assume the input file is compiled by $(docv)" in
  Config.when_ready (fun {Config.get=(!)} ->
      main !sigsfile !min_length !max_length !threshold !compiler)
