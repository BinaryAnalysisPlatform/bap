open Core_kernel.Std
open Bap.Std
open Cmdliner

include Self()

module BW = Bap_byteweight.Bytes
module Sigs = Bap_byteweight_signatures

let find_roots path length threshold arch mem : addr list  =
  match Sigs.load ?path ~mode:"bytes" arch with
  | None ->
    eprintf "No signatures found@.Please, use `bap-byteweight update' \
             to get the latest available signatures.@.%!";
    []
  | Some data ->
    let bw = Binable.of_string (module BW) data in
    BW.find bw ~length ~threshold mem

let main path length threshold =
  let find arch mem = find_roots path length threshold arch mem in
  let rooter xs =
    xs |> Seq.of_list |> Rooter.create |> Option.some in
  let of_mem (mem,arch) = rooter (find arch mem) in
  let of_image img =
    let arch = Image.arch img in
    Image.segments img |>
    Table.foldi ~init:[] ~f:(fun mem sec roots ->
        if Image.Segment.is_executable sec
        then find arch mem @ roots
        else roots) |> rooter in
  Rooter.Factory.register Source.Memory name of_mem;
  Rooter.Factory.register Source.Binary name of_image

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
