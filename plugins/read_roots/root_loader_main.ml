open Core_kernel.Std
open Bap.Std
open Bap_future.Std

include Self ()

let extract filename arch =
  let width = Arch.addr_size arch |> Size.in_bits in
  let addr = Addr.of_int64 ~width in
  In_channel.with_file filename ~f:(fun ch ->
      Sexp.input_sexps ch |> List.map ~f:Int64.t_of_sexp |>
      List.map ~f:addr |>
      Seq.of_list)

let register filename =
  Stream.map Project.Info.arch (fun arch ->
      Ok (Rooter.create (extract filename arch))) |>
  Rooter.Factory.register "file"

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P
        "Read roots information from a file and provide a rooter under
     the name $(b,file). So print $(b,--rooter=file) to use only this
     rooter. Input file format is [<name>.<fmt>[-<ver>]]. If fmt is not
     specified the default reader will be used, otherwise the `<fmt>`
     part should match with some reader, registered for Addr module.
     The `<ver>` part is optional, and defines format version.
     Example: \"functions.sexp\" or \"functions.sexp-1.0.
     The example above will use sexp serializer to read data from file.";
      `S "SEE ALSO";
      `P "$(b,bap-plugin-objdump)(1), $(b,bap-plugin-byteweight)(1), $(b,bap-plugin-ida)(1)";
    ] in
  let file = Config.(param (some non_dir_file) "from" ~docv:"SYMS"
                       ~doc:"Use this file as roots source") in
  Config.when_ready (fun {Config.get=(!)} ->
      match !file with
      | Some file -> register file
      | None -> () )
