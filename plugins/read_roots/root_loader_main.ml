open Core_kernel.Std
open Bap.Std
open Bap_future.Std

include Self ()

let extract filename arch =
  let width = Arch.addr_size arch |> Size.in_bits in
  In_channel.with_file filename ~f:(fun ch ->
      Sexp.input_sexps ch |> List.map ~f:Int64.t_of_sexp |>
      List.map ~f:(Addr.of_int64 ~width) |>
      Seq.of_list)

let register filename =
  Stream.map Project.Info.arch (fun arch ->
      Ok (Rooter.create (extract filename arch))) |>
  Rooter.Factory.register "file"

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Read roots information from a file and provide a rooter under
          the name $(b,file). So print $(b,--rooter=file) to use only this
          rooter.";
      `S "SEE ALSO";
      `P "$(b,bap-plugin-read-symbols)(1)";
    ] in
  let file = Config.(param (some non_dir_file) "from" ~docv:"SYMS"
                       ~doc:"Use this file as roots source") in
  Config.when_ready (fun {Config.get=(!)} ->
      match !file with
      | Some file -> register file
      | None -> () )
