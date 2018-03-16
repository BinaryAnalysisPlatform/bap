open Core_kernel.Std
open Bap.Std
open Bap_future.Std

include Self ()

let extract_with_fmt filename fmt ver =
  In_channel.with_file filename
    ~f:(fun ch -> Addr.Io.load_all ~fmt ?ver ch) |> Seq.of_list

let extract_default filename arch =
  let width = Arch.addr_size arch |> Size.in_bits in
  In_channel.with_file filename ~f:(fun ch ->
      Sexp.input_sexps ch |> List.map ~f:Int64.t_of_sexp |>
      List.map ~f:(Addr.of_int64 ~width) |>
      Seq.of_list)

let split_right ~after str =
  match String.rindex str after with
  | None -> str,None
  | Some n -> String.(subo ~len:n str, Some (subo ~pos:(n+1) str))

let extension s = split_right ~after:'.' s |> snd

let rooter filename arch =
  let name,ver = split_right ~after:'-' filename in
  match extension name with
  | None -> extract_default filename arch
  | Some fmt -> extract_with_fmt filename fmt ver

let register filename =
  let name = sprintf "file:%s" filename in
  Stream.map Project.Info.arch (fun arch ->
      Ok (Rooter.create (rooter filename arch))) |>
    Rooter.Factory.register name

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Read roots information from a file and provide a rooter.
          The name of registered rooter is a concatenation of
          \"file:\" and a filename with roots. So once roots are read,
          use $(b,--rooter)=$(b,file:filename) to use them.
          Default file format is just a list of numbers. One can use
          a common convention to specify other format, see $(bap --help)
          for details.";
      `S "SEE ALSO";
      `P "$(b,bap-plugin-read-symbols)(1), $(b,bap)";
    ] in
  let file = Config.(param (some non_dir_file) "from" ~docv:"ROOTS"
                       ~doc:"Use this file as roots source") in
  Config.when_ready (fun {Config.get=(!)} ->
      match !file with
      | Some file -> register file
      | None -> () )
