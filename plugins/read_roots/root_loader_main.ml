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

let parse_ver fmt =
  match String.split ~on:'-' fmt with
  | [fmt;ver] -> fmt, Some ver
  | _ -> fmt,None

let extract_format filename =
  match String.split ~on:'.' filename with
  | [] | [_] -> None, None
  | [_ ; fmt] -> Some fmt, None
  | _ :: strs ->
     let of_prefix (r,_,_) =
       List.exists strs ~f:(String.is_prefix ~prefix:r) in
     match Addr.available_readers () |> List.find ~f:of_prefix with
     | None -> None, None
     | Some (fmt,_,_) ->
        let pos =
          String.substr_index_exn filename ~pattern:("." ^ fmt) + 1 in
        let fmt,ver = parse_ver (String.subo ~pos filename) in
        Some fmt, ver

let extract filename arch =
  match extract_format filename with
  | None, _ -> extract_default filename arch
  | Some fmt, ver -> extract_with_fmt filename fmt ver

let register filename =
  let name = sprintf "file:%s" filename in
  Stream.map Project.Info.arch (fun arch ->
      Ok (Rooter.create (extract filename arch))) |>
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
