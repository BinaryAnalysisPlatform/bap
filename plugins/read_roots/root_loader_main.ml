open Core_kernel.Std
open Bap.Std
open Bap_future.Std

include Self ()

let extract_with_fmt arch filename fmt ver =
  let bits = Arch.addr_size arch |> Size.in_bits in
  In_channel.with_file filename
    ~f:(fun ch ->
        Or_error.try_with (fun () ->
            let addrs = Addr.Io.load_all ~fmt ?ver ch in
            match List.find addrs ~f:(fun x -> Addr.bitwidth x <> bits) with
            | None -> Seq.of_list addrs
            | Some a ->
              failwith
                (sprintf
                  "address %s can't be used with %d bit arch"
                  (Addr.to_string a) bits)))

let root_of_string bits s =
  let mk_error reason =
    let ers = sprintf "Can't read int64 from %s: %s" s reason in
    Error (Error.of_string ers) in
  try
    let x = Int64.of_string s in
    let y = Word.of_int64 ~width:bits x in
    let x' = Word.to_int64_exn y in
    if Int64.(x <> x') then
      mk_error "value doesn't match to the arch bitwidth"
    else Ok y
  with exn -> mk_error (Exn.to_string exn)

let extract_default filename arch =
  let bits = Arch.addr_size arch |> Size.in_bits in
  In_channel.with_file filename ~f:(fun ch ->
      Sexp.input_sexps ch |>
      List.map ~f:(fun s -> Sexp.to_string s |> root_of_string bits) |>
      Result.all |>
      Result.map ~f:Seq.of_list)

let split_right ~after str =
  match String.rindex str after with
  | None -> str,None
  | Some n -> String.(subo ~len:n str, Some (subo ~pos:(n+1) str))

let extension s = split_right ~after:'.' s |> snd

let rooter filename arch =
  let name,ver = split_right ~after:'-' filename in
  match extension name with
  | None -> extract_default filename arch
  | Some fmt ->
    let fmts =
      Addr.available_readers () |> List.map ~f:(fun (x,_,_) -> x) in
    if List.mem fmts ~equal:String.equal fmt then
      extract_with_fmt arch filename fmt ver
    else extract_default filename arch

let register filename =
  let name = sprintf "file:%s" filename in
  Stream.map Project.Info.arch (fun arch ->
      match rooter filename arch with
      | Ok s -> Ok (Rooter.create s)
      | Error e -> Error e) |>
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
