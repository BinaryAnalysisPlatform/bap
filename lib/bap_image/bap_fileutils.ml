open Core_kernel.Std
open Bap_types.Std

let mapfile path : Bigstring.t option =
  let fd = Unix.(openfile path [O_RDONLY] 0o400) in
  try
    let size = Unix.((fstat fd).st_size) in
    let data = Bigstring.map_file ~shared:false fd size in
    Unix.close fd;
    Some data
  with exn ->
    Unix.close fd;
    None

let readfile path : Bigstring.t =
  match mapfile path with
  | Some data -> data
  | None -> Bigstring.of_string (In_channel.read_all path)
