open Core_kernel
open Option.Monad_infix

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

let split_right ~after str =
  match String.rindex str after with
  | None -> str,None
  | Some n -> String.(subo ~len:n str, Some (subo ~pos:(n+1) str))

let extension s = split_right ~after:'.' s |> snd

let parse_name filename =
  let name,ver = split_right ~after:'-' filename in
  extension name >>| fun ext -> ext,ver
