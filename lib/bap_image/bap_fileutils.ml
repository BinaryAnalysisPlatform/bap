open Core_kernel
open Option.Monad_infix

let mapfile path : Bigstring.t option =
  let fd = Unix.(openfile path [O_RDONLY] 0o400) in
  try
    let data =
      Mmap.V1.map_file
        fd Bigarray.char Bigarray.c_layout false [|-1|] in
    Unix.close fd;
    Some (Bigarray.array1_of_genarray data)
  with exn ->
    Unix.close fd;
    None
[@@warning "-D"]

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
