open Core_kernel.Std
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

let%test_module "fileutils" = (module struct
  let%test "1" = parse_name "subs" = None
  let%test "2" = parse_name "subs.sexp" = Some ("sexp",None)
  let%test "3" = parse_name "subs.sexp-1.0" = Some ("sexp", Some "1.0")
  let%test "4" = parse_name "subs.local.sexp" = Some ("sexp",None)
  let%test "5" = parse_name "subs." = Some ("",None)
  let%test "6" = parse_name "subs-1.0" = None
  let%test "7" = parse_name "subs.sexp-" = Some ("sexp", Some "")
end)
