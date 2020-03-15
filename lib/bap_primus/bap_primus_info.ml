open Bap_knowledge
open Core_kernel

module Name = Knowledge.Name

type t = {
  name : Name.t;
  desc : string;
  long : unit -> string;
}

let name x = x.name
let desc x = x.desc
let long x = x.long ()

let pp ppf {name; desc} =
  Format.fprintf ppf "@[<hov2>- %a:@\n%a@]@\n@\n"
    Name.pp name Format.pp_print_text desc

let unquote s =
  if String.is_prefix s ~prefix:{|"|} &&
     String.is_suffix s ~suffix:{|"|}
  then String.sub s ~pos:1 ~len:(String.length s - 2)
  else s

let dedup_whitespace str =
  let buf = Buffer.create (String.length str) in
  let push = Buffer.add_char buf in
  String.fold str ~init:`white ~f:(fun state c ->
      let ws = Char.is_whitespace c in
      if not ws then push c;
      match state,ws with
      | `white,true  -> `white
      | `white,false -> `black
      | `black,true  -> push c; `white
      | `black,false -> `black) |> ignore;
  Buffer.contents buf

let normalize_text s =
  dedup_whitespace (unquote (String.strip s))

let create
    ?(long=fun () -> "")
    ?(desc="")
    name = {
  name; long;
  desc = normalize_text desc;
}
