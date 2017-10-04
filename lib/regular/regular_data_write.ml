open Core_kernel.Std
open Regular_data_types

type bytes = Regular_bytes.t

type 'a t = {
  size : 'a -> int;
  copy : ('a,string) copy;
  blit : ('a,bigstring) copy;
  dump : Out_channel.t -> 'a -> unit;
  pp   : Format.formatter -> 'a -> unit;
  to_bytes : 'a -> bytes;
  to_bigstring : 'a -> bigstring;
} [@@deriving fields]

let not_sufficient () =
  invalid_arg "Writable class definition is not complete"

let string_to_string ~dst ~src dst_pos =
  String.blito ~src ~dst ~dst_pos ()

let string_to_bigstring ~dst ~src dst_pos =
  Bigstring.From_string.blito ~src ~dst ~dst_pos ()

let bigstring_to_string ~dst ~src dst_pos =
  Bigstring.To_string.blito ~src ~dst ~dst_pos ()

let bigstring_to_bigstring ~dst ~src dst_pos =
  Bigstring.blito ~dst ~src ~dst_pos ()

let copy_via_blit size blit ~dst x pos =
  let buf = Bigstring.create (size x) in
  blit buf x;
  bigstring_to_string ~dst ~src:buf pos

let blit_via_copy size copy ~dst x pos =
  let str = String.create (size x) in
  copy str x;
  string_to_bigstring ~dst ~src:str pos

let bytes_via_copy size copy x =
  let buf = Bytes.create (size x) in
  copy buf x;
  buf

let bigstring_via_blit size blit x =
  let buf = Bigstring.create (size x) in
  blit buf x;
  buf

let pp_bytes f x = Format.asprintf "%a" f x

let create
    ?to_bytes ?to_bigstring
    ?dump ?pp ?size
    ?blit_to_string:copy ?blit_to_bigstring:blit () =
  let to_bytes = match to_bytes,to_bigstring,pp with
    | Some f,_,_ -> Some f
    | None,Some f,_ -> Some (fun x -> Bigstring.to_string (f x))
    | None,None,Some f -> Some (pp_bytes f)
    | None,None,None -> None in
  let size = match size, to_bytes, to_bigstring with
    | None,None,None -> not_sufficient ()
    | Some f,_,_ -> f
    | _,Some f,_ -> fun x -> String.length (f x)
    | _,_,Some f -> fun x -> Bigstring.length (f x) in
  let copy = match copy,to_bytes,blit,to_bigstring with
    | None,None,None,None -> not_sufficient ()
    | Some f,_,_,_ -> f
    | _,Some f,_,_ -> fun dst x -> string_to_string ~dst ~src:(f x)
    | _,_,Some f,_ -> fun dst x -> copy_via_blit size f ~dst x
    | _,_,_,Some f -> fun dst x -> bigstring_to_string ~dst ~src:(f x) in
  let blit = match blit,to_bytes,to_bigstring with
    | Some f,_,_ -> f
    | _,Some f,_ -> fun dst x -> string_to_bigstring ~dst ~src:(f x)
    | _,_,Some f -> fun dst x -> bigstring_to_bigstring ~dst ~src:(f x)
    | _ -> fun dst x -> blit_via_copy size copy ~dst x in
  let to_bytes = match to_bytes with
    | Some f -> f
    | None -> bytes_via_copy size copy in
  let pp = match pp with
    | Some f -> f
    | None -> fun ppf x -> Format.fprintf ppf "%s" (to_bytes x) in
  let to_bigstring = match to_bigstring with
    | Some f -> f
    | None -> fun x -> Bigstring.of_string (to_bytes x) in
  let dump = match dump with
    | Some f -> f
    | None -> fun c x -> Out_channel.output_string c (to_bytes x) in
  {size; copy; blit; dump; pp; to_bytes; to_bigstring}


let blit_to_string = copy
let blit_to_bigstring = blit
let to_channel = dump
let to_formatter = pp
