open Core_kernel
open Regular_data_types

type bytes = Regular_bytes.t

type 'a t = {
  of_channel : In_channel.t -> 'a;
  of_bigstring : bigstring -> 'a ;
  of_bytes : bytes -> 'a;
} [@@deriving fields]

let not_sufficient () =
  invalid_arg "Readable class definition is not complete"

let bytes_of_string = Bytes.of_string

module Bytes = struct
  let of_lexbuf  f s = f (Lexing.from_string (Bytes.to_string s))
  let of_scanbuf f s = f (Scanf.Scanning.from_string (Bytes.to_string s))
  let of_bigstring f s = f (Bigstring.of_bytes s)
end

module Channel = struct
  let of_bytes f c = In_channel.input_all c |> bytes_of_string |> f
  let of_scanbuf f c = f (Scanf.Scanning.from_channel c)
  let of_lexbuf f c = f (Lexing.from_channel c)
end

let create
    ?of_channel:fc
    ?of_lexbuf:fl
    ?of_scanbuf:fs
    ?of_bigstring:fB
    ?of_bytes:fb () =
  let of_bytes = match fb,fs,fl,fB with
    | None,None,None,None -> not_sufficient ()
    | Some f,_,_,_ -> f
    | _,Some f,_,_ -> Bytes.of_scanbuf f
    | _,_,Some f,_ -> Bytes.of_lexbuf  f
    | _,_,_,Some f -> Bytes.of_bigstring f in
  let of_channel = match fc,fs,fl with
    | None,None,None -> Channel.of_bytes of_bytes
    | Some f,_,_ -> f
    | _,Some f,_ -> Channel.of_scanbuf f
    | _,_,Some f -> Channel.of_lexbuf f in
  let of_bigstring = match fB with
    | None -> fun b -> of_bytes (Bigstring.to_bytes b)
    | Some f -> f in
  {of_bytes; of_bigstring; of_channel}
