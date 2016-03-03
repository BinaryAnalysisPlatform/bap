open Core_kernel.Std
open Word_size
open Or_error
open Binary_packing

open Bap.Std
open Dwarf_types
module Leb128 = Dwarf_leb128

type 'a reader = string -> pos_ref : int ref -> 'a Or_error.t

let read_cstring src ~pos_ref : string t =
  match String.index_from src !pos_ref '\x00' with
  | None -> Or_error.errorf "String was not null-termintated"
  | Some pos ->  (* assuming that String module can't return bad pos *)
    let len = pos - !pos_ref in
    let dst = String.create len in
    String.blit ~src ~src_pos:!pos_ref ~dst ~dst_pos:0 ~len;
    pos_ref := (pos + 1); (* move over the null byte  *)
    return dst

let%test_module "read_cstring" = (module struct
  let str = "hello\000,\000world\000!\000"
  let pos_ref = ref 0
  let%test "hello" = read_cstring str ~pos_ref = Ok "hello"
  let%test "comma" = read_cstring str ~pos_ref = Ok ","
  let%test "world" = read_cstring str ~pos_ref = Ok "world"
  let%test "emark" = read_cstring str ~pos_ref = Ok "!"
  let%test "full"  = String.length str = pos_ref.contents
end)

let read_leb128 inj str ~pos_ref =
  Leb128.read ~signed:false str ~pos_ref >>=
  Leb128.to_int >>= inj

let tag_of_int = function
  | 0x03 -> return Tag.entry_point
  | 0x11 -> return Tag.compile_unit
  | 0x1d -> return Tag.inlined_subroutine
  | 0x2e -> return Tag.subprogram
  | n    -> return @@ Tag.unknown n

let read_word_exn reader size buf ~pos_ref =
  let word = reader ~buf ~pos:!pos_ref in
  pos_ref := size + !pos_ref;
  word

let read_word reader ~size str ~pos_ref =
  try_with (fun () -> read_word_exn reader size str ~pos_ref)

let read_as inj reader ~size str ~pos_ref =
  read_word reader ~size str ~pos_ref >>= inj


let ok_some inj v =
  let error = Error.of_string "overflow" in
  Result.of_option ~error (inj v)

let ok_any inj v = Ok (inj v)

type 'a inj = {
  of_int64 : int64 -> 'a Or_error.t;
  of_int32 : int32 -> 'a Or_error.t;
}

let int = {
  of_int64 = ok_some Int64.to_int;
  of_int32 = ok_some Int32.to_int
}

let int32 = {
  of_int64 = ok_some Int64.to_int32;
  of_int32 = ok_any ident
}

let int64 = {
  of_int64 = ok_any ident;
  of_int32 = ok_any Int64.of_int32;
}


let addr = {
  of_int64 = ok_any Addr.of_int64;
  of_int32 = ok_any Addr.of_int32;
}

let of_endian = function
  | LittleEndian -> `Little_endian
  | BigEndian -> `Big_endian

(** [make_reader inj endian size] will read a word of [size], with
    byte order specified with [ending] and then it will try to inject
    it into ['a] type using [inj] function. *)
let make_reader (inj : 'a inj) endian size : 'a reader =
  let byte_order = of_endian endian in
  let get_int64 = unpack_signed_64 ~byte_order in
  let get_int32 = unpack_signed_32 ~byte_order in
  match size with
  | W64 -> read_as inj.of_int64 get_int64 ~size:8
  | W32 -> read_as inj.of_int32 get_int32 ~size:4

let read_int16 endian : int reader =
  let byte_order = of_endian endian in
  read_word (unpack_signed_16 ~byte_order) ~size:2



(* right now we do not need a form, but form some attributes
   reading and representation do depend on form. *)
let attr_of_int = function
  | 0x03 -> return Attr.name
  | 0x11 -> return Attr.low_pc
  | 0x12 -> return Attr.high_pc
  | 0x52 -> return Attr.entry_pc
  | n    -> return @@ Attr.unknown n

let form_of_int = function
  | 0x01 -> return @@ Form.addr
  | 0x03 -> return @@ Form.block Two
  | 0x04 -> return @@ Form.block Four
  | 0x05 -> return @@ Form.const Two
  | 0x06 -> return @@ Form.const Four
  | 0x07 -> return @@ Form.const Eight
  | 0x08 -> return @@ Form.string
  | 0x09 -> return @@ Form.block Leb128
  | 0x0a -> return @@ Form.block One
  | 0x0b -> return @@ Form.const One
  | 0x0c -> return @@ Form.const One
  | 0x0d -> return @@ Form.const Leb128  (* fixme: signed!     *)
  | 0x0e -> return @@ Form.strp          (* not supported, yet *)
  | 0x0f -> return @@ Form.const Leb128
  | 0x10 -> return @@ Form.offset
  | 0x11 -> return @@ Form.ref One
  | 0x12 -> return @@ Form.ref Two
  | 0x13 -> return @@ Form.ref Four
  | 0x14 -> return @@ Form.ref Eight
  | 0x15 -> return @@ Form.ref Leb128
  | 0x16 -> return @@ Form.indirect
  | 0x17 -> return @@ Form.offset
  | 0x18 -> return @@ Form.expr
  | 0x19 -> return @@ Form.flag_present
  | 0x20 -> return @@ Form.ref Eight
  | n    -> errorf "unknown form code: %d" n

let tag  = read_leb128 tag_of_int
let attr = read_leb128 attr_of_int
let form = read_leb128 form_of_int

let read_addr  = make_reader addr
let read_int   = make_reader int
let read_int32 = make_reader int32
let read_int64 = make_reader int64

let take read str ~pos_ref = read str ~pos_ref >>| Option.some
let drop read str ~pos_ref = read str ~pos_ref >>| fun _ -> None
let ignore read str ~pos_ref = read str ~pos_ref >>| fun _ -> ()
let map read ~f =
  fun str ~pos_ref ->
    read str ~pos_ref >>| fun x -> f x

let pair read1 read2 str ~pos_ref =
  read1 str ~pos_ref >>= fun w1 ->
  read2 str ~pos_ref >>= fun w2 ->
  return (w1,w2)

let unit_size endian str ~pos_ref =
  read_int32 endian W32 str ~pos_ref >>= function
  | 0xffffffffl -> read_int endian W64 str ~pos_ref >>| fun n -> (W64,n)
  | addr -> ok_some Int32.to_int addr >>| fun n -> (W32,n)

let offset = read_int

let char : int reader =
  read_word unpack_unsigned_8 ~size:1

let skip ~bytes str ~pos_ref =
  let rec loop n =
    if n > 0
    then char str ~pos_ref >>= fun (_:int) -> loop (n-1)
    else return () in
  loop bytes

let skip_zeros str ~pos_ref =
  let rec loop () =
    char str ~pos_ref >>= function
    | 0 -> loop ()
    | n -> pos_ref := !pos_ref - 1;
      return () in
  loop ()


let address_size str ~pos_ref =
  char str ~pos_ref >>= function
  | 4 -> return W32
  | 8 -> return W64
  | n -> errorf "unsupported address size: %d" n

(* yes it is weird, but I just too lazy, maybe later ~ivg  *)
let version str ~pos_ref =
  pair char char str ~pos_ref >>| function
  | (0,n) | (n,0) -> n
  | (_,n) -> n

let string = read_cstring
let code = read_leb128 (fun x -> Ok x)
let address = read_addr


let read_big_leb128 str ~pos_ref =
  Leb128.read ~signed:false str ~pos_ref >>=
  Leb128.to_int64



let const lenspec : endian -> int64 reader =
  match lenspec with
  | Leb128 -> fun (_ : endian) -> read_big_leb128
  | One -> fun (_ : endian) -> map ~f:Int64.of_int char
  | Two -> fun (e) -> map ~f:Int64.of_int (read_int16 e)
  | Four ->  fun endian -> read_int64 endian W32
  | Eight -> fun endian -> read_int64 endian W64


let block lenspec endian src ~pos_ref =
  const lenspec endian src ~pos_ref >>= fun len ->
  Result.of_option
    ~error:(Error.create "block size is too big" len sexp_of_int64)
    (Int64.to_int len) >>= fun len ->
  Int.validate_bound len ~min:(Excl 0) ~max:(Incl (String.max_length))
  |> Validate.result >>= fun () ->
  let dst = String.create len in
  String.blit ~src ~src_pos:!pos_ref ~dst ~dst_pos:0 ~len;
  pos_ref := !pos_ref + len;
  return dst
