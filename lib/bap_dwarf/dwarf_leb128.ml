(** The implementation can work with numbers of arbitrary length. *)
open Core_kernel.Std

module Bits = struct
  open Binary_packing
  let get_uint8  = unpack_unsigned_8
  let get_uint16 = unpack_unsigned_16_little_endian
  let get_int32 = unpack_signed_32 ~byte_order:`Little_endian
  let get_int64 = unpack_signed_64_little_endian
  let set_int8 = pack_unsigned_8
  let set_int16 = pack_signed_16_little_endian
  let set_int32 = pack_signed_32 ~byte_order:`Little_endian
  let set_int64 = pack_signed_64_little_endian
end

type t = {
  negative : bool;
  data : int list;
} with sexp,bin_io,compare

type 'a encoder = ?signed:bool -> 'a -> t
type 'a decoder = t -> 'a Or_error.t


type cursor = {
  bit  : int;
  pos  : int;
  len  : int;
}

module Cursor = struct
  type t = cursor

  let init n : t = {bit = 0; pos = 0; len = n * 8}

  let next  cur : t =
    let bit = cur.bit + 7 in
    let m = min (cur.len - 1) (bit + 7) in
    let pos = if m < cur.pos * 8 + 16 then cur.pos else cur.pos + 1 in
    { cur with
      pos = min (cur.len / 8 - 2) pos;
      bit;
  }

  let bit0 cur : int = cur.bit - cur.pos * 8

  let print cur =
    let bit0 = bit0 cur in
    printf "bits [%2d-%2d] ([%2d-%2d]@%d)\n"
      cur.bit (cur.bit+6) bit0 (bit0+6) cur.pos
end

(** [pack7] returns a sequence of 7-bit packs  *)
let pack7 ~negative (s : string) =
  let n = String.length s in
  Sequence.unfold ~init:(Cursor.init n) ~f:(fun cur ->
      let bit0 = Cursor.bit0 cur in
      if cur.bit >= n * 8 then None
      else
        let word = Bits.get_uint16 s cur.pos in
        let mask = 0b0111_1111 lsl bit0 in
        let word = (word land mask) lsr bit0 in
        let word =
          if negative && (Cursor.next cur).bit >= n * 8
          then (word lor (0b0111_1111 land (lnot 0 lsl (n*8 - cur.bit))))
          else word in
        Some (word, Cursor.next cur))

(** accepts a sequence of bytes (in little endian) and performs the
    last step of conversion to LEB128 format, i.e., sets MSB to 1 for
    all bytes except the last one and drops the tail  *)
let fix ~negative (s : int Sequence.t) : t = {
  negative;
  data = List.fold ~init:[] (Sequence.to_list_rev s)
      ~f:(fun acc x -> match acc,x with
          | [],0   when not negative -> []
          | [],(127) when negative   -> []
          | [],x -> [x]
          | xs,x -> x lor (1 lsl 7) :: xs)
}

(** [decode_unsigned leb] transforms a number in a LEB128
    representation to a binary form *)
let decode (leb : t) buf ~off ~len : unit =
  let m = List.length leb.data in
  let (_ : Cursor.t) = List.foldi leb.data ~init:(Cursor.init len)
      ~f:(fun i cur x ->
          let bit0 = Cursor.bit0 cur in
          let word = Bits.get_uint16 buf cur.pos in
          let word = word lor ((x land 0b0111_1111) lsl bit0) in
          let word =
            if leb.negative && i = m - 1
            then word lor (lnot 0 lsl (bit0 + 7)) else word in
          Bits.set_int16 buf cur.pos word;
          Cursor.next cur) in
  if leb.negative then begin
    for i = m to len - 1 do
      buf.[i] <- '\255';
    done;
  end


let encode ~negative bits : t =
  fix ~negative (pack7 ~negative bits)


let read_exn ?(signed=false) bits ~pos_ref =
  let s = Sequence.unfold ~init:(`Continue) ~f:(function
      | `Stop -> None
      | `Continue ->
        let x = Bits.get_uint8 bits !pos_ref  in
        incr pos_ref;
        if x land (1 lsl 7) = 0 then Some (x,`Stop)
        else Some (x, `Continue)) in
  let data = Sequence.to_list_rev s in
  let negative = match data with
    | x::_ -> signed && x land (1 lsl 6) <> 0
    | [] -> false in
  {negative; data = List.rev data}

let read ?signed bits ~pos_ref =
  Or_error.try_with ~backtrace:true
    (fun () -> read_exn ?signed bits ~pos_ref)

let size t = max 1 @@ List.length t.data

let write t bits ~pos : unit =
  List.iteri t.data ~f:(fun i x ->
      Bits.set_int8 bits (pos + i) x)

type 'a repr = {
  size : int;
  read : string -> 'a option;
  is_negative: 'a -> bool;
  write: string -> 'a -> unit;
}

let int =
  let open Word_size in
  let open Bits in
  let safe cast x =
    Option.value_exn ~message:"cast failed"
      (cast x) in
  let read,write = match word_size with
    | W32 ->
      (fun buf -> Int32.to_int (get_int32 ~buf ~pos:0)),
      (fun buf x -> set_int32 ~buf ~pos:0 @@ safe Int32.of_int x)
    | W64 ->
      (fun buf -> Int64.to_int (get_int64 buf 0)),
      (fun buf x -> set_int64 ~buf ~pos:0 @@ Int64.of_int x) in
  {
    size = num_bits word_size / 8;
    read;
    is_negative = Int.is_negative;
    write;
  }

let int32 = {
  size = 4;
  read = (fun buf -> Some (Bits.get_int32 buf 0));
  is_negative = Int32.is_negative;
  write = (fun buf x -> Bits.set_int32 buf 0 x);
}

let int64 = {
  size = 8;
  read = (fun buf -> Some (Bits.get_int64 buf 0));
  is_negative = Int64.is_negative;
  write = (fun buf x -> Bits.set_int64 buf 0 x)
}

let decoder (repr : 'a repr) x : 'a Or_error.t =
  let bits = String.make repr.size '\000' in
  decode x bits ~off:0 ~len:repr.size;
  match
    if String.length bits > repr.size then None
    else repr.read bits
  with
  | Some n -> Ok n
  | None -> Or_error.errorf "number doesn't fit"

let encoder (repr : 'a repr) ?(signed=false) x : t =
  let bits = String.make repr.size '\000' in
  repr.write bits x;
  encode ~negative:(signed && repr.is_negative x) bits


let to_int   : int   decoder = decoder int
let to_int32 : int32 decoder = decoder int32
let to_int64 : int64 decoder = decoder int64

let of_int   : int   encoder = encoder int
let of_int32 : int32 encoder = encoder int32
let of_int64 : int64 encoder = encoder int64
