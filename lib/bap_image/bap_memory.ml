open Core_kernel.Std
open Bap_types.Std
open Or_error
open Image_common

type 'a or_error = 'a Or_error.t

type 'a m = 'a

type getter = {
  safe : pos_ref : addr ref -> word or_error;
  fast : pos_ref :  int ref -> word;
}

module Getters = struct
  type getters = {
    int8  : getter;
    uint8 : getter;
    int16 : getter;
    uint16: getter;
    int32 : getter;
    int64 : getter;
  } with fields
end

open Getters

type t = {
  endian : endian;
  data: Bigstring.t;
  addr : addr;
  off  : int;
  size : int;
  get  : getters;
}

module Repr = struct
  type t = {
    endian  : endian;
    offset  : int;
    base    : addr;
    size    : int;
  } with sexp_of
end

let to_repr mem = {
  Repr.endian = mem.endian;
  Repr.offset = mem.off;
  Repr.base   = mem.addr;
  Repr.size   = mem.size;
}

let sexp_of_t mem = Repr.sexp_of_t (to_repr mem)

let endian t = t.endian

(** [create_getters endian addr offset size data] creates a getters
    class according to the specified parameters. All parameters will be
    encapsulated inside a closure (cf, [getter] type).
    [addr] is a virtual address of the byte located at the [offset]
    from the beginning of the [data] *)
let create_getters endian addr off size data  =
  let length = Bigstring.length data in
  (* this precondition should be checked before entry *)
  assert (off + size <= length);
  let make inj bytes read  =
    let fast ~pos_ref : word =
      let r = read data ~pos:!pos_ref in
      pos_ref := !pos_ref + bytes;
      inj r in

    let safe ~pos_ref : word or_error =
      Addr.Int_err.(!$(!pos_ref) - !$addr) >>= fun addr ->
      Addr.(to_int (addr ++ off)) >>= fun pos ->
      if pos < off then
        errorf "segfault: addr < min_addr" else
      if pos >= off + size then
        errorf "segfault: addr > max_addr" else
      if pos + bytes > off + size then
        errorf "segfault: addr + %d > max_addr\n" bytes
      else
        let () = pos_ref := Addr.(!pos_ref ++ bytes) in
        let pos_ref = ref pos in
        return (fast ~pos_ref) in
    {safe; fast} in
  let int n = make (Word.of_int ~width:(n*8)) n in
  let int32 = make Word.of_int32 in
  let int64 = make Word.of_int64 in
  let open Bigstring in
  if endian = BigEndian then {
    int8   = int   1 unsafe_get_int8;
    uint8  = int   1 unsafe_get_uint8;
    int16  = int   2 unsafe_get_int16_be;
    uint16 = int   2 unsafe_get_uint16_be;
    int32  = int32 4 unsafe_get_int32_t_be;
    int64  = int64 8 unsafe_get_int64_t_be;
  } else {
    int8   = int 1 unsafe_get_int8;
    uint8  = int 1 unsafe_get_uint8;
    int16  = int 2 unsafe_get_int16_le;
    uint16 = int 2 unsafe_get_uint16_le;
    int32  = int32 4 unsafe_get_int32_t_le;
    int64  = int64 8 unsafe_get_int64_t_le;
  }

let one_byte_getters data addr pos =
  let byte = Word.of_int ~width:8 in
  let make read  =
    let fast ~pos_ref =
      pos_ref := !pos_ref + 1;
      byte (read data ~pos)  in
    let safe ~pos_ref =
      if Addr.(pos_ref.contents <> addr)
      then errorf "segfault: you missed a byte"
      else (pos_ref := Addr.(!pos_ref ++ 1);
            return (byte (read data ~pos))) in
    {fast ; safe } in
  let error =
    let msg = "trying to read word from one byte of memory" in
    {
      safe = (fun ~pos_ref:_ -> errorf "%s" msg);
      fast = (fun ~pos_ref:_ -> invalid_arg msg);
    } in
  let open Bigstring in
  {
    int8   = make unsafe_get_int8;
    uint8  = make unsafe_get_uint8;
    int16  = error;
    uint16 = error;
    int32  = error;
    int64  = error;
  }

let make_byte mem addr off : t = {
  mem with
  addr;
  off;
  size = 1;
  get = one_byte_getters mem.data addr off;
}

let create ?(pos=0) ?len endian addr data : t Or_error.t =
  let data_len = Bigstring.length data in
  let size = Option.value ~default:data_len len in
  let v = Validate.(name_list "Bap_memory.create" [
      name "size" @@ Int.validate_bound size
        ~min:(Excl 0) ~max:(Incl data_len);
      name "pos" @@ Int.validate_bound pos
        ~min:(Incl 0) ~max:(Excl data_len);
      name "pos+size" @@ Int.validate_ubound (pos+size)
        ~max:(Incl data_len);
    ]) in
  Validate.result v >>= fun () ->
  if size = 1 then
    let get = one_byte_getters data addr pos in
    return {endian; data; addr; off=pos; size; get}
  else
    let get = create_getters endian addr pos size data in
    return {endian; data; addr; off=pos; size; get }

let of_file endian addr path : t Or_error.t =
  create endian addr (Bap_fileutils.readfile path)

let min_addr t : addr = t.addr

let max_addr t : addr =
  let n = t.size - 1 in
  Addr.(t.addr ++ n)

let length t : int = t.size


let first_byte mem : t =
  make_byte mem mem.addr mem.off

let last_byte mem : t =
  make_byte mem (max_addr mem) (mem.off + mem.size - 1)

let getter mem : size -> getter = function
  | `r8  -> mem.get.uint8
  | `r16 -> mem.get.uint16
  | `r32 -> mem.get.int32
  | `r64 -> mem.get.int64

let contains mem =
  Addr.between ~low:(min_addr mem) ~high:(max_addr mem)

let compare_with mem addr =
  let low = min_addr mem and high = max_addr mem in
  if Addr.between ~low ~high addr then `addr_is_inside else
  if Addr.(addr < low) then `addr_is_below else `addr_is_above

let get ?disp ?index ?(scale=`r8) ?addr t : word or_error =
  let base = Option.value addr ~default:t.addr in
  let addr = Addr.memref ?disp ?index ~scale base in
  (getter t scale).safe ~pos_ref:(ref addr)

let (^) t addr = get ~addr t
let (^!) t addr = ok_exn (t ^ addr)

module Input = struct
  type 'a reader = t -> pos_ref : addr ref -> 'a or_error

  let word ~word_size m = (getter m word_size).safe

  let read  get t ~pos_ref = (get t.get).safe  ~pos_ref

  let int8 = read int8
  let uint8 = read uint8
  let int16 = read int16
  let uint16 = read uint16
  let int32 = read int32
  let int64 = read int64
end

let sub copy ?(word_size=`r8) ?from ?words  t : t or_error =
  let amin = Option.value from ~default:(min_addr t) in
  let amax =
    Option.map words
      ~f:(fun w -> Addr.(amin ++ Int.(w * Size.to_bytes word_size - 1))) |>
    Option.value ~default:(max_addr t) in
  Validate.(result @@ name "view must not be empty" @@
            Addr.validate_lbound amax ~min:(Incl amin)) >>= fun () ->
  Addr.Int_err.(!$amax - !$amin >>= Addr.to_int) >>= fun diff ->
  let size = diff + 1 in
  Addr.Int_err.(!$amin - !$(t.addr) >>= Addr.to_int) >>= fun off ->
  let off = t.off + off in
  let check_preconditions = Validate.(name_list "preconditions" [
      name "offset in bounds" @@ Int.validate_bound off
        ~min:(Incl t.off)
        ~max:(Excl (t.off + t.size));
      name_list "size fits" [
        Int.validate_bound size
          ~min:(Incl 1) ~max:(Incl t.size);
        Int.validate_ubound (off + size) ~max:(Incl (t.off + t.size))
      ];
    ]) in
  Validate.result check_preconditions >>= fun () ->
  if size = 1 then return (make_byte t amin off)
  else
    let get = create_getters t.endian amin off size t.data in
    return { t with size; data = t.data; addr = amin; off; get}

let view = sub ident
let copy = sub Bigstring.subo

let range mem a1 a2 =
  Addr.Int_err.(!$a2 - !$a1) >>= Addr.to_int >>= fun bytes ->
  view ~from:a1 ~words:(bytes + 1) mem

let to_buffer {data; off; size} =
  Bigsubstring.create ~pos:off ~len:size data

let merge m1 m2 =
  let m1,m2 =
    Addr.(if min_addr m1 < min_addr m2 then m1,m2 else m2,m1) in
  let m1_max = max_addr m1 in
  if Addr.(min_addr m2 > succ m1_max)
  then errorf "blocks doesn't intersect"
  else if endian m1 <> endian m2
  then errorf "blocks has different sex"
  else if not (phys_equal m1.data m2.data)
  then errorf "blocks doesn't share base"
  else
    let pos = m1.off in
    let len = m2.off + m2.size - m1.off in
    create ~pos ~len m1.endian (min_addr m1) m1.data

let folder step ?(word_size=`r8) t ~(init:'a) ~f : 'a =
  let read = (getter t word_size).fast in
  let pos_ref = ref t.off in
  let word_len = Size.to_bytes word_size in
  let finish = t.off + t.size - word_len in
  let base = Addr.(t.addr -- t.off) in
  let rec loop init =
    if pos_ref.contents <= finish then
      loop (step base ~pos_ref init f read)
    else init in
  loop init

let with_address base ~pos_ref (init : 'a) f read : 'a =
  let addr = Addr.(base ++ !pos_ref)  in
  f addr (read ~pos_ref) init

let without_address _base ~pos_ref init f read =
  f (read ~pos_ref) init

let foldi ?word_size t ~init ~f  =
  folder with_address ?word_size t ~init ~f

let fold ?word_size t ~init ~f =
  folder without_address ?word_size t ~init ~f





(* this was made with a functor previously,
   but binding is costly even when no-op, so
   for performance purposes I implemented everything
   by hand *)

let iter ?word_size t ~f =
  fold ?word_size t ~init:() ~f:(fun w () -> f w)

let iteri ?word_size t ~f =
  foldi ?word_size t ~init:() ~f:(fun a w () -> f a w)

let exists ?word_size t ~f =
  with_return (fun s -> iteri ?word_size t ~f:(fun a w ->
      if f a w then s.return true); false)

let for_all ?word_size t ~f =
  with_return (fun s -> iteri ?word_size t ~f:(fun a w ->
      if not(f a w) then s.return false); true)

let count ?word_size t ~f =
  foldi ?word_size t ~init:0 ~f:(fun a w n -> if f a w then n + 1 else n)

let find_map ?word_size t ~f =
  with_return (fun s -> iteri ?word_size t ~f:(fun a w -> match f a w with
      | None -> () | some -> s.return some); None)

let find_if ?word_size t ~f =
  find_map ?word_size t ~f:(fun a w -> if f a w then Some w else None)

module Make_iterators( M : Monad.S) = struct
  open M
  type 'a m = 'a M.t

  let fold ?word_size t ~(init:'a) ~f : 'a m =
    fold ?word_size t ~init:(return init) ~f:(fun w s -> s >>= f w)

  let foldi ?word_size t ~(init:'a) ~f : 'a m =
    foldi ?word_size t ~init:(return init) ~f:(fun a w s -> s >>= f a w)

  let iter ?word_size t ~f =
    fold ?word_size t ~init:() ~f:(fun w () -> f w)

  let iteri ?word_size t ~f =
    foldi ?word_size t ~init:() ~f:(fun a w () -> f a w)

  let exists ?word_size t ~f =
    with_return (fun s ->
        iteri ?word_size t ~f:(fun a x -> f a x >>= function
          | true -> s.return (return true)
          | false -> return ())
        >>= fun () -> return false)

  let for_all ?word_size t ~f =
    with_return (fun s ->
        iteri ?word_size t ~f:(fun a x -> f a x >>= function
          | false -> s.return (return true)
          | true -> return ())
        >>= fun () -> return false)

  let count ?word_size t ~f =
    foldi ?word_size t ~init:0 ~f:(fun a w s ->
        f a w >>| function
        | true  -> s + 1
        | false -> s)

  let find_map ?word_size t ~f =
    with_return (fun s ->
        iteri t ~f:(fun a w -> f a w >>= function
          | None -> return ()
          | some -> s.return (return some)) >>| fun () -> None)

  let find_if ?word_size t ~f =
    find_map t ~f:(fun a w -> f a w >>| function
      | true  -> Some w
      | false -> None)
end

module With_error = Make_iterators(Or_error)


let pp_hex fmt t =
  let print_char c =
    let c = match Char.of_int c with
      | Some c when Char.is_print c -> c
      | _ -> '.' in
    Format.fprintf fmt "%c" c in

  let print_chars off = function
    | [] -> ()
    | chars ->
      Format.fprintf fmt "%*s" (3*off + 1) "|";
      List.iter (List.rev chars) ~f:print_char;
      Format.fprintf fmt "%*s\n" (off + 1) "|" in
  let chars = foldi t ~init:[] ~f:(fun addr char chars ->
      let newline = chars = [] || List.length chars = 16 in
      let addr = ok_exn Addr.(to_int64 (signed addr)) in
      let char = ok_exn Word.(to_int char) in
      if newline then begin
        print_chars 0 chars;
        Format.fprintf fmt "%08LX  " addr;
      end;
      Format.fprintf fmt "%02X " char;
      if newline then [char] else char :: chars) in
  let x = 16 - List.length chars in
  print_chars x chars


module Trie = struct
  module Key(Spec : sig val size : size end ) = struct
    open Spec
    type nonrec t = t
    type token = word with bin_io, compare, sexp

    let length m = length m / Size.to_bytes size
    let nth_token m n = get ~index:n ~scale:size m |> ok_exn
    let token_hash = Word.hash
  end
  module R8  = Trie.Make(Key(struct let size = `r8 end))
  module R16 = Trie.Make(Key(struct let size = `r16 end))
  module R32 = Trie.Make(Key(struct let size = `r32 end))
  module R64 = Trie.Make(Key(struct let size = `r64 end))
end

include Printable(struct
    open Format
    type nonrec t = t

    let module_name = "Bap.Std.Memory"

    let print_word fmt addr =
      let width = Addr.bitwidth addr / 4 in
      fprintf fmt "%0*Lx" width
        (Addr.(to_int64 (signed addr)) |> ok_exn)

    let pp_small fmt t =
      Format.fprintf fmt "%a: " print_word t.addr;
      iter t ~f:(fun b -> fprintf fmt "%a " print_word b)

    let pp fmt t =
      if length t < 16
      then pp_small fmt t
      else pp_hex fmt t
  end)

let hexdump t = Format.asprintf "%a" pp_hex t
