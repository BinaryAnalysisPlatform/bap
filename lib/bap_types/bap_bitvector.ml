open Core_kernel
open Regular.Std
open Or_error
open Format


(* The bap_bitvector module is provided as a shim for the new and much
   more efficient bitvec library. The sole purpose of this library is
   to provide compatiblity with the legacy interace where bitvector
   was bearing its width and signedness property, and operations on
   bitvectors were defined by its runtime representation.

   Since the shift to the new semantics representation, we no longer
   need to store widths and signedness inside the bitvector, as those
   properties are now totally defined by the typing context.

   The implementation of the shim uses Bitv.t as the representation,
   however we borrown the least 15 bits for storing bitwidth and the
   signedness flag. This is basically the same representation as we
   had for BAP 1.x, except that we used Z.t directly, and now we are
   using Bitvec.t instead (which is also Z.t underneath the hood).


   word format:
     +-----------+------+---+
     |  payload  | size | s |
     +-----------+------+---+
      size+15  15 14       0
*)

type endian = LittleEndian | BigEndian
[@@deriving bin_io, compare, sexp]



module Packed : sig
  type t [@@deriving bin_io, sexp]

  val create : ?signed:bool -> Z.t -> int -> t
  val bitwidth : t -> int
  val modulus : t -> Bitvec.modulus
  val is_signed : t -> bool
  val signed : t -> t
  val unsigned : t -> t
  val payload : t -> Bitvec.t

  val hash : t -> int

  val lift1 : t -> (Bitvec.t -> Bitvec.t Bitvec.m) -> t
  val lift2 : t -> t -> (Bitvec.t -> Bitvec.t -> Bitvec.t Bitvec.m) -> t
  val lift3 : t -> t -> t -> (Bitvec.t -> Bitvec.t -> Bitvec.t -> Bitvec.t Bitvec.m) -> t
end = struct
  type packed = {packed : Z.t} [@@unboxed]
  type meta = {meta : Z.t} [@@unboxed]
  type data = {data : Z.t} [@@unboxed]
  type t = packed

  let metasize = 15
  let lenoff   = 1
  let lensize  = metasize - 1
  let maxlen   = 1 lsl lensize
  let metamask = Z.(one lsl metasize - one)

  let meta {packed} = {meta=Z.(packed land metamask)} [@@inline]
  let bitwidth x =
    let {meta} = meta x in
    Z.(to_int (meta asr 1)) [@@inline]
  let modulus x = Bitvec.modulus @@ bitwidth x [@@inline]

  let data {packed} = {data=Z.(packed asr metasize)} [@@inline]
  let hash x =
    let {data} = data x in
    Z.hash data
  let payload x =
    let m = modulus x in
    let z = data x in
    Bitvec.(bigint z.data mod m)
  [@@inline]

  let is_signed {packed=x} = Z.(is_odd x) [@@inline]

  let unsigned ({packed} as x) =
    if is_signed x
    then {packed=Z.((packed asr 1) lsl 1)}
    else x


  let mk_signed {packed=x} = {packed=Z.(x lor one)} [@@inline]
  let signed x = mk_signed x [@@inline]

  let pack x w =
    let meta = Z.of_int (w lsl 1) in
    {packed=Z.(x lsl metasize lor meta)}
  [@@inline]

  let create ?(signed=false) x w =
    let m = Bitvec.modulus w in
    let x = Bitvec.(bigint x mod m) in
    let r = pack (Bitvec.to_bigint x) w in
    if signed then mk_signed r else r
  [@@inline]

  let lift1 x f =
    let w = bitwidth x in
    let x = payload x in
    pack Bitvec.(to_bigint (f x mod modulus w)) w
  [@@inline]

  let lift2 x y f =
    let w = bitwidth x in
    let x = payload x and y = payload y in
    pack Bitvec.(to_bigint (f x y mod modulus w)) w
  [@@inline]

  let lift3 x y z f =
    let w = bitwidth x in
    let x = payload x and y = payload y and z = payload z in
    pack Bitvec.(to_bigint (f x y z mod modulus w)) w
  [@@inline]

  module Stringable = struct
    type t = packed
    let to_string {packed} = Z.to_bits packed
    let of_string s = {packed = Z.of_bits s}
  end

  include Binable.Of_stringable(Stringable)
  include Sexpable.Of_stringable(Stringable)
end

let pack = Packed.create
let payload = Packed.payload
let lift1 = Packed.lift1
let lift2 = Packed.lift2
let lift3 = Packed.lift3


type t = Packed.t [@@deriving bin_io]

let create x w = Packed.create (Bitvec.to_bigint x) w [@@inline]
let to_bitvec x = Packed.payload x [@@inline]
let unsigned x = x [@@inline]
let signed x = Packed.signed x [@@inline]
let hash x = Packed.hash x [@@inline]
let bits_of_z x = Bitvec.to_binary (Packed.payload x)
let unop op t = Packed.lift1 t op [@@inline]
let binop op t1 t2 = Packed.lift2 t1 t2 op [@@inline]
let bitwidth x = Packed.bitwidth x [@@inline]
let is_signed x = Packed.is_signed x [@@inline]

let pp_generic
    ?(case:[`lower|`upper]=`upper)
    ?(prefix:[`auto|`base|`none|`this of string]=`auto)
    ?(suffix:[`full|`none|`size]=`none)
    ?(format:[`hex|`dec|`oct|`bin]=`hex) ppf x =
  let width = bitwidth x in
  let m = Bitvec.modulus width in
  let is_signed = is_signed x in
  let x = Packed.payload x in
  let is_negative = is_signed && Bitvec.(msb x mod m) in
  let x = if is_negative then Bitvec.(abs x mod m) else x in
  let x = Bitvec.to_bigint x in
  let word x = Z.of_int x in
  let int x  = Z.to_int x in
  let base = match format with
    | `dec -> word 10
    | `hex -> word 0x10
    | `oct -> word 0o10
    | `bin -> word 0b10 in
  let pp_prefix ppf = match format with
    | `dec -> ()
    | `hex -> fprintf ppf "0x"
    | `oct -> fprintf ppf "0o"
    | `bin -> fprintf ppf "0b" in
  if is_negative then fprintf ppf "-";
  let () = match prefix with
    | `none -> ()
    | `this x -> fprintf ppf "%s" x
    | `base -> pp_prefix ppf
    | `auto ->
      if Bitvec_order.(x >= (min (word 10) base))
      then pp_prefix ppf in
  let fmt = format_of_string @@ match format, case with
    | `hex,`upper -> "%X"
    | `hex,`lower -> "%x"
    | _ -> "%d" in
  let rec print x =
    let d = int Z.(x mod base) in
    if Z.(x >= base)
    then print Z.(x / base);
    fprintf ppf fmt d in
  print x;
  match suffix with
  | `full -> fprintf ppf ":%d%c" width (if is_signed then 's' else 'u')
  | `size -> fprintf ppf ":%d" width
  | `none -> ()

let pp_full ppf = pp_generic ~suffix:`full ppf
let pp = pp_full

let string_of_word x = asprintf "%a" pp_full x

let of_suffixed stem suffix =
  let z = Z.of_string stem in
  let sl = String.length suffix in
  if sl = 0
  then invalid_arg "Bitvector.of_string: an empty suffix";
  let chop x = String.subo ~len:(sl - 1) x in
  match suffix.[sl-1] with
  | 's' -> pack ~signed:true z (Int.of_string (chop suffix))
  | 'u' -> pack z (Int.of_string (chop suffix))
  | x when Char.is_digit x -> pack z (Int.of_string suffix)
  | _ -> invalid_arg "Bitvector.of_string: invalid prefix format"

let word_of_string = function
  | "false" -> pack Z.zero 1
  | "true"  -> pack Z.one  1
  | s -> match String.split ~on:':' s with
    | [z; n] -> of_suffixed z n
    | _ -> failwithf "Bitvector.of_string: '%s'" s ()

let pp_hex ppf = pp_generic ppf
let pp_dec ppf = pp_generic ~format:`dec ppf
let pp_oct ppf = pp_generic ~format:`oct ppf
let pp_bin ppf = pp_generic ~format:`bin ppf

let pp_hex_full ppf = pp_generic ~suffix:`full ppf
let pp_dec_full ppf = pp_generic ~format:`dec ~suffix:`full ppf
let pp_oct_full ppf = pp_generic ~format:`oct ~suffix:`full ppf
let pp_bin_full ppf = pp_generic ~format:`bin ~suffix:`full ppf

let string_of_value ?(hex=true) x =
  if hex
  then asprintf "%a" (fun p -> pp_generic ~prefix:`none ~case:`lower p) x
  else asprintf "%a" (fun p -> pp_generic ~format:`dec p) x

let pp = pp_hex
let to_string = string_of_word
let of_string = word_of_string

module Sexp_hum = struct
  type t = Packed.t
  let sexp_of_t x = Sexp.Atom (to_string x)
  let t_of_sexp = function
    | Sexp.Atom x -> of_string x
    | _ -> invalid_arg "Bitvector.t_of_sexp: expects an atom"
end

include (Sexp_hum : Sexpable.S with type t := Packed.t)

let msb x = Bitvec.(msb (Packed.payload x) mod Packed.modulus x)
let lsb x = Bitvec.(lsb (Packed.payload x) mod Packed.modulus x)

type packed = Packed.t [@@deriving bin_io]
let sexp_of_packed = Sexp_hum.sexp_of_t
let packed_of_sexp = Sexp_hum.t_of_sexp

let compare_mono x y =
  if is_signed x || is_signed y then
    let x_is_neg = msb x and y_is_neg = msb y in
    match x_is_neg, y_is_neg with
    | true,false -> -1
    | false,true -> 1
    | _ -> Bitvec.compare (payload x) (payload y)
  else Bitvec.compare (payload x) (payload y)

let with_validation t ~f = Or_error.map ~f (Validate.result t)

let extract ?hi ?(lo=0) t =
  let n = bitwidth t in
  let z = Bitvec.to_bigint (payload t) in
  let hi = Option.value ~default:(n-1) hi in
  let len = hi-lo+1 in
  if len <= 0
  then failwithf "Bitvector.extract: len %d is negative" len ();
  if is_signed t && msb t
  then pack Z.((minus_one lsl n) lor Z.extract z lo len) len
  else pack (Z.extract z lo len) len

module Cons = struct
  let b0 = pack Z.zero 1
  let b1 = pack Z.one 1
  let of_bool v = if v then b1 else b0
  let of_int32 ?(width=32) n = pack (Z.of_int32 n) width
  let of_int64 ?(width=64) n = pack (Z.of_int64 n) width
  let of_int ~width v = pack (Z.of_int v) width
  let ones  n = of_int (-1) ~width:n
  let zeros n = of_int (0)  ~width:n
  let zero  n = of_int 0    ~width:n
  let one   n = of_int 1    ~width:n
end
include Cons

let safe f t = try_with (fun () -> f t)


let convert cast_z cast_bitvec x =
  if Packed.is_signed x
  then
    cast_z @@
    Z.signed_extract (Bitvec.to_bigint (payload x))
      0                         (* pos *)
      (Packed.bitwidth x)       (* len *)
  else
    cast_bitvec (payload x)

let to_int_exn x = convert Z.to_int Bitvec.to_int x
let to_int32_exn x = convert Z.to_int32 Bitvec.to_int32 x
let to_int64_exn x = convert Z.to_int64 Bitvec.to_int64 x
let to_int   = safe to_int_exn
let to_int32 = safe to_int32_exn
let to_int64 = safe to_int64_exn

let of_binary ?width endian num  =
  let num = match endian with
    | LittleEndian -> num
    | BigEndian -> String.rev num in
  let w = Option.value width ~default:(String.length num * 8) in
  pack (Z.of_bits num) w

let nsucc t n = Packed.lift1 t @@ fun t -> Bitvec.(nsucc t n)
let npred t n = Packed.lift1 t @@ fun t -> Bitvec.(npred t n)

let (++) t n = nsucc t n
let (--) t n = npred t n
let succ n = n ++ 1
let pred n = n -- 1

let (%:) x w = pack (Bitvec.to_bigint x) w

let gcd_exn x y = Packed.lift2 x y Bitvec.gcd
let lcm_exn x y = Packed.lift2 x y Bitvec.lcm
let gcdext_exn x y =
  let w = bitwidth x in
  let m = Bitvec.modulus w in
  let x = payload x and y = payload y in
  let (g,a,b) = Bitvec.(gcdext x y mod m) in
  g %: w,
  a %: w,
  b %: w

let gcd a b = Or_error.try_with (fun () ->
    gcd_exn a b)
let lcm a b = Or_error.try_with (fun () ->
    lcm_exn a b)
let gcdext a b = Or_error.try_with (fun () ->
    gcdext_exn a b)

let concat x y =
  let w1 = bitwidth x and w2 = bitwidth y in
  let x = payload x and y = payload y in
  Bitvec.append w1 w2 x y %: (w1+w2)

let (@.) = concat

module Unsafe = struct
  module Base = struct
    type t = Packed.t
    let one = Bitvec.one %: 1
    let zero = Bitvec.zero %: 1
    let succ = succ
    let pred = pred
    let abs x = lift1 x Bitvec.abs [@@inline]
    let neg  x = lift1 x Bitvec.neg [@@inline]
    let lnot x = lift1 x Bitvec.lnot [@@inline]
    let logand x y = lift2 x y Bitvec.logand [@@inline]
    let logor  x y = lift2 x y Bitvec.logor [@@inline]
    let logxor x y = lift2 x y Bitvec.logxor [@@inline]
    let add    x y = lift2 x y Bitvec.add [@@inline]
    let sub    x y = lift2 x y Bitvec.sub [@@inline]
    let mul    x y = lift2 x y Bitvec.mul [@@inline]
    let sdiv   x y = lift2 x y Bitvec.sdiv [@@inline]
    let udiv   x y = lift2 x y Bitvec.div [@@inline]
    let srem   x y = lift2 x y Bitvec.srem [@@inline]
    let urem   x y = lift2 x y Bitvec.rem [@@inline]
    let lshift x y = lift2 x y Bitvec.lshift [@@inline]
    let rshift x y = lift2 x y Bitvec.rshift [@@inline]
    let arshift x y = lift2 x y Bitvec.arshift [@@inline]

    let div x y = match is_signed x, is_signed y with
      | true,_|_,true -> sdiv x y
      | _ -> udiv x y
    [@@inline]

    let rem x y = match is_signed x, is_signed y with
      | true,_|_,true -> srem x y
      | _ -> urem x y
    [@@inline]

    let modulo x y = rem x y [@@inline]
  end
  include Base
  include (Bap_integer.Make(Base) : Bap_integer.S with type t := t)
end

module Safe = struct
  include Or_error.Monad_infix
  let (!$) v = Ok v


  let badwidth m x =
    Or_error.errorf "Word - wrong width, expects %d got %d" m x

  let lift m x =
    let w = bitwidth x in
    if m = w then Ok x else badwidth m w

  let lift1 op x = match x with
    | Ok x -> Ok (op x)
    | Error _ as e  -> e

  let lift2 op x y = match x, y with
    | Ok x, Ok y ->
      let w1 = bitwidth x and w2 = bitwidth y in
      if w1 = w2 then Ok (op x y) else badwidth w1 w2
    | (Error _ as e),_ | _, (Error _ as e) -> e

  let lift2h op x y = match x, y with
    | Ok x, Ok y -> Ok (op x y)
    | (Error _ as e),_ | _, (Error _ as e) -> e

  let int = lift
  let i1  = lift 1
  let i4  = lift 4
  let i8  = lift 8
  let i16 = lift 16
  let i32 = lift 32
  let i64 = lift 64
  let of_word_size = function
    | Word_size.W64 -> i64
    | Word_size.W32 -> i32

  module Base = struct
    type nonrec t = t Or_error.t
    let one = i1 Unsafe.one
    let zero = i1 Unsafe.zero
    let succ = lift1 Unsafe.succ
    let pred = lift1 Unsafe.pred
    let abs  = lift1 Unsafe.abs
    let neg  = lift1 Unsafe.neg
    let lnot = lift1 Unsafe.lnot
    let logand = lift2 Unsafe.logand
    let logor  = lift2 Unsafe.logor
    let logxor = lift2 Unsafe.logxor
    let add    = lift2 Unsafe.add
    let sub    = lift2 Unsafe.sub
    let mul    = lift2 Unsafe.mul
    let sdiv   = lift2 Unsafe.div
    let udiv   = lift2 Unsafe.udiv
    let srem   = lift2 Unsafe.rem
    let urem   = lift2 Unsafe.urem
    let sdiv   = lift2 Unsafe.sdiv
    let div   = lift2 Unsafe.div
    let rem   = lift2 Unsafe.rem
    let lshift = lift2h Unsafe.lshift
    let rshift = lift2h Unsafe.rshift
    let arshift = lift2h Unsafe.arshift
    let modulo = rem
  end
  include Bap_integer.Make(Base)
end

module Int_exn = struct
  module Base = struct
    type t = Packed.t
    let one = one 1
    let zero = zero 1

    let succ = succ
    let pred = pred

    open Safe

    let exn1 op x = ok_exn (op !$x)
    let exn2 op x y = ok_exn (op !$x !$y)

    let abs = exn1 abs
    let neg = exn1 neg
    let add = exn2 add
    let sub = exn2 sub
    let mul = exn2 mul
    let div = exn2 div
    let modulo = exn2 modulo
    let lnot = exn1 lnot
    let logand = exn2 logand
    let logor  = exn2 logor
    let logxor = exn2 logxor
    let lshift = exn2 lshift
    let rshift = exn2 rshift
    let arshift = exn2 arshift
  end
  include Bap_integer.Make(Base)
end

let extract_exn = extract
let extract ?hi ?lo x = Or_error.try_with (fun () ->
    extract_exn ?hi ?lo x)

let is_zero x =  Bitvec.compare (payload x) Bitvec.zero = 0
let is_one x = Bitvec.compare (payload x) Bitvec.one = 0
let is_negative x = is_signed x && msb x
let is_positive x =  not (is_zero x) && not (is_negative x)
let is_non_positive  = Fn.non is_positive
let is_non_negative = Fn.non is_negative


let validate check msg x =
  if check x then Validate.pass
  else Validate.fails msg x sexp_of_t

let validate_positive =
  validate is_positive "expects a positive number"
let validate_non_positive =
  validate is_non_positive "expects a non positive number"
let validate_negative =
  validate is_negative "expects a negative number"
let validate_non_negative =
  validate is_non_negative "expects a non negative number"

let enum_chars t endian  =
  let open Sequence in
  let n = (bitwidth t + 7) / 8 in
  if is_zero t then take (repeat '\x00') n
  else
    let bytes = bits_of_z t in
    let len = String.length bytes in
    let len_diff = len - n in
    let start,next,finish,cat,cut = match endian with
      | LittleEndian ->
        0, Int.succ, len,
        Fn.flip append, Fn.flip take (len - len_diff)
      | BigEndian ->
        len-1, Int.pred, ~-1,
        append, Fn.flip drop len_diff in
    let s = unfold ~init:start ~f:(fun i ->
        if i = finish then None
        else Some (bytes.[i], next i)) in
    if len_diff > 0 then cut s
    else
      let zeros = take (repeat '\x00') (-len_diff) in
      cat zeros s


let enum_bytes t endian =
  Sequence.map (enum_chars t endian) ~f:(fun c ->
      of_int ~width:8 (Char.to_int c))

let bits_of_byte byte =
  let byte = Char.to_int byte in
  Sequence.unfold ~init:7 ~f:(fun n ->
      if n < 0 then None
      else Some (byte land (1 lsl n) <> 0, n - 1))

let enum_bits bv endian =
  enum_chars bv endian |> Sequence.map ~f:bits_of_byte |> Sequence.concat

module Mono = Comparable.Make(struct
    type t = packed [@@deriving sexp]
    let compare x y =
      if phys_equal x y then 0
      else match Int.compare (bitwidth x) (bitwidth y) with
        | 0 -> compare_mono x y
        | _ -> failwith "Non monomorphic comparison"
  end)

module Trie = struct
  module Common = struct
    type nonrec t = t
    type token = int [@@deriving bin_io, compare, sexp]
    let token_hash = Fn.id

  end

  module Little = struct
    module Bits = Bap_trie.Make(struct
        include Common
        let length = bitwidth
        let nth_token v n =
          extract_exn ~hi:n ~lo:n v |> to_int |> ok_exn
      end)

    module Bytes = Bap_trie.Make(struct
        include Common
        let length v = (bitwidth v + 8 - 1) / 8
        let nth_token v n =
          extract_exn ~hi:(n*8 + 8) ~lo:(n*8) v |> to_int |> ok_exn
      end)
  end

  module Big = struct
    module Bits = Bap_trie.Make(struct
        include Common
        let length = bitwidth
        let nth_token v n =
          let m = bitwidth v - 1 - n in
          extract_exn ~hi:m ~lo:m v |> to_int |> ok_exn
      end)

    module Bytes = Bap_trie.Make(struct
        include Common
        let length v = (bitwidth v + 8 - 1) / 8
        let nth_token v n =
          let n = length v - n - 1 in
          extract_exn ~hi:(n*8 + 8) ~lo:(n*8) v |> to_int |> ok_exn
      end)
  end
end

include Or_error.Monad_infix
include Regular.Make(struct
    type t = packed [@@deriving bin_io, sexp]
    let compare x y =
      if phys_equal x y then 0
      else match Int.compare (bitwidth x) (bitwidth y) with
        | 0 -> compare_mono x y
        | r -> r
    [@@inline]
    let version = "2.0.0"
    let module_name = Some "Bap.Std.Word"
    let pp ppf = pp_generic ppf
    let hash = Packed.hash
  end)
module Int_err = Safe
include (Unsafe : Bap_integer.S with type t := t)
let one = Cons.one
let zero = Cons.zero

(* old representation for backward compatibility. *)
module V1 = struct
  module Bignum = struct
    module Repr : Stringable with type t = Z.t = struct
      type t = Z.t
      let to_string z = sprintf "0x%s" (Z.format "%X" z)
      let of_string = Z.of_string
    end
    include Z
    include Sexpable.Of_stringable(Repr)
    include Binable.Of_stringable(Repr)
  end

  type t = {
    z : Bignum.t;
    w : int;
    signed : bool;
  } [@@deriving bin_io, sexp]
end

(* stable serialization protocol *)
module Stable = struct
  module V1 = struct
    type t = Packed.t
    let compare = compare

    let of_legacy {V1.z; w; signed=s} =
      let x = pack z w in
      if s then Packed.signed x else x

    let to_legacy x = V1.{
        z = Bitvec.to_bigint (payload x);
        w = bitwidth x;
        signed = is_signed x;
      }

    include Binable.Of_binable(V1)(struct
        type t = Packed.t
        let to_binable = to_legacy
        let of_binable = of_legacy
      end)

    include Sexpable.Of_sexpable(V1)(struct
        type t = Packed.t
        let to_sexpable = to_legacy
        let of_sexpable = of_legacy
      end)
  end

  module V2 = struct
    type t = Packed.t [@@deriving bin_io, sexp]
    let compare = compare
  end
end


let to_string = string_of_word
let of_string = word_of_string

let () =
  add_reader ~desc:"Janestreet Binary Protocol" ~ver:"1.0.0" "bin"
    (Data.bin_reader (module Stable.V1));
  add_writer ~desc:"Janestreet Binary Protocol" ~ver:"1.0.0" "bin"
    (Data.bin_writer (module Stable.V1));
  add_reader ~desc:"Janestreet Sexp Protocol" ~ver:"1.0.0" "sexp"
    (Data.sexp_reader (module Stable.V1));
  add_writer ~desc:"Janestreet Sexp Protocol" ~ver:"1.0.0" "sexp"
    (Data.sexp_writer (module Stable.V1));
  add_reader ~desc:"Janestreet Binary Protocol" ~ver:"2.0.0" "bin"
    (Data.bin_reader (module Packed));
  add_writer ~desc:"Janestreet Binary Protocol" ~ver:"2.0.0" "bin"
    (Data.bin_writer (module Packed));
  add_reader ~desc:"Janestreet Sexp Protocol" ~ver:"2.0.0" "sexp"
    (Data.sexp_reader (module Sexp_hum));
  add_writer ~desc:"Janestreet Sexp Protocol" ~ver:"2.0.0" "sexp"
    (Data.sexp_writer (module Sexp_hum));

  let add name desc pp =
    add_writer ~desc ~ver:"2.0.0" name (Data.Write.create ~pp ()) in
  add "hex" "Hexadecimal without a suffix" pp_hex;
  add "dec" "Decimal without a suffix" pp_dec;
  add "oct" "Octal without a suffix" pp_oct;
  add "bin" "Binary (0 and 1) without a suffix" pp_bin;
  add "hex_full" "Hexadecimal" pp_hex_full;
  add "dec_full" "Decimal" pp_dec_full;
  add "oct_full" "Octal" pp_oct_full;
  add "bin_full" "Binary (0 and 1)" pp_bin_full;
