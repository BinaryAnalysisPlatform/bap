open Core_kernel.Std
open Regular.Std
open Or_error
open Format


(* current representation has a very big overhead,
   depending on a size of a payload it is minumum five words,
   For example, although Zarith stores a 32bit word on a 64 bit
   machine in one word and represent it as an unboxed int, we still
   take four more words on top of that, as bitvector is represented as
   a pointer (1) to a boxed value that contains (3) fields, plus the
   header (1), that gives us 5 words (40 bytes), to store 4 bytes of
   payload.

   We have the following representation in mind, that will minimize
   the overhead. We will store an extra information, attached to a
   word in the word itself. Thus, bitvector will become a Z.t. We will
   use several bits of the word for meta data.
   To be able to store 32 bit words on a 64 bit platform we need to
   leave enough space in a 63 bit word for the payload. Ideally, we
   would like to have a support for an arbitrary bitwidth, but we can
   limit it to 2^14=32 (2 kB), spend one bit for sign (that can be
   removed later), thus we will have 48 bits for the payload.


     small:
     +-----------+------+---+
     |  payload  | size | s |
     +-----------+------+---+
      size+15  15 14       0


    Given this scheme, all values smaller than 0x100_000_000_0000 will
    have the same representation as OCaml int.

    The performance overhead is minimal, especially since no
    allocations are done anymore.

    Speaking of the sign. I would propose to remove it from the
    bitvector, as sign is not a property of a bitvector, it is its
    interpretation.

    Removing the sign will get us extra memory and CPU efficiency.
 *)

type endian = LittleEndian | BigEndian
  [@@deriving bin_io, compare, sexp]


module Bignum = struct
  module Repr : Stringable with type t = Z.t = struct
    type t = Z.t
    let to_string = Z.to_bits
    let of_string = Z.of_bits
  end
  include Z
  include Binable.Of_stringable(Repr)
end

type bignum = Bignum.t

module type Compare = sig
  val compare: int -> int -> int
end

module Size_poly = struct
  let compare x y = Int.compare x y
end

module Size_mono = struct
  let compare x y =
    if Int.(x <> y) then failwith "Non monomoprhic compare" else 0
end

(** internal representation *)
module Make(Size : Compare) = struct
  type t = Bignum.t [@@deriving bin_io]

  let module_name = Some "Bap.Std.Bitvector"

  let version = "2.0.0"

  let metasize = 15
  let lenoff   = 1
  let lensize  = metasize - 1
  let maxlen   = 1 lsl lensize

  let meta x = Z.extract x 0 (metasize - 1)
  let is_signed = Z.is_odd
  let bitwidth x = Z.extract x lenoff lensize |> Z.to_int

  let z x =
    let w = bitwidth x in
    if is_signed x
    then Z.signed_extract x metasize w
    else Z.extract x metasize w

  let signed x = Z.(x lor one)

  let with_z x v =
    let w = bitwidth x in
    let v = Z.(extract v 0 w lsl metasize) in
    Z.(v lor meta x)

  let create z w =
    if w > maxlen
    then invalid_argf
        "Bitvector overflow: maximum allowed with is %d bits"
        maxlen ();
    if w <= 0
    then invalid_argf
        "A nonpositive width is specified (%s,%d)"
        (Z.to_string z) w ();
    let meta = Z.(of_int w lsl 1) in
    let z = Z.(extract z 0 w lsl metasize) in
    Z.(z lor meta)

  let unsigned x = create (z x) (bitwidth x)
  let hash x = Z.hash (z x)
  let bits_of_z x = Z.to_bits (z x)
  let unop op t = op (z t)
  let binop op t1 t2 = op (z t1) (z t2)
  let lift1 op t = create (unop op t) (bitwidth t)
  let lift2 op t1 t2 = create (binop op t1 t2) (bitwidth t1)

  let lift2_triple op t1 t2 : t * t * t =
    let (a, b, c) = binop op t1 t2 in
    let w = bitwidth t1 in
    create a w, create b w, create c w

  let pp_generic
      ?(case:[`lower|`upper]=`upper)
      ?(prefix:[`auto|`base|`none|`this of string]=`auto)
      ?(suffix:[`full|`none|`size]=`none)
      ?(format:[`hex|`dec|`oct|`bin]=`hex) ppf x =
    let width = bitwidth x in
    let is_signed = is_signed x in
    let is_negative = Z.compare (z x) Z.zero < 0 in
    let x = Z.abs (z x) in
    let word = Z.of_int in
    let int  = Z.to_int in
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
        if Z.compare x (Z.min (word 10) base) >= 0
        then pp_prefix ppf in
    let fmt = format_of_string @@ match format, case with
      | `hex,`upper -> "%X"
      | `hex,`lower -> "%x"
      | _ -> "%d" in
    let rec print x =
      let d = int Z.(x mod base) in
      if x >= base
      then print Z.(x / base);
      fprintf ppf fmt d in
    print x;
    match suffix with
    | `full -> fprintf ppf ":%d%c" width (if is_signed then 's' else 'u')
    | `size -> fprintf ppf ":%d" width
    | `none -> ()

  let compare l r =
    let s = Size.compare (bitwidth l) (bitwidth r) in
    if s <> 0 then s
    else match is_signed l, is_signed r with
      | true,true | false,false -> Bignum.compare (z l) (z r)
      | true,false -> Bignum.compare (z l) (z (signed r))
      | false,true -> Bignum.compare (z (signed l)) (z r)

  let pp_full ppf = pp_generic ~suffix:`full ppf
  let pp = pp_full

  let to_string x =
    let z = z x in
    match bitwidth x with
    | 1 -> if Z.equal z Z.zero then "false" else "true"
    | n -> asprintf "%a" pp_full x

  let of_suffixed stem suffix =
    let z = Bignum.of_string stem in
    let sl = String.length suffix in
    if sl = 0
    then invalid_arg "Bitvector.of_string: an empty suffix";
    let chop x = String.subo ~len:(sl - 1) x in
    match suffix.[sl-1] with
    | 's' -> create z (Int.of_string (chop suffix)) |> signed
    | 'u' -> create z (Int.of_string (chop suffix))
    | x when Char.is_digit x -> create z (Int.of_string suffix)
    | _ -> invalid_arg "Bitvector.of_string: invalid prefix format"

  let of_string = function
    | "false" -> create Bignum.zero 1
    | "true"  -> create Bignum.one  1
    | s -> match String.split ~on:':' s with
      | [z; n] -> of_suffixed z n
      | _ -> failwithf "Bitvector.of_string: '%s'" s ()

  let with_validation t ~f = Or_error.map ~f (Validate.result t)

  let extract ?hi ?(lo=0) t =
    let n = bitwidth t in
    let z = z t in
    let hi = Option.value ~default:(n-1) hi in
    let len = hi-lo+1 in
    if len <= 0
    then failwithf "Bitvector.extract: len %d is negative" len ();
    create (Z.extract z lo len) len

  let sexp_of_t t = Sexp.Atom (to_string t)
  let t_of_sexp = function
    | Sexp.Atom s -> of_string s
    | _ -> invalid_argf
             "Bitvector.of_sexp: expected an atom got a list" ()
end

(* About monomorphic comparison.

   With monomorphic size comparison functions [hash] and [compare]
   will not be coherent with each other, since we can't prohibit
   someone to compare hashes from bitvectors with different sizes. For
   example, it means, that we can't really guarantee that in a Table
   all keys are bitvectors with the same size. So, as a consequence we
   can't make bitvector a real value implementing [Identifiable]
   interface. Since, monomorphic behaviour is rather specific and
   unintuitive we will move it in a separate submodule and use size
   polymorphic comparison by default.
*)
module T = Make(Size_poly)
include T

module Cons = struct
  let b0 = create (Bignum.of_int 0) 1
  let b1 = create (Bignum.of_int 1) 1
  let of_bool v = if v then b1 else b0
  let of_int32 ?(width=32) n = create (Bignum.of_int32 n) width
  let of_int64 ?(width=64) n = create (Bignum.of_int64 n) width
  let of_int ~width v = create (Bignum.of_int v) width
  let ones  n = of_int (-1) ~width:n
  let zeros n = of_int (0)  ~width:n
  let zero  n = of_int 0    ~width:n
  let one   n = of_int 1    ~width:n
end
include Cons

let safe f t = try_with (fun () -> f t)

let to_int_exn = unop Bignum.to_int
let to_int32_exn = unop Bignum.to_int32
let to_int64_exn = unop Bignum.to_int64
let to_int   = safe to_int_exn
let to_int32 = safe to_int32_exn
let to_int64 = safe to_int64_exn


let of_binary ?width endian num  =
  let num = match endian with
    | LittleEndian -> num
    | BigEndian -> String.rev num in
  let w = Option.value width ~default:(String.length num * 8) in
  create (Bignum.of_bits num) w

let nsucc t n = with_z t Bignum.(z t + of_int n)
let npred t n = with_z t Bignum.(z t - of_int n)

let (++) t n = nsucc t n
let (--) t n = npred t n
let succ n = n ++ 1
let pred n = n -- 1

let gcd_exn    = lift2 Bignum.gcd
let lcm_exn    = lift2 Bignum.lcm
let gcdext_exn = lift2_triple Bignum.gcdext

let gcd a b = Or_error.try_with (fun () ->
    gcd_exn a b)
let lcm a b = Or_error.try_with (fun () ->
    lcm_exn a b)
let gcdext a b = Or_error.try_with (fun () ->
    gcdext_exn a b)

let concat x y =
  let w = bitwidth x + bitwidth y in
  let x = Bignum.(z x lsl bitwidth y) in
  let z = Bignum.(x lor z y) in
  create z w

let (@.) = concat

module Unsafe = struct
  module Base = struct
    type t = T.t
    let one = create Z.one 1
    let zero = create Z.zero 1
    let succ = lift1 Bignum.succ
    let pred = lift1 Bignum.pred
    let abs  = lift1 Bignum.abs
    let neg  = lift1 Bignum.neg
    let lnot = lift1 Bignum.lognot
    let logand = lift2 Bignum.logand
    let logor  = lift2 Bignum.logor
    let logxor = lift2 Bignum.logxor
    let add    = lift2 Bignum.add
    let sub    = lift2 Bignum.sub
    let mul    = lift2 Bignum.mul
    let sdiv   = lift2 Bignum.div
    let udiv   = lift2 Bignum.ediv
    let srem   = lift2 Bignum.rem
    let urem   = lift2 Bignum.erem

    let sign_disp ~signed ~unsigned x y =
      let op = if is_signed x || is_signed y then signed else unsigned in
      op x y

    let div = sign_disp ~signed:sdiv ~unsigned:udiv
    let rem = sign_disp ~signed:srem ~unsigned:urem
    let modulo  = rem

    let shift dir x n = create (dir (z x) (Z.to_int (z n))) (bitwidth x)
    let lshift = shift Bignum.shift_left
    let rshift = shift Bignum.shift_right
    let arshift x y = shift Bignum.shift_right (signed x) y
  end
  include Base
  include (Bap_integer.Make(Base) : Bap_integer.S with type t := t)
end

module Safe = struct
  include Or_error.Monad_infix

  type m = t Or_error.t

  let (!$) v = Ok v

  let validate_equal (n,m) : Validate.t =
    if m = n then Validate.pass
    else Validate.failf "expected width %d, but got %d" m n

  let lift m t : m =
    validate_equal (m, bitwidth t) |> Validate.result >>|
    fun () -> t

  let lift1 op x : m = x >>| lift1 op

  let lift2 op (x : m) (y : m) : m =
    x >>= fun x -> y >>= fun y ->
    let v = validate_equal (bitwidth x, bitwidth y) in
    Validate.result v >>| fun () -> op x y

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
    type t = m
    let one = i1 (one 1)
    let zero = i1 (zero 1)
    let succ = lift1 Bignum.succ
    let pred = lift1 Bignum.pred
    let abs  = lift1 Bignum.abs
    let neg  = lift1 Bignum.neg

    let lnot = lift1 Bignum.lognot

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

    let sign_disp ~signed ~unsigned x y =
      x >>= fun x -> y >>= fun y ->
      let op = if is_signed x || is_signed y then signed else unsigned in
      op !$x !$y

    let div = sign_disp ~signed:sdiv ~unsigned:udiv
    let rem = sign_disp ~signed:srem ~unsigned:urem
    let modulo  = rem

    let shift dir (x : m) (y : m) : m =
      x >>= fun x -> y >>= fun y ->
      if unop Bignum.fits_int y
      then Ok (dir x y)
      else Or_error.errorf
          "cannot perform shift, because rhs doesn't fit int: %s" @@
        to_string y

    let lshift = shift Unsafe.lshift
    let rshift = shift Unsafe.rshift
    let arshift = shift Unsafe.arshift
  end
  include Bap_integer.Make(Base)
end

module Int_exn = struct
  module Base = struct
    type t = T.t
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

let is_zero = unop Bignum.(equal zero)
let is_one = unop Bignum.(equal one)
let is_positive = unop Bignum.(fun z -> gt z zero)
let is_non_positive  = Fn.non is_positive
let is_negative = unop Bignum.(fun z -> lt z zero)
let is_non_negative = Fn.non is_negative


let validate check msg x =
  if check x then Validate.pass
  else Validate.fails msg x sexp_of_t

let validate_positive =
  validate is_positive "should be positive"
let validate_non_positive =
  validate is_non_positive "should be non positive"
let validate_negative =
  validate is_negative "should be negative"
let validate_non_negative =
  validate is_non_negative "should be non negative"

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

module Mono = Comparable.Make(Make(Size_mono))

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
include Regular.Make(T)
module Int_err = Safe
include (Unsafe : Bap_integer.S with type t := t)
let one = Cons.one
let zero = Cons.zero

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
    type t = bignum
    let compare = compare

    let of_legacy {V1.z; w; signed=s} =
      let x = create z w in
      if s then signed x else x

    let to_legacy x = V1.{
        z = z x;
        w = bitwidth x;
        signed = is_signed x;
      }

    include Binable.Of_binable(V1)(struct
        type t = bignum
        let to_binable = to_legacy
        let of_binable = of_legacy
      end)

    include Sexpable.Of_sexpable(V1)(struct
        type t = bignum
        let to_sexpable = to_legacy
        let of_sexpable = of_legacy
      end)
  end

  module V2 = struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
  end
end


let pp = pp_hex

let () =
  add_reader ~desc:"Janestreet Binary Protocol" ~ver:"1.0.0" "bin"
    (Data.bin_reader (module Stable.V1));
  add_writer ~desc:"Janestreet Binary Protocol" ~ver:"1.0.0" "bin"
    (Data.bin_writer (module Stable.V1));
  add_reader ~desc:"Janestreet Sexp Protocol" ~ver:"1.0.0" "sexp"
    (Data.sexp_reader (module Stable.V1));
  add_writer ~desc:"Janestreet Sexp Protocol" ~ver:"1.0.0" "sexp"
    (Data.sexp_writer (module Stable.V1));
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
