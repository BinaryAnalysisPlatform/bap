(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(** Type declarations for BAP.

    @author Ivan Jager
*)

open Core_kernel

(** Addresses are big_ints *)
type addr = Big_int_Z.big_int
(** Support for s-expressions *)
let addr_of_sexp sexp =
  Core_kernel.String.t_of_sexp sexp |> Big_int_Z.big_int_of_string
let sexp_of_addr bi =
  Big_int_Z.string_of_big_int bi |> Core_kernel.String.sexp_of_t
let compare_addr = Big_int_Z.compare_big_int

type bv_size = int

type float_size = {exp_bits: int; sig_bits: int;}

let sexp_of_bv_size = Sexplib.Std.sexp_of_int
let sexp_of_float_size float =
  Sexp.List [
    Sexp.List [Sexp.Atom "exp_bits"; Sexplib.Std.sexp_of_int float.exp_bits];
    Sexp.List [Sexp.Atom "sig_bits"; Sexplib.Std.sexp_of_int float.sig_bits];
  ]

let bv_size_of_sexp = Sexplib.Std.int_of_sexp
let float_size_of_sexp = function
  | Sexp.List [
      Sexp.List [Sexp.Atom "exp_bits"; exp_bits];
      Sexp.List [Sexp.Atom "sig_bits"; sig_bits];
    ] ->
    {
      exp_bits = Sexplib.Std.int_of_sexp exp_bits;
      sig_bits = Sexplib.Std.int_of_sexp sig_bits;
    }
  | _ -> failwith "not a floating point s-expression"


let compare_bv_size (a:bv_size) (b:bv_size) = compare a b
let compare_float_size (a:float_size) (b:float_size) = Poly.compare a b


(** Labels are program locations that can be jumped to. *)
type label =
  | Name of string (** For named labels*)
  | Addr of addr (** For addresses. Cast REG_type as unsigned when comparing. *)
[@@deriving sexp, compare]

(** The IR type of a BAP expression *)
type typ =
  | Reg of bv_size (** an N-bit bitvector (use 1 for booleans). *)
  | TMem of (typ * typ) (** Memory of given index type, element type. *)
  | Array of (typ * typ) (** Array of index type, element type. *)
  | Float of float_size (** ebits, sbits *)
[@@deriving sexp, compare]

let reg_1 = Reg 1
and reg_8 = Reg 8
and reg_16 = Reg 16
and reg_32 = Reg 32
and reg_64 = Reg 64
and reg_128 = Reg 128
and reg_256 = Reg 256

let fp_16_bits = {exp_bits=5; sig_bits=11}
and fp_32_bits = {exp_bits=8; sig_bits=24}
and fp_64_bits = {exp_bits=11; sig_bits=53}
and fp_80_bits = {exp_bits=15; sig_bits=64}
and fp_128_bits = {exp_bits=15; sig_bits=113}

let fp_pc_32_bits = {exp_bits=15; sig_bits=24}
and fp_pc_64_bits = {exp_bits=15; sig_bits=53}
and fp_pc_79_bits = {exp_bits=15; sig_bits=64}

let fp_16 = Float fp_16_bits
and fp_32 = Float fp_32_bits
and fp_64 = Float fp_64_bits
and fp_80 = Float fp_80_bits
and fp_128 = Float fp_128_bits

let fp_width fp =
  match fp with
  | _ when Poly.(=) fp fp_80_bits -> 80
  | {exp_bits; sig_bits} -> exp_bits + sig_bits

(** [Array] memories can only be updated or accessed in terms of
    their element type, which is usually [Reg 8].  [TMem] memories
    can be updated or accessed by any type, for instance both [Reg 8]
    and [Reg 32].  Native code is generally lifted with [TMem]
    memories for simplicity, and converted to [Array] type when
    needed.  SMT solvers require [Array] type memories. *)

(** Different forms of casting *)
type cast_type =
  | CAST_UNSIGNED (** 0-padding widening cast. *)
  | CAST_SIGNED (** Sign-extending widening cast. *)
  | CAST_HIGH (** Narrowning cast. Keeps the high bits. *)
  | CAST_LOW (** Narrowing cast. Keeps the low bits. *)
[@@deriving sexp]

(** For constant round modes. Defined in the IEEE 754 standard:
    https://en.wikipedia.org/wiki/IEEE_floating_point
*)
type roundmode_type =
  | RNE (** round to nearest ties to even *)
  | RTZ (** round toward zero *)
  | RTP (** round toward positive infinity *)
  | RTN (** round toward negative infinity *)
  | RNA (** round to nearest ties to away *)
[@@deriving sexp]

(** Float binary ops, which take a rounding mode. *)
type fbinop_type =
  | FADD  (** adds two floating point numbers *)
  | FSUB  (** subtract second float from the first *)
  | FMUL  (** multiply two floating point numbers *)
  | FDIV  (** divide first floating point by the second *)
  | FREM  (** the remainder after dividing the first by the second float *)
  | FMIN  (** returns the minimum of two floats *)
  | FMAX  (** returns the maximum of two floats *)
  | FLE   (** returns true if the first float is less or equal than the second *)
  | FLT   (** returns true if the first float is less than the second *)
  | FEQ   (** returns true if the the floats are equal *)
[@@deriving sexp]

(** Float unary ops, which take a rounding mode. *)
type funop_type =
  | FABS    (** gets the absolute value of a float *)
  | FNEG    (** gets the negation of a float *)
  | FSQRT   (** gets the square root of a float *)
  | FROUND  (** rounds a float *)
  | FISNORM (** returns true if a float is normal *)
  | FISSUB  (** returns true if a float is subnormal *)
  | FISZERO (** returns true if a float is zero *)
  | FISINF  (** returns true if a float is Inf *)
  | FISNAN  (** returns true if a float is NaN *)
  | FISNEG  (** returns true if a float is negative *)
  | FISPOS  (** returns true if a float is positive *)

  | FFTOUBV of bv_size    (** converts float to BV of bv_size *)
  | FFTOSBV of bv_size    (** converts float to BV of bv_size *)
  | FBVTOUF of float_size (** converts BV to float of float_size *)
  | FBVTOSF of float_size (** converts BV to float of float_size *)
  | FFTOF of float_size   (** converts float to float of float_size *)
  | FIEEEBVTOF of float_size   (** converts BV to float of float_size *)
  | FFTOIEEEBV of bv_size     (** converts float to BV of bv_size *)
  | FNAN of float_size   (** creates a NaN float *)
[@@deriving sexp]

(** Binary operations implemented in the IR *)
type binop_type =
  | PLUS (** Integer addition. (commutative, associative) *)
  | MINUS (** Subtract second integer from first. *)
  | TIMES (** Integer multiplication. (commutative, associative)*)
  | DIVIDE (** Unsigned integer division. *)
  | SDIVIDE (** Signed integer division. *)
  | MOD (** Unsigned modulus. *)
  | SMOD (** Signed modulus. *)
  | LSHIFT (** Left shift. *)
  | RSHIFT (** Right shift, fill with 0. *)
  | ARSHIFT (** Right shift, sign extend. *)
  | AND (** Bitwise and. (commutative, associative) *)
  | OR (** Bitwise or. (commutative, associative) *)
  | XOR (** Bitwise xor. (commutative, associative) *)
  | EQ (** Equals (commutative) (associative on booleans) *)
  | NEQ (** Not equals (commutative) (associative on booleans) *)
  | LT (** Unsigned less than *)
  | LE (** Unsigned less than or equal to *)
  | SLT (** Signed less than *)
  | SLE (** Signed less than or equal to *)
  | FP of fbinop_type * roundmode_type (** FP operation with a rounding mode *)
[@@deriving sexp]

(** Unary operations implemented in the IR *)
type unop_type =
  | NEG (** Negate (2's complement) *)
  | NOT (** Bitwise not *)
  | FP of funop_type * roundmode_type (** FP operation with a rounding mode *)
[@@deriving sexp]

(** The position of a statement in a source file *)
type pos = (string * int)

(** {5 Extra attributes} *)

type taint_type = Taint of int
type usage = RD | WR | RW

(** Information about a concrete operand from a trace *)
type context =
  {
    name  : string;
    mem   : bool;
    t     : typ;
    index : addr;
    value : Big_int_Z.big_int;
    usage : usage;
    taint : taint_type
  }

(** Attributes are extra information contained in a [stmt]. *)
type attribute =
  | Asm of string (** Assembly representation of the following IL code *)
  | Address of addr (** The address corresponding to lifted IL. *)
  | Target of label (** An address this insn may jump to (esp. function specials) *)
  | Liveout (** Statement should be considered live by deadcode elimination *)
  | StrAttr of string (** Generic printable and parseable attribute *)
  | NamedStrAttr of string * string (** Generic printable and parseable attribute *)
  | Context of context         (** Information about the
                                   instruction operands from a
                                   trace. *)
  | ThreadId of int (** Executed by a specific thread *)
  | ExnAttr of exn (** Generic extensible attribute, but no parsing *)
  | InitRO (** The memory in this assignment is stored in the binary *)
  | Synthetic (** Operation was added by an analysis *)
  | SpecialBlock (** Start of a special block *)
  | TaintIntro of int * string * int

type attributes = attribute list

(** Visitors are a systematic method for exploring and changing
    objects of type ['a].  In BAP, ['a] can be [stmt], [exp], etc. The
    children of statements are expressions; the children of
    expressions are subexpressions and variables. *)
(*type 'a visit_action =
  | SkipChildren (** Do not visit the children. Return the node
                     as is. *)
  | DoChildren      (** Continue exploring children of the current node. Changes to children will propagate up. *)
  | ChangeTo of 'a  (** Replace the current object with the specified one. *)
  | ChangeToAndDoChildren of 'a (** Replace the current object with
                                    the given one, and visit children
                                    of the {b replacement} object. *) *)

(** Specifies whether generated VCs will be evaluated for
    satisfiability or validity. Alternatively, quantifiers can be
    used. *)
let bits = function Reg n -> n | _ -> failwith "Not of register type"
