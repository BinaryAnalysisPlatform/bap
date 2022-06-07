(** C Type System.

    We represent a C type structurally, i.e., the type representation
    is self-containted and doesn't require any typing requirement.

    Polymorphic variants are used to represent C type constructors and
    type groups.

    The type system is extended with attributes, i.e., it is possible
    to attach attributes of the form [attr(args)] to C type
    declarations.
*)
open Core_kernel[@@warning "-D"]

type char =
  [ `schar | `char | `uchar]
[@@deriving bin_io,compare,sexp,enumerate]

type short =
  [`sshort | `ushort]
[@@deriving bin_io,compare,sexp,enumerate]

type cint =
  [`uint | `sint]
[@@deriving bin_io,compare,sexp,enumerate]

type long =
  [`slong | `ulong]
[@@deriving bin_io,compare,sexp,enumerate]

type long_long =
  [`slong_long | `ulong_long]
[@@deriving bin_io,compare,sexp,enumerate]

type signed =
  [`schar | `sshort | `sint | `slong | `slong_long]
[@@deriving bin_io,compare,sexp,enumerate]

type unsigned =
  [`bool | `uchar | `ushort | `uint | `ulong | `ulong_long]
[@@deriving bin_io,compare,sexp,enumerate]

type enum =
  [`enum of (string * int64 option) list]
[@@deriving bin_io,compare,sexp]

type integer =
  [char | signed | unsigned | enum]
[@@deriving bin_io,compare,sexp]

type real =
  [`float | `double | `long_double]
[@@deriving bin_io,compare,sexp,enumerate]

type complex =
  [`cfloat | `cdouble | `clong_double]
[@@deriving bin_io,compare,sexp,enumerate]

type floating = [real | complex]
[@@deriving bin_io,compare,sexp,enumerate]

type basic = [integer | floating]
[@@deriving bin_io,compare,sexp]


type cv = unit [@@deriving bin_io,compare,sexp]
type cvr = Bool.t [@@deriving bin_io,compare,sexp]

module Qualifier : sig
  type 'a t = {
    const : Bool.t;
    volatile : Bool.t;
    restrict : 'a;
  } [@@deriving bin_io,compare,sexp]
end

type 'a qualifier = 'a Qualifier.t [@@deriving bin_io, compare, sexp]

module Attr : sig
  type t = {
    name : string;
    args : string list [@sexp.list];
  } [@@deriving bin_io, compare, sexp]
end

type attr = Attr.t
[@@deriving bin_io, compare, sexp]

module Spec : sig
  type ('a,'b) t = {
    qualifier : 'a;
    t : 'b;
    attrs : attr list [@sexp.list];
  } [@@deriving bin_io, compare, sexp]

end

type ('a,'b) spec = ('a,'b) Spec.t
[@@deriving bin_io, compare, sexp]

type no_qualifier = [`no_qualifier]
[@@deriving bin_io, compare, sexp]

module Proto : sig
  type 'a t = {
    return : 'a;
    args   : (string * 'a) list;
    variadic : Bool.t;
  } [@@deriving bin_io, compare, sexp]
end

module Compound : sig
  type 'a t = {
    name : string;
    fields : (string * 'a) list;
  } [@@deriving bin_io, compare, sexp]
end

module Array : sig
  type 'a t = {
    element : 'a;
    size : Int.t option
  } [@@deriving bin_io, compare, sexp]
end

type t = [
  | `Void
  | `Basic     of (cv  qualifier, basic) spec
  | `Pointer   of (cvr qualifier, t) spec
  | `Array     of (cvr qualifier, array) spec
  | `Structure of (no_qualifier,  compound) spec
  | `Union     of (no_qualifier,  compound) spec
  | `Function  of (no_qualifier,  proto) spec
] [@@deriving bin_io, compare, sexp]
and proto     = t Proto.t [@@deriving bin_io, compare, sexp]
and compound  = t Compound.t [@@deriving bin_io, compare, sexp]
and array     = t Array.t [@@deriving bin_io, compare, sexp]


type scalar = [
  | `Basic of (cv qualifier,basic) spec
  | `Pointer of (cvr qualifier, t) spec
] [@@deriving bin_io, compare, sexp]

type aggregate = [
  | `Array of (no_qualifier, t) spec
  | `Structure of (no_qualifier, t list) spec
] [@@deriving bin_io, compare, sexp]


(** [attrs t] the list of attributes associated with the type [t].  *)
val attrs : t -> attr list

(** [is_const t] is [true] if type is const-qualified.*)
val is_const : t -> Bool.t

(** [is_volatile t] is [true] if type is volatile-qualified.*)
val is_volatile : t -> Bool.t

(** [is_restrict t] is [true] if type is restrict-qualified.*)
val is_restrict : t -> Bool.t


(** {2 Basic Types} *)

(** [basic x] constructs a basic type.

    Example, [basic `char].
    All parameters default to false or empty.

    @since 2.5.0 *)
val basic : ?attrs:attr list -> ?const:Bool.t -> ?volatile:Bool.t -> basic -> t

val is_basic : t -> Bool.t
val is_char : t -> Bool.t
val is_short : t -> Bool.t
val is_cint : t -> Bool.t
val is_signed : t -> Bool.t
val is_unsigned : t -> Bool.t
val is_enum : t -> Bool.t
val is_integer : t -> Bool.t
val is_real : t -> Bool.t
val is_complex : t -> Bool.t
val is_floating : t -> Bool.t


(** {2 Pointers and Arrays} *)


(** [pointer t] constructs a pointer to the type [t].  *)
val pointer :
  ?attrs:attr list ->
  ?const:Bool.t ->
  ?volatile:Bool.t ->
  ?restrict:Bool.t -> t -> t

(** [array t] constructs an array of type [t] elements.

    The [size] is the optional size (the number of elements) of the array.
*)
val array :
  ?attrs:attr list ->
  ?const:Bool.t ->
  ?volatile:Bool.t ->
  ?restrict:Bool.t ->
  ?size:Int.t ->
  t -> t

val is_array : t -> Bool.t

val is_pointer : t -> Bool.t

(** {2 Compounds }  *)

(** [structure name fields] constructure a structure type.  *)
val structure : ?attrs:attr list -> string -> (string * t) list -> t

val is_structure : t -> Bool.t

(** [union name fields] conunion a union type.  *)
val union : ?attrs:attr list -> string -> (string * t) list -> t

val is_union : t -> Bool.t


(** [function args] constructs a function type.

    @param return defaults to [`Void].
*)
val function_ : ?attrs:attr list -> ?variadic:Bool.t -> ?return:t ->
  (string * t) list -> t

val is_function : t -> Bool.t

val pp : Format.formatter -> t -> unit
val pp_proto : Format.formatter -> proto -> unit
