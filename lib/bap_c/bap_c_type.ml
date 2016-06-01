(** C Type System.

    We represent a C type structurally, i.e., the type representation
    is self-containted and doesn't require any typing requirement.

    Polymorphic variants are used to represent C type constructors and
    type groups.

    The type system is extended with attributes, i.e., it is possible
    to attach attributes of the form [attr(args)] to C type
    declarations.
*)
open Core_kernel.Std

type bool = [`bool] [@@deriving bin_io,compare,sexp]
type char = [ `schar | `char | `uchar] [@@deriving bin_io,compare,sexp]
type short = [`sshort | `ushort] [@@deriving bin_io,compare,sexp]
type cint = [`uint | `sint] [@@deriving bin_io,compare,sexp]
type long = [`slong | `ulong] [@@deriving bin_io,compare,sexp]
type long_long = [`slong_long | `ulong_long] [@@deriving bin_io,compare,sexp]
type signed   = [`schar | `sshort | `sint | `slong | `slong_long]
  [@@deriving bin_io,compare,sexp]
type unsigned = [`uchar | `ushort | `uint | `ulong | `ulong_long]
  [@@deriving bin_io,compare,sexp]
type enum = [`enum of Int.t] [@@deriving bin_io,compare,sexp]
type integer = [char | signed | unsigned | enum] [@@deriving bin_io,compare,sexp]
type real = [`float | `double | `long_double] [@@deriving bin_io,compare,sexp]
type complex = [`cfloat | `cdouble | `clong_double] [@@deriving bin_io,compare,sexp]
type floating = [real | complex] [@@deriving bin_io,compare,sexp]
type basic = [integer | floating] [@@deriving bin_io,compare,sexp]


type cv = unit [@@deriving bin_io,compare,sexp]
type cvr = Bool.t [@@deriving bin_io,compare,sexp]

module Qualifier = struct
  type 'a t = {
    const : Bool.t;
    volatile : Bool.t;
    restrict : 'a;
  } [@@deriving bin_io,compare,sexp]
end

type 'a qualifier = 'a Qualifier.t [@@deriving bin_io, compare, sexp]

module Attr = struct
  type t = {
    name : string;
    args : string sexp_list;
  } [@@deriving bin_io, compare, sexp]
end

type attr = Attr.t
  [@@deriving bin_io, compare, sexp]

module Spec = struct
  type ('a,'b) t = {
    qualifier : 'a;
    t : 'b;
    attrs : attr sexp_list;
  } [@@deriving bin_io, compare, sexp]

end

type ('a,'b) spec = ('a,'b) Spec.t
  [@@deriving bin_io, compare, sexp]

type no_qualifier = [`no_qualifier]
  [@@deriving bin_io, compare, sexp]

module Proto = struct
  type 'a t = {
    return : 'a;
    args   : (string * 'a) list;
    variadic : Bool.t;
  } [@@deriving bin_io, compare, sexp]
end

type t = [
  | `Void
  | `Basic of (cv qualifier,basic) spec
  | `Pointer of (cvr qualifier, t) spec
  | `Array of (cvr qualifier, (t * Int.t option)) spec
  | `Structure of (no_qualifier, (string * t) list) spec
  | `Union of (no_qualifier, (string * t) list) spec
  | `Function of (no_qualifier, proto) spec
] and proto = t Proto.t [@@deriving bin_io, compare, sexp]


type scalar = [
  | `Basic of (cv qualifier,basic) spec
  | `Pointer of (cvr qualifier, t) spec
] [@@deriving bin_io, compare, sexp]

type aggregate = [
  | `Array of (no_qualifier, t) spec
  | `Structure of (no_qualifier, t list) spec
] [@@deriving bin_io, compare, sexp]
