open Core_kernel.Std

type bool = [`bool]
type char = [ `schar | `char | `uchar]
type short = [`sshort | `ushort]
type int = [`sint | `uint]
type long = [`slong | `ulong]
type long_long = [`slong_long | `ulong_long]
type signed   = [`schar | `sshort | `sint | `slong | `slong_long]
type unsigned = [`uchar | `ushort | `uint | `ulong | `ulong_long]
type enum = [`enum of Int.t]
type integer = [char | signed | unsigned | enum]
type real = [`float | `double | `long_double]
type complex = [`cfloat | `cdouble | `clong_double]
type floating = [real | complex]
type basic = [integer | floating]

type cv = unit
type cvr = Bool.t

module Qualifier = struct
  type 'a t = {
    const : Bool.t;
    volatile : Bool.t;
    restrict : 'a;
  }
end

type 'a qualifier = 'a Qualifier.t

module Attr = struct
  type t = {
    name : string;
    args : string list;
  }
end

type attr = Attr.t

module Spec = struct
  type ('a,'b) t = {
    attrs : attr list;
    qualifier : 'a;
    t : 'b;
  }
end

type ('a,'b) spec = ('a,'b) Spec.t

type no_qualifier = unit

module rec T : sig
  type t = [
    | `Void
    | `Basic of (cv qualifier,basic) spec
    | `Pointer of (cvr qualifier, t) spec
    | `Array of (cvr qualifier, (t * Int.t option)) spec
    | `Structure of (no_qualifier, (string * t) list) spec
    | `Union of (no_qualifier, (string * t) list) spec
    | `Function of (no_qualifier, Proto.t) spec
  ]
end = T
and Proto : sig
  type t = {
    return : T.t;
    args   : (string * T.t) list;
    variadic : Bool.t;
  }
end = Proto

type t = T.t

type scalar = [
  | `Basic of (cv qualifier,basic) spec
  | `Pointer of (cvr qualifier, t) spec
]
type aggregate = [
  | `Array of (no_qualifier, t) spec
  | `Structure of (no_qualifier, t list) spec
]
