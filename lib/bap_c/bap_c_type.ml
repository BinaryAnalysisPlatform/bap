open Core_kernel.Std

type bool = [`bool]
type char = [ `schar | `char | `uchar]
type short = [`sshort | `ushort]
type int = [`sint | `uint]
type long = [`slong | `ulong]
type long_long = [`slong_long | `ulong_long]
type signed   = [`schar | `sshort | `sint | `slong | `slong_long]
type unsigned = [`uchar | `ushort | `uint | `ulong | `ulong_long]
type enum = [`enum of int]
type integer = [char | signed | unsigned | enum]
type real = [`float | `double | `long_double]
type complex = [`cfloat | `cdouble | `clong_double]
type floating = [real | complex]
type basic = [integer | floating]

type cv
type cvr = bool

type 'a qualifier = {
  const : bool;
  volatile : bool;
  restrict : 'a;
}

type attr = {
  attr_name : string;
  attr_args : string;
}

type ('a,'b) spec = {
  attrs : attr list;
  qualifier : 'a;
  spec : 'b;
}

type no_qualifier

type t = [
  | `Void
  | `Basic of (cv qualifier,basic) spec
  | `Pointer of (cvr qualifier, t) spec
  | `Array of (no_qualifier, (t * Int.t option)) spec
  | `Structure of (no_qualifier, t list) spec
  | `Union of (no_qualifier, t list) spec
  | `Function of (no_qualifier, proto) spec
]
and proto = {
  return : t;
  args   : t list;
  variadic : bool;
}

type scalar = [
  | `Basic of (cv qualifier,basic) spec
  | `Pointer of (cvr qualifier, t) spec
]
type aggregate = [
  | `Array of (no_qualifier, t) spec
  | `Structure of (no_qualifier, t list) spec
]
