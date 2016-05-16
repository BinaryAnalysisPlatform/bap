open Core_kernel.Std
open Sexp.O

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

open Format
let pr ppf x = fprintf ppf x

let pp_spec ppq ppt ppf {Spec.qualifier; t; attrs} =
  pr ppf "%a%a" ppq qualifier ppt t

let pp_flag c ppf x = pr ppf "%s" (if x then c else "")
let pp_c,pp_v,pp_r = pp_flag "c", pp_flag "v", pp_flag "r"
let pp_cvr ppf {Qualifier.const=c;restrict=r;volatile=v} =
  pr ppf "%a%a%a" pp_c c pp_v v pp_r r
let pp_cv ppf {Qualifier.const=c;volatile=v} =
  pr ppf "%a%a" pp_c c pp_v v

let pp_basic ppf t = Sexp.pp ppf (sexp_of_basic t)

let pp_size ppf t = Option.iter t ~f:(Int.pp ppf)

let pp_or ppf () = pr ppf "@ | @ "
let pp_to ppf () = pr ppf "@ ->@ "
let pp_sc ppf () = pr ppf ";@ "

let rec pp ppf : t -> unit = function
  | `Void -> pr ppf "void"
  | `Basic spec -> pp_spec pp_cv pp_basic ppf spec
  | `Pointer {Spec.t; qualifier} -> pr ppf "%a %aptr" pp t pp_cvr qualifier
  | `Array {Spec.t=(et,sz);qualifier} ->
    pr ppf "%a %aptr%a" pp et pp_cvr qualifier pp_size sz
  | `Structure {Spec.t} -> pr ppf "@[<2>{%a}@]" (pp_fields pp_sc) t
  | `Union {Spec.t} -> pr ppf "@[<2>{%a}@]" (pp_fields pp_or) t
  | `Function {Spec.t} -> pp_proto ppf t
and pp_fields pp_sep = pp_print_list ~pp_sep pp_field
and pp_field ppf = function
  | "",t -> pp ppf t
  | (n,t) -> pr ppf "%s:%a" n pp t
and pp_proto ppf {Proto.return; args} =
  pr ppf "%a@ ->@ %a" pp_args args pp return
and pp_args ppf = function
  | [] -> pp ppf `Void
  | args -> pp_fields pp_to ppf args
