open Core_kernel.Std
open Bap.Std
open Format
open Bap_c_type

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

let pp_list pp_sep field = pp_print_list ~pp_sep field

let rec pp ppf : t -> unit = function
  | `Void -> pr ppf "void"
  | `Basic spec -> pp_spec pp_cv pp_basic ppf spec
  | `Pointer {Spec.t; qualifier} -> pr ppf "%a %aptr" pp t pp_cvr qualifier
  | `Array {Spec.t={Array.element=et; size};qualifier} ->
    pr ppf "%a %aptr[%a]" pp et pp_cvr qualifier pp_size size
  | `Structure {Spec.t={Compound.fields; name}} ->
    pr ppf "@[<2>%s.{%a}@]" name (pp_list pp_sc pp_field) fields
  | `Union {Spec.t={Compound.fields; name}} ->
    pr ppf "@[<2>%s.{%a}@]" name (pp_list pp_or pp_field) fields
  | `Function {Spec.t} -> pp_proto ppf t
and pp_field ppf = function
  | (name,t) -> pr ppf "%s:%a" name pp t
and pp_arg ppf = function
  | "",t -> pr ppf "%a" pp t
  | n,t -> pr ppf "%s:%a" n pp t
and pp_proto ppf {Proto.return; args} =
  pr ppf "%a@ ->@ %a" pp_args args pp return
and pp_args ppf = function
  | [] -> pp ppf `Void
  | args -> (pp_list pp_to pp_arg ppf) args
