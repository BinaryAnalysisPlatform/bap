open Core_kernel.Std
open Bap.Std
open Format


module Data = struct
  include Bap_c_data
  let pp_value ppf = function
    | Top -> fprintf ppf "Top"
    | Set xs -> fprintf ppf "%a" (Seq.pp Word.pp) (Seq.of_list xs)
  let rec pp ppf = function
    | Imm (sz,v) -> fprintf ppf "%a:%a" pp_value v Size.pp sz
    | Seq ts -> fprintf ppf "%a" (Seq.pp pp) (Seq.of_list ts)
    | Ptr t  -> fprintf ppf "%a ptr" pp t
end

module Type = struct
  include Bap_c_type
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
    | `Array {Spec.t=(et,sz);qualifier} ->
      pr ppf "%a %aptr%a" pp et pp_cvr qualifier pp_size sz
    | `Structure {Spec.t} -> pr ppf "@[<2>{%a}@]" (pp_list pp_sc pp_field) t
    | `Union {Spec.t} -> pr ppf "@[<2>{%a}@]" (pp_list pp_or pp_field) t
    | `Function {Spec.t} -> pp_proto ppf t
  and pp_field ppf = function
    | ({Field.tag;name},t) -> pr ppf "%s.%s:%a" tag name pp t
  and pp_arg ppf = function
    | "",t -> pr ppf "%a" pp t
    | n,t -> pr ppf "%s:%a" n pp t
  and pp_proto ppf {Proto.return; args} =
    pr ppf "%a@ ->@ %a" pp_args args pp return
  and pp_args ppf = function
    | [] -> pp ppf `Void
    | args -> (pp_list pp_to pp_arg ppf) args
end


module Proto = struct
  type t = Bap_c_type.proto [@@deriving bin_io, compare, sexp]
  let pp = Type.pp_proto
end

let data = Value.Tag.register (module Data)
    ~name:"c.data"
    ~uuid:"e857a310-2cf0-487f-a879-ef5d8e38b3c9"

let t = Value.Tag.register (module Type)
    ~name:"c.type"
    ~uuid:"f668d2ac-874c-4369-acb3-138c041c98c7"

let proto = Value.Tag.register (module Proto)
    ~name:"c.proto"
    ~uuid:"23efab19-4293-4bb7-9c34-0afc63986c2e"
