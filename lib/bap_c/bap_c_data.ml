open Core_kernel[@@warning "-D"]
open Bap.Std
open Format

type model32 = [
  | `LP32
  | `ILP32
]

type model64 = [
  | `ILP64
  | `LLP64
  | `LP64
]

type model = [model32 | model64]
type value =
  | Top
  | Set of word list
[@@deriving bin_io, compare, sexp]

type 'd obj =
  | Basic of Bap_c_type.basic
  | Field of (string * 'd)
  | Undef
  | Union of 'd list
[@@deriving bin_io, compare, sexp]

type ('d,'s) datum =
  | Imm of 's * 'd
  | Seq of ('d,'s) datum list
  | Ptr of ('d,'s) datum
[@@deriving bin_io, compare, sexp]

type layout = {layout : (layout obj,int) datum}
[@@deriving bin_io, compare, sexp]

type t = (value,Size.t) datum
[@@deriving bin_io, compare, sexp]

let pp_value ppf = function
  | Top -> fprintf ppf "Top"
  | Set xs -> fprintf ppf "%a" (Seq.pp Word.pp) (Seq.of_list xs)
let rec pp ppf = function
  | Imm (sz,v) -> fprintf ppf "%a:%a" pp_value v Size.pp sz
  | Seq ts -> fprintf ppf "%a" (Seq.pp pp) (Seq.of_list ts)
  | Ptr t  -> fprintf ppf "%a ptr" pp t


let rec pp_layout ppf : layout -> unit = fun {layout=datum} ->
  pp_datum ppf datum
and pp_datum ppf : (layout obj, int) datum -> unit = function
  | Imm (sz,v) ->
    fprintf ppf "@[<2>[%a : %d]@]" pp_obj v sz
  | Seq objs ->
    fprintf ppf "@[@[<hv2>{@ ";
    pp_print_list ~pp_sep:(fun ppf () ->
        fprintf ppf ",@ ")
      pp_datum ppf objs;
    fprintf ppf "@]@;}@]"
  | Ptr t ->
    fprintf ppf "*%a" pp_datum t
and pp_obj ppf : layout obj -> unit = function
  | Basic t -> Bap_c_type.(pp ppf (basic t))
  | Field (name,layout) ->
    fprintf ppf "@[<2><%s : %a>@]" name pp_layout layout
  | Undef ->
    fprintf ppf "<undef>"
  | Union xs ->
    fprintf ppf "@[<hv>";
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@;| ")
      pp_layout ppf xs;
    fprintf ppf "@]"
