open Core_kernel.Std
open Bap_common

type 'a t = 'a Type_equal.Id.t with sexp_of
type value = Univ.t with sexp_of

let register name sexp = Type_equal.Id.create ~name sexp

let is t v = Univ.does_match v t
let value t v = Univ.match_ v t
let create t v = Univ.create t v
let tagname = Univ.type_id_name
let name = Type_equal.Id.name

include Printable(struct
    type t = value with sexp_of

    let rec ppstring_of_sexp = function
      | Sexp.List ss -> sprintf "(%s)" @@
        String.concat ~sep:", " (List.map ss ~f:ppstring_of_sexp)
      | Sexp.Atom x -> x

    let pp ppf v = Format.fprintf ppf "%s %s"
        (String.capitalize (tagname v))
        (ppstring_of_sexp (sexp_of_value v))
    let module_name = "Bap.Std.Tag"
  end)
