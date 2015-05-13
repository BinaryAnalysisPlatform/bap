open Core_kernel.Std
open Bap_common

type 'a tag = 'a Type_equal.Id.t with sexp_of
type t = Univ.t with sexp_of

let is t v = Univ.does_match v t
let get t v = Univ.match_ v t
let create t v = Univ.create t v
let tagname = Univ.type_id_name

module Tag = struct
  type 'a t = 'a tag with sexp_of
  let to_string = Type_equal.Id.name
  let register name sexp = Type_equal.Id.create ~name sexp
end

module Map = Univ_map

include Printable(struct
    type nonrec t = t with sexp_of

    let rec ppstring_of_sexp = function
      | Sexp.List ss -> sprintf "(%s)" @@
        String.concat ~sep:", " (List.map ss ~f:ppstring_of_sexp)
      | Sexp.Atom x -> x

    let pp ppf v = Format.fprintf ppf "%s %s"
        (String.capitalize (tagname v))
        (ppstring_of_sexp (sexp_of_t v))
    let module_name = "Bap.Std.Value"
  end)
