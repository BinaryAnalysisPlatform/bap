open Sexplib0

type t = Bitvec.t

module Functions = struct
  let sexp_of_t x = Sexp.Atom (Bitvec.to_string x)
  let t_of_sexp = function
    | Sexp.Atom x -> Bitvec.of_string x
    | _ -> invalid_arg "Bitvec_sexp: expects an atom, got list"
end

include Functions
