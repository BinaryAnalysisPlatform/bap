open Core_kernel.Std

let sexpable_of_string t_of_sexp name =
  try Some (t_of_sexp @@ Sexp.of_string name)
  with Sexp.Of_sexp_error _ -> None
