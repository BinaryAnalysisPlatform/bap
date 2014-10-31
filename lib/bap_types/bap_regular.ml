open Core_kernel.Std

module type S = sig
  type t with bin_io, sexp, compare
  val to_string: t -> string
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
  include Pretty_printer.S     with type t := t
end

module Make(M : sig
              type t with bin_io, compare, sexp
              include Pretty_printer.S with type t := t
              val hash: t -> int
              val module_name : string
            end) = struct
  include M

  let to_string t =
    let open Format in
    pp str_formatter t;
    flush_str_formatter ()

  let () = Pretty_printer.register (M.module_name ^ ".pp")
  include Comparable.Make_binable(M)
  include Hashable.Make_binable(M)
end
