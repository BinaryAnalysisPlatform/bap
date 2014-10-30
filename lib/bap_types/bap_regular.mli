open Core_kernel.Std

(** Regular types models a general concept of value, i.e., something
    that can be used in way similiar to regular [int], [string],
    [char] and other built in types. So that it can be compared, used
    in maps, sets, hashtables, printer, etc.

    Note: this signature is pretty similiar to core's [Identifiable],
    but doesn't require [of_string] function, that is usually much
    harder to implement in comparison with [to_string] function. Also,
    instead of [to_string] it requires [pp] function that can be
    implemented much more efficiently and elegantly.
*)
module type S = sig
  type t with bin_io, sexp, compare
  val to_string : t -> string
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
  include Pretty_printer.S     with type t := t
end

module Make(M : sig
              type t with bin_io, sexp, compare
              include Pretty_printer.S with type t := t
              val hash : t -> int
              val module_name : string
            end ) : S with type t := M.t
