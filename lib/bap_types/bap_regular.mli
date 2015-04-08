open Core_kernel.Std

module type Printable = sig
  type t
  val to_string : t -> string

  (** [str () t] is formatted output function that matches "%a"
      conversion format specifier in functions, that prints to string,
      e.g., [sprintf], [failwithf], [errorf] and, suprisingly all
      [Lwt] printing function, including [Lwt_io.printf] and logging
      (or any other function with type ('a,unit,string,...)
      formatN`. Example:

      [Or_error.errorf "type %a is not valid for %a"
         Type.str ty Exp.str exp]
  *)
  val str : unit -> t -> string

  (** synonym for [str]  *)
  val pps : unit -> t -> string

  (** will print to a standard [output_channel], useful for using in
      [printf], [fprintf], etc. *)
  val ppo : out_channel -> t -> unit

  include Pretty_printer.S     with type t := t
end

(** Regular types models a general concept of value, i.e., something
    that can be used in way similiar to regular [int], [string],
    [char] and other built in types. So that it can be compared, used
    in maps, sets, hashtables, printer, etc.

    Note: this signature is pretty similiar to core's [Identifiable],
    but doesn't require [of_string] function, that is usually much
    harder to implement in comparison with [to_string] function. Also,
    instead of [to_string] it requires [pp] function that can be
    implemented much more efficiently and elegantly. From the [pp]
    function the whole plethora of printing functions are derived:
    [str], [pps], [ppo].

*)
module type S = sig
  type t with bin_io, sexp, compare
  include Printable            with type t := t
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
end

module Make(M : sig
    type t with bin_io, sexp, compare
    include Pretty_printer.S with type t := t
    val hash : t -> int
    val module_name : string
  end ) : S with type t := M.t

module Printable(M : sig
    include Pretty_printer.S
    val module_name : string
  end) : Printable with type t := M.t
