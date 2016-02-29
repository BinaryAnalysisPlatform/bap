open Core_kernel.Std
open Regular_data_types
open Regular_data_intf

module type Printable = sig
  type t
  val to_string : t -> string
  val str : unit -> t -> string
  val pps : unit -> t -> string
  val ppo : out_channel -> t -> unit
  val pp_seq : Format.formatter -> t Sequence.t -> unit
  include Pretty_printer.S     with type t := t
end

module type S = sig
  type t with bin_io, sexp, compare
  include Printable            with type t := t
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
  include Data with type t := t
end

module Make(M : sig
    type t with bin_io, sexp, compare
    include Pretty_printer.S with type t := t
    include Versioned with type t := t
    val hash : t -> int
    val module_name : string option
  end ) : S with type t := M.t

module Printable(M : sig
    include Pretty_printer.S
    val module_name : string option
  end) : Printable with type t := M.t
