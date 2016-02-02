

module Std_bytes = Bytes

open Core_kernel.Std

type t = Std_bytes.t with bin_io, compare, sexp

include module type of Std_bytes with type t := t
include Blit.S         with type t := t
include Container.S0   with type t := t
include Identifiable.S with type t := t

