open Core_kernel

module Std = struct

  type 'a printer = Format.formatter -> 'a -> unit

  module Bytes = Regular_bytes
  module Regular = Regular_regular
  module Opaque = Regular_opaque
  module Printable = struct
    module type S = Regular.Printable
    module Make = Regular.Printable
  end
  module type Opaque = Opaque.S
  module type Regular = Regular.S
  module type Printable = Printable.S

  include Regular_data_intf

  module Data = struct
    include Regular_data_intf
    include Regular_data_types
    include Regular_data
    module type S = Data
    module Write = Regular_data_write
    module Read = Regular_data_read
    module Cache = Regular_cache
  end

  module Seq = struct
    include Sequence
    include Regular_seq
  end
  include Seq.Export

  type 'a seq = 'a Seq.t [@@deriving bin_io, compare, sexp]
end
