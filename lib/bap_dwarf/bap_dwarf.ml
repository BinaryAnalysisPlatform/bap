module Std = struct
  module Leb128 = Dwarf_leb128
  module Dwarf = struct
    include Dwarf_types
    module Fbi = Dwarf_fbi
    module Data = Dwarf_data
    module Buffer = Data.Buffer
    module Fn = Fbi.Fn
    type fn = Fn.t [@@deriving bin_io, compare, sexp]
  end
end
