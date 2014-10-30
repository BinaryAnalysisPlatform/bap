include Dwarf_types
module Fbi = Dwarf_fbi
module Data = Dwarf_data
module Buffer = Data.Buffer
module Fn = Fbi.Fn

type fn = Fn.t with bin_io, compare, sexp
