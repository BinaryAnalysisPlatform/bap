(** C language support library.

    This library brings support for C Abstract Machine. In particular
    it adds a fairly complete support for C type system, data model,
    and ABI.

    [open Bap_c.Std]  module to use this library, it only defines one
    module [C] that includes the rest of the library. It also defines
    an interface for parser, that maybe provided by a third party.
*)

module Std = struct
  module C = struct
    module Abi  = Bap_c_abi
    module Attr = Bap_c_attr
    module Data = Bap_c_data
    module Size = Bap_c_size
    module Type = struct
      include Bap_c_type
      module Mapper = struct
        include Bap_c_type_mapper_intf
        include Bap_c_type_mapper
      end
      include Bap_c_type_printer
    end
    module Parser = Bap_c_parser
    include Bap_c_term_attributes
  end
end
