module Std = struct
  module C = struct
    module Abi  = Bap_c_abi
    module Attr = Bap_c_attr
    module Data = Bap_c_data
    module Size = Bap_c_size
    module Type = Bap_c_type
    module Parser = Bap_c_parser
    include Bap_c_term_attributes
  end
end
