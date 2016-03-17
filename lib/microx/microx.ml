module Std = struct
  module Concretizer = Microx_concretizer
  module Conqueror   = Microx_conqueror

  class ['a] concretizer = ['a] Concretizer.main
  class ['a] conqueror   = ['a] Conqueror.main
end
