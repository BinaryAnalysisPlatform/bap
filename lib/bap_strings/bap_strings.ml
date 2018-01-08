module Std = struct
  module Strings = struct
    module Detector = Bap_strings_detector
    module Unscrambler = Bap_strings_unscrambler
    module Scanner = Bap_strings_scanner
    module Index = Bap_strings_index
  end
end
