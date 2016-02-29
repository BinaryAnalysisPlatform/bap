module Std = struct
  module Elf = struct
    include Elf_types
    type t = elf
    include Elf_utils
    include Elf_parse
  end
end
