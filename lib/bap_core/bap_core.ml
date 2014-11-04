module Std = struct
  include Bap_types.Std

  module Dwarf = Bap_dwarf
  module Elf = Bap_elf
  module Image = Bap_image
  module Memory = Image.Mem
  module Memory_exn = Image.Mem_exn
  module Plugin = Bap_plugin
  module Plugins = Bap_plugins

  type memory = Image.mem
  type memory_exn = Image.mem_exn
  type image = Image.t
  type elf = Elf.t
end
