module Std = struct
  include Bap_types.Std

  module Dwarf = Bap_dwarf
  module Elf = Bap_elf
  include Image_internal_std
  module Image = Bap_image
  module Plugin = Bap_plugin
  module Plugins = Bap_plugins
  type image = Image.t
  type elf = Elf.t
end
