module Std = struct
  include Bap_types.Std

  module Dwarf = Bap_dwarf
  module Elf = Bap_elf
  include Image_internal_std
  module Image = Bap_image
  module Section = Image.Sec
  module Symbol  = Image.Sym
  module Plugin = Bap_plugin
  module Plugins = Bap_plugins

  module Disasm = Bap_disasm


  type image = Image.t
  type elf = Elf.t
end
