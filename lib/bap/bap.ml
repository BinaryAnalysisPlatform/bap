open Core_kernel.Std

module Std = struct
  type 'a printer = Format.formatter -> 'a -> unit
  include Bap_types.Std
  include Bap_image_std
  include Bap_disasm_std
  include Bap_sema.Std
  module Event = Bap_event
  module Project = Bap_project
  module Self = Bap_self.Create
  module Log = Bap_log
  type project = Project.t
  type event = Event.t = ..
  module Monad = Legacy.Monad
  type 'a param = 'a Bap_self.param
  module Service = struct
    include Bap_service
    include Bap_services
  end
end
