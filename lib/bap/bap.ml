open Core_kernel

module Std = struct
  type 'a printer = Format.formatter -> 'a -> unit
  include Bap_types.Std
  include Bap_image_std
  include Bap_disasm_std
  include Bap_sema.Std
  module Event = struct
    include Bap_main_event
    include Regular.Std.Printable.Make(struct
        type t = Bap_main_event.t
        let module_name = Some "Bap.Std.Event"
        let pp = Bap_main_event.pp
      end)
  end
  module Project = Bap_project
  module Self = Bap_self.Create
  module Log = struct
    let start ?logdir:_ () =
      Event.Log.message Event.Log.Warning ~section:"main"
        "The deprecated Bap.Std.Log.start function is used. \
         This function does nothing. Use `Bap_main.init' instead"
  end
  type project = Project.t
  type event = Event.t = ..
  module Monad = Legacy.Monad
end
