
module Std : sig
  module Plugin_rules : sig
    val install : unit -> unit
  end

  module Plugin_options : sig
    val set : unit -> unit
  end
end
