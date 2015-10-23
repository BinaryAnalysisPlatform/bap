
module Trace = struct
  include Bap_trace
  include Bap_trace_event_types
  include Bap_trace_meta_types
  module Event = Bap_trace_events
  module Meta = Bap_trace_meta
end

let () = Bap_trace_binprot.register ()
