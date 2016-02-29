include Bap_trace_event_types
include Bap_trace_meta_types
module Event = Bap_trace_events
module Meta = Bap_trace_meta
module Trace = Bap_trace
let () = Bap_trace_binprot.register ()
