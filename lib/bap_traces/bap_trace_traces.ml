open Core_kernel.Std

module Trace = Bap_trace

let traces = Trace.Id.Table.create ()

let add t = Hashtbl.set traces ~key:(Trace.id t) ~data:t
let remove t = Hashtbl.remove traces (Trace.id t)
let to_list () = Hashtbl.data traces
let enum () = to_list () |> Sequence.of_list
