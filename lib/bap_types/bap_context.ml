open Core_kernel
open Bap_result
open Bap_common
open Bap_bil

type delta = result Bap_var.Map.t

class t = object
  val delta : delta = Bap_var.Map.empty
  method lookup = Map.find delta
  method update key data = {< delta = Map.set delta ~key ~data >}
  method bindings = Map.to_sequence delta
end
