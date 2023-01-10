open Core_kernel[@@warning "-D"]
open Bap_result
open Bap_common_types
open Bap_bil

class t : object('s)
  method lookup : var -> result option
  method update : var -> result -> 's
  method bindings : (var * result) Sequence.t
end
