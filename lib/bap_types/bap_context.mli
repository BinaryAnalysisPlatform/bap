open Core_kernel
open Bap_result
open Bap_common
open Bap_bil

class t : object('s)
  method lookup : var -> result option
  method update : var -> result -> 's
  method bindings : (var * result) Sequence.t
end
