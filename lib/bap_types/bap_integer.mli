open Bap_integer_intf

module type Base = Base
module type S = S

(** Derives [S] from [R]  *)
module Make(R : Base) : S with type t = R.t
