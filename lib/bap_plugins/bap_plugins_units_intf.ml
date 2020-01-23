type reason = [
  | `In_core
  | `Provided_by of string
  | `Requested_by of string
]


module type S = sig
  val init : unit -> unit
  val list : unit -> string list
  val record : string -> reason -> unit
  val lookup : string -> reason option
end
