type reason = [
  | `In_core
  | `Provided_by of string
  | `Requested_by of string
]


module type S = sig
  val init : unit Lazy.t

  val record : string -> reason -> unit
  val lookup : string -> reason option
end
