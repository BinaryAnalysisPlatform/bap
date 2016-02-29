open Regular_data_intf

type 'a t

val create :
  load:(string -> 'a option) ->
  save:(string -> 'a -> unit) -> 'a t

val load : 'a t -> string -> 'a option

val save : 'a t -> string -> 'a -> unit

type service = {
  create : 'a . 'a reader -> 'a writer -> 'a t
}

module Service : sig
  val provide : service -> unit
  val request : 'a reader -> 'a writer -> 'a t
end
