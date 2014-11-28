module type K = sig
  type 'a t
  type key
  val create : unit -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val add : 'a t -> key -> 'a -> unit
  val replace : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a option
  val string_of_key : key -> string
end

module type T = sig
  type 'a t
  type key
  val init : 'a -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val output : 'a t -> string -> ('a -> string) -> unit
end

