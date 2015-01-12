type how = [ `Parallel | `Sequential ] with sexp_of

module type Monad_sequence = sig
  type 'a monad
  type 'a t

  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b monad) -> 'b monad
  val fold  : 'a t -> init:'b -> f:(       'b -> 'a -> 'b monad) -> 'b monad

  (** default [how] is [`Sequential] *)
  val init       : ?how:how ->  int -> f:(int       ->         'a monad) -> 'a t monad
  val iter       : ?how:how -> 'a t -> f:(       'a ->       unit monad) -> unit monad
  val iteri      : ?how:how -> 'a t -> f:(int -> 'a ->       unit monad) -> unit monad
  val map        : ?how:how -> 'a t -> f:(       'a ->         'b monad) -> 'b t monad
  val filter     : ?how:how -> 'a t -> f:(       'a ->       bool monad) -> 'a t monad
  val filter_map : ?how:how -> 'a t -> f:(       'a  -> 'b option monad) -> 'b t monad

  val find     : 'a t -> f:('a -> bool      monad) -> 'a option monad
  val find_map : 'a t -> f:('a -> 'b option monad) -> 'b option monad

  val all      : 'a   monad t -> 'a t monad
  val all_unit : unit monad t -> unit monad
end
