open Core_kernel

module type Base = sig
  type t
  type dom
  val min : dom
  val max : dom
  val value : t -> dom
end

module type Finite = sig
  include Base
  val next : t -> t option
end

module type Infinite = sig
  include Base
  val next : t -> t
end

type ('a,'e) finite = (module Finite with type t = 'a and type dom = 'e)
type ('a,'e) infinite = (module Infinite with type t = 'a and type dom = 'e)

type ('a,'e) iterator =
  | Fin of ('a,'e) finite
  | Inf of ('a,'e) infinite

type ('a,'e) t = ('a,'e) iterator

module Finite = struct
  module type S = Finite
  type ('a,'e) t = ('a,'e) finite
  let enum : type a e. (a,e) t -> a -> e Sequence.t =
    fun (module G) gen -> Sequence.unfold ~init:(Some gen) ~f:(function
        | None -> None
        | Some gen -> Some (G.value gen, G.next gen))

  let create : type a e. (a,e) finite -> (a,e) iterator =
    fun g -> Fin g

end

module Infinite = struct
  module type S = Infinite
  type ('a,'e) t = ('a,'e) infinite

  let enum : type a e. (a,e) t -> a -> e Sequence.t =
    fun (module G) init
      -> Sequence.unfold ~init ~f:(fun g -> Some (G.value g,G.next g))

  let create : type a e. (a,e) infinite -> (a,e) iterator =
    fun g -> Inf g
end

let enum = function
  | Fin g -> Finite.enum g
  | Inf g -> Infinite.enum g


type ('a,'e) base = (module Base with type t = 'a and type dom = 'e)

let base : type a e. (a,e) t -> (a,e) base = function
  | Fin (module G) -> (module G)
  | Inf (module G) -> (module G)


let base_min : type a e. (a,e) base -> e = fun (module G) -> G.min
let base_max : type a e. (a,e) base -> e = fun (module G) -> G.max
let base_val : type a e. (a,e) base -> a -> e = fun (module G) g -> G.value g
let min g = base_min (base g)
let max g = base_max (base g)
let value g = base_val (base g)
