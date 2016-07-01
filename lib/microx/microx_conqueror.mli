open Bap.Std

module Limit : sig
  class virtual t : object('s)
    method virtual update : 't 'p. ('p,'t) cls -> 't term -> 's
    method virtual reached : bool
  end

  val nothing : t
  val all : t list -> t
  val max_trace_length : int -> t
  val max_loop_length : int -> t
end

module Crawler : sig
  class type ['c] t = object('s)
    method save : tid -> 'c -> 's
    method visit : tid  -> 's
    method visited : Tid.Set.t
    method backtrack : ('s * 'c) option
  end

  class ['a] exponential : ['a] t

  class ['a] linear : object('s)
    inherit ['a] t
    val checkpoints : 'a Tid.Map.t
    method prune : tid -> 's
  end

  class ['a] deterministic : ['a] t

end

type limit = Limit.t
type 's crawler = 's Crawler.t

class context : limit -> 's crawler -> program term -> object('s)
    inherit Biri.context
    method limit : limit
    method with_limit : limit -> 's
    method crawler : 's crawler
    method with_crawler : 's crawler -> 's
  end

class ['a] main : object
  inherit ['a] biri
  constraint 'a = #context
end
