open Core_kernel.Std

module Types = Monads_types
module Monoid = Monads_monoid

module type Basic = Types.Monad.Basic
module type Basic2 = Types.Monad.Basic2

module type S = Types.Monad.S
module type S2 = Types.Monad.S2
module type Core = Monad.S
module type Core2 = Monad.S2
module type Minimal = Types.Monad.Minimal
module type Minimal2 = Types.Monad.Minimal2
module type Monad = S
module type Monad2 = S2
module Collection : module type of Types.Collection

module Make(M : Basic) : Monad with type 'a t := 'a M.t
module Make2(M : Basic2) : Monad2 with type ('a,'s) t := ('a,'s) M.t

module Core(M : Core) : Monad with type 'a t = 'a M.t
module Core2(M : Core2) : Monad2 with type ('a,'e) t = ('a,'e) M.t
module Minimal(M : Minimal) : Monad with type 'a t = 'a M.t
module Minimal2(M : Minimal2) : Monad2 with type ('a,'e) t = ('a,'e) M.t

module Plus : module type of Types.Plus
module Fail : module type of Types.Fail
module Trans : module type of Types.Trans
module Choice : sig
  include module type of Types.Choice
  module Make(M : Basic) : S with type 'a t := 'a M.t
  module Make2(M : Basic2) : S2 with type ('a,'e) t := ('a,'e) M.t
end

module Ident : Monad with type 'a t = 'a

module Option : sig
  include module type of Types.Option
  module T1(M : T1) : sig
    type 'a t = 'a option M.t
    type 'a m = 'a M.t
    type 'a e = 'a t
  end

  module T2(M : T2) : sig
    type ('a,'e) t = ('a option, 'e) M.t
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) e = ('a,'e) t
  end

  module Make(M : Monad) :
    S with type 'a m := 'a T1(M).m
       and type 'a t := 'a T1(M).t
       and type 'a e := 'a T1(M).e

  module Make2(M : Monad2) :
    S2 with type ('a,'e) t := ('a,'e) T2(M).t
        and type ('a,'e) m := ('a,'e) T2(M).m
        and type ('a,'e) e := ('a,'e) T2(M).e
end

module Result : sig
  type ('a,'e) result = ('a,'e) Result.t =
    | Ok of 'a
    | Error of 'e
  include module type of Types.Result
  module T1(T : T)(M : Monad) : sig
    type error = T.t
    type 'a m = 'a M.t
    type 'a t = ('a,error) result m
    type 'a e = ('a,error) result m
  end

  module T2(M : Monad) : sig
    type 'a m = 'a M.t
    type ('a,'e) t = ('a,'e) result m
    type ('a,'e) e = ('a,'e) result m
  end

  module Make(T : T)(M : Monad) : S
    with type 'a t := 'a T1(T)(M).t
     and type 'a m := 'a T1(T)(M).m
     and type 'a e := 'a T1(T)(M).e
     and type err := T.t

  module Make2(M : Monad) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type 'a m     := 'a     T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e

  module Error : sig
    include S
      with type 'a t = 'a Or_error.t
       and type 'a m = 'a
       and type 'a e = 'a Or_error.t
       and type err := Error.t

    module T(M : Monad) : sig
      type 'a m = 'a M.t
      type 'a t = 'a Or_error.t m
      type 'a e = 'a Or_error.t m
    end

    module Make(M : Monad) : S
      with type 'a t := 'a T(M).t
       and type 'a m := 'a T(M).m
       and type 'a e := 'a T(M).e
       and type err := Error.t
  end

  module Exception : sig
    include S
      with type 'a t = ('a,exn) Result.t
       and type 'a e = ('a,exn) Result.t
       and type 'a m = 'a
       and type err := exn

    module T(M : Monad) : sig
      type 'a m = 'a M.t
      type 'a t = ('a,exn) Result.t m
      type 'a e = ('a,exn) Result.t m
    end
    module Make(M : Monad) : S
      with type 'a t := 'a T(M).t
       and type 'a m := 'a T(M).m
       and type 'a e := 'a T(M).e
       and type err := exn
  end
end

module List : sig
  include module type of Types.List

  include S with type 'a t = 'a list
             and type 'a m = 'a
             and type 'a e = 'a list
  module T1(M : T1) : sig
    type 'a t = 'a list M.t
    type 'a m = 'a M.t
    type 'a e = 'a t
  end

  module Make(M: Monad)
    : S with type 'a m := 'a T1(M).m
         and type 'a t := 'a T1(M).t
         and type 'a e := 'a T1(M).e

  module T2(M : T2) : sig
    type ('a,'e) t = ('a list, 'e) M.t
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) e = ('a,'e) t
  end

  module Make2(M : Monad2)
    : S2 with type ('a,'e) m := ('a,'e) T2(M).m
          and type ('a,'e) t := ('a,'e) T2(M).t
          and type ('a,'e) e := ('a,'e) T2(M).e
end


module Seq : sig
  include module type of Types.List

  include S with type 'a t = 'a Sequence.t
             and type 'a m = 'a
             and type 'a e = 'a Sequence.t
  module T1(M : T1) : sig
    type 'a t = 'a Sequence.t M.t
    type 'a m = 'a M.t
    type 'a e = 'a t
  end

  module Make(M: Monad)
    : S with type 'a m := 'a T1(M).m
         and type 'a t := 'a T1(M).t
         and type 'a e := 'a T1(M).e

  module T2(M : T2) : sig
    type ('a,'e) t = ('a Sequence.t, 'e) M.t
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) e = ('a,'e) t
  end

  module Make2(M : Monad2)
    : S2 with type ('a,'e) m := ('a,'e) T2(M).m
          and type ('a,'e) t := ('a,'e) T2(M).t
          and type ('a,'e) e := ('a,'e) T2(M).e
end

module Writer : sig
  include module type of Types.Writer

  type ('a,'b) writer

  module T1(T : Monoid.S)(M : Monad) : sig
    type state = T.t
    type 'a m = 'a M.t
    type 'a t = ('a,state) writer m
    type 'a e = ('a * state) m
  end

  module Make(T : Monoid.S)(M : Monad)
    : S with type 'a m := 'a T1(T)(M).m
         and type 'a t := 'a T1(T)(M).t
         and type 'a e := 'a T1(T)(M).e
         and type state := T1(T)(M).state


  module T2(T : Monoid.S)(M : Monad2) : sig
    type state = T.t
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) t = (('a,state) writer,'e) M.t
    type ('a,'e) e = ('a * state, 'e) m
  end

  module Make2(T : Monoid.S)(M : Monad2)
    : S2 with type ('a,'e) m := ('a,'e) T2(T)(M).m
          and type ('a,'e) t := ('a,'e) T2(T)(M).t
          and type ('a,'e) e := ('a,'e) T2(T)(M).e
          and type state := T2(T)(M).state
end

module Reader : sig
  include module type of Types.Reader

  type ('a,'b) reader

  include S2 with type ('a,'e) t = ('a,'e) reader
             and type 'a m = 'a
             and type ('a,'e) e = 'e -> 'a

  module T1(T : T)(M : Monad) : sig
    type env = T.t
    type 'a m = 'a M.t
    type 'a t = ('a m, env) reader
    type 'a e = env -> 'a m
  end

  module T2(M : Monad) : sig
    type 'a m = 'a M.t
    type ('a,'e) t = ('a m,'e) reader
    type ('a,'e) e = 'e -> 'a m
  end

  module Make(T : T)(M : Monad): S
    with type 'a t := 'a T1(T)(M).t
     and type 'a m := 'a T1(T)(M).m
     and type 'a e := 'a T1(T)(M).e
     and type env := T.t

  module Make2(M : Monad) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type 'a m     := 'a     T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
end


module State : sig
  include module type of Types.State
  type ('a,'e) storage
  type ('a,'e) state

  include S2 with type ('a,'e) t = (('a,'e) storage, 'e) state
              and type 'a m = 'a
              and type ('a,'e) e = 'e -> ('a * 'e)
  val eval : ('a,'e) t -> 'e -> 'a
  val exec : ('a,'e) t -> 'e -> 'e

  module T1(T : T)(M : Monad) : sig
    type env = T.t
    type 'a m = 'a M.t
    type 'a t = (('a,env) storage m, env) state
    type 'a e = env -> ('a * env) m
  end

  module T2(M : Monad) : sig
    type 'a m = 'a M.t
    type ('a,'e) t = (('a,'e) storage m, 'e) state
    type ('a,'e) e = 'e -> ('a * 'e) m
  end

  module Make(T : T)(M : Monad): S
    with type 'a t := 'a T1(T)(M).t
     and type 'a m := 'a T1(T)(M).m
     and type 'a e := 'a T1(T)(M).e
     and type env := T.t

  module Make2(M : Monad) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type 'a m     := 'a     T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e

  module Multi : sig
    include module type of Types.Multi
    type 'a contexts

    type id
    module Id : Identifiable with type t = id

    module T1(T : T)(M : Monad) : sig
      type env = T.t
      type 'a m = 'a M.t
      type 'a t = (('a,env contexts) storage m, env contexts) state
      type 'a e = env -> ('a * env) m
    end

    module T2(M : Monad) : sig
      type 'a m = 'a M.t
      type ('a,'e) t = (('a,'e contexts) storage m, 'e contexts) state
      type ('a,'e) e = 'e -> ('a * 'e) m
    end

    module Make(T : T)(M : Monad): S
      with type 'a t := 'a T1(T)(M).t
       and type 'a m := 'a T1(T)(M).m
       and type 'a e := 'a T1(T)(M).e
       and type env := T.t
       and type id := id
       and module Id := Id

    module Make2(M : Monad) : S2
      with type ('a,'e) t := ('a,'e) T2(M).t
       and type 'a m     := 'a     T2(M).m
       and type ('a,'e) e := ('a,'e) T2(M).e
       and type id := id
       and module Id := Id
  end
end

module Fun : sig
  include module type of Types.Fun
  type 'a thunk

  include S with type 'a t = 'a thunk
             and type 'a m = 'a
             and type 'a e = 'a

  module T1(M : Monad) : sig
    type 'a m = 'a M.t
    type 'a t = 'a m thunk
    type 'a e = 'a m
  end

  module Make(M : Monad) : S
    with type 'a t := 'a T1(M).t
     and type 'a m := 'a T1(M).m
     and type 'a e := 'a T1(M).e

  module T2(M : Monad2) : sig
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) t = ('a,'e) m thunk
    type ('a,'e) e = ('a,'e) m
  end

  module Make2(M : Monad2) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type ('a,'e) m := ('a,'e) T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
end

module Lazy : sig
  include module type of Types.Lazy
  include S with type 'a t = 'a Lazy.t
             and type 'a m = 'a
             and type 'a e = 'a
  module T1(M : Monad) : sig
    type 'a m = 'a M.t
    type 'a t = 'a m Lazy.t
    type 'a e = 'a m
  end

  module Make(M : Monad) : S
    with type 'a t := 'a T1(M).t
     and type 'a m := 'a T1(M).m
     and type 'a e := 'a T1(M).e


  module T2(M : Monad2) : sig
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) t = ('a,'e) m Lazy.t
    type ('a,'e) e = ('a,'e) m
  end

  module Make2(M : Monad2) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type ('a,'e) m := ('a,'e) T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
end

module Cont : sig
  include module type of Types.Cont

  type ('a,'r) cont

  include S2 with type ('a,'r) t = ('a,'r) cont
              and type 'a m = 'a
              and type ('a,'e) e = ('a -> 'e) -> 'e
  module T1(T : T)(M : Monad) : sig
    type r = T.t
    type 'a m = 'a M.t
    type 'a t = ('a,r m) cont
    type 'a e = ('a -> r m) -> r m
  end

  module T2(M : Monad) : sig
    type 'a m = 'a M.t
    type ('a,'e) t = ('a, 'e m) cont
    type ('a,'e) e = ('a -> 'e m) -> 'e m
  end

  module Make(T : T)(M : Monad): S
    with type 'a t := 'a T1(T)(M).t
     and type 'a m := 'a T1(T)(M).m
     and type 'a e := 'a T1(T)(M).e
     and type r := T.t

  module Make2(M : Monad) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type 'a m     := 'a     T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
end
