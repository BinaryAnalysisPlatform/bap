open Core_kernel.Std

module Std : sig
  module Monoid : sig
    module type Base = sig
      type t
      val zero : t
      val plus : t -> t -> t
    end

    module type S = sig
      include Base
      val concat : t list -> t
      val (@@) : t -> t -> t
    end

    module Make(M : Base) : S with type t := M.t

    module TList(T : T) : sig type t = T.t list end

    module Stack : sig
      module Make (T : T) : S with type t := TList(T).t
    end

    module List : sig
      module Make (T : T) : S with type t := TList(T).t
    end

    module String : S with type t = string

    module Set : sig
      module Make(S : Set.S) : S with type t := S.t
    end

    module Int : sig
      include S with type t = int
      module Sum : S with type t = int
      module Product : S with type t = int
    end

    module Float : sig
      include S with type t = float
      module Sum : S with type t = float
      module Product : S with type t = float
    end
  end

  module Monad : sig

    module Plus : sig
      module type S = sig
        type 'a t
        val zero : unit -> 'a t
        val plus : 'a t -> 'a t -> 'a t
      end

      module type S2 = sig
        type ('a,'e) t
        val zero : unit -> ('a,'e) t
        val plus : ('a,'e) t -> ('a,'e) t -> ('a,'e) t
      end
    end

    module Fail : sig
      module type S = sig
        type 'a error
        type 'a t
        val fail : _ error -> 'a t
        val catch : 'a t -> (_ error -> 'a t) -> 'a t
      end

      module type S2 = sig
        type 'a error
        type ('a,'e) t
        val fail : 'e error -> ('a,'e) t
        val catch : ('a,'e) t -> ('e error -> ('a,'e) t) -> ('a,'e) t
      end
    end

    module Choice : sig
      module type Basic = sig
        type 'a t
        val pure : 'a -> 'a t
        val zero : unit -> 'a t
      end

      module type S = sig
        type 'a t
        include Basic with type 'a t := 'a t
        val accept : 'a -> 'a t
        val reject : unit -> 'a t
        val guard : bool -> unit t
        val on : bool -> unit t -> unit t
        val unless : bool -> unit t -> unit t
      end

      module type Basic2 = sig
        type ('a,'e) t
        val pure : 'a -> ('a,'e) t
        val zero : unit -> ('a,'e) t
      end

      module type S2 = sig
        type ('a,'e) t
        include Basic2 with type ('a,'e) t := ('a,'e) t
        val accept : 'a -> ('a,'e) t
        val reject : unit -> ('a,'e) t
        val guard : bool -> (unit,'e) t
        val on : bool -> (unit,'e) t -> (unit,'e) t
        val unless : bool -> (unit,'e) t -> (unit,'e) t
      end

      module Make(M : Basic) : S with type 'a t := 'a M.t
      module Make2(M : Basic2) : S2 with type ('a,'e) t := ('a,'e) M.t
    end

    module Trans : sig
      module type S = sig
        type 'a t
        type 'a m
        type 'a e                    (* essence or effect *)

        val lift : 'a m -> 'a t
        val run : 'a t -> 'a e
      end

      module type S1 = sig
        type ('a,'e) t
        type 'a m
        type ('a,'e) e
        val lift : 'a m -> ('a,'e) t
        val run : ('a,'e) t -> ('a,'e) e
      end

      module type S2 = sig
        type ('a,'e) t
        type ('a,'e) m
        type ('a,'e) e

        val lift : ('a,'e) m -> ('a,'e) t
        val run : ('a,'e) t -> ('a,'e) e
      end
    end

    module type Basic = sig
      type 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val return : 'a -> 'a t
      val map : [ `Define_using_bind
                | `Custom of ('a t -> f:('a -> 'b) -> 'b t)
                ]
    end

    module type Basic2 = sig
      type ('a, 'e) t
      val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : [ `Define_using_bind
                | `Custom of (('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t)
                ]
      val return : 'a -> ('a, _) t
    end

    module Collection : sig
      module type Basic = sig
        type 'a t
        val return : 'a -> 'a t
        include Plus.S   with type 'a t := 'a t
      end

      module type Eager = sig
        include Basic
        val fold : 'a t -> init:'s -> f:('s -> 'a -> 's) -> 's
      end

      module type Delay = sig
        include Basic
        val fold : 'a t -> init:'s -> f:('s -> 'a -> ('s -> 'r) -> 'r) -> (('s -> 'r) -> 'r)
      end

      module type S2 = sig
        type ('a,'e) m
        type 'a t
        val all : ('a,'e) m t -> ('a t, 'e) m
        val all_ignore : ('a,'e) m t -> (unit,'e) m
        val sequence : (unit,'e) m t -> (unit,'e) m
        val map : 'a t -> f:('a -> ('b,'e) m) -> ('b t,'e) m
        val iter : 'a t -> f:('a -> (unit,'e) m) -> (unit,'e) m
        val fold : 'a t -> init:'b -> f:('b -> 'a -> ('b,'e) m) -> ('b,'e) m
        val fold_left  : 'a t -> init:'b -> f:('b -> 'a -> ('b,'e) m) -> ('b,'e) m
        val fold_right : 'a t -> f:('a -> 'b -> ('b,'e) m) -> init:'b -> ('b,'e) m
        val reduce : 'a t -> f:('a -> 'a -> ('a,'e) m) -> ('a option,'e) m
        val exists : 'a t -> f:('a -> (bool,'e) m) -> (bool,'e) m
        val for_all : 'a t -> f:('a -> (bool,'e) m) -> (bool,'e) m
        val count : 'a t -> f:('a -> (bool,'e) m) -> (int,'e) m
        val map_reduce : (module Monoid.S with type t = 'a) -> 'b t -> f:('b -> ('a,'e) m) -> ('a,'e) m
        val find : 'a t -> f:('a -> (bool,'e) m) -> ('a option,'e) m
        val find_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b option,'e) m
        val filter : 'a t -> f:('a -> (bool,'e) m) -> ('a t,'e) m
        val filter_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b t,'e) m
      end

      module type S = sig
        type 'a m
        type 'a t

        val all : 'a m t -> 'a t m
        val all_ignore : 'a m t -> unit m
        val sequence : unit m t -> unit m
        val map : 'a t -> f:('a -> 'b m) -> 'b t m
        val iter : 'a t -> f:('a -> unit m) -> unit m
        val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b m) -> 'b m
        val fold_left : 'a t -> init:'b -> f:('b -> 'a -> 'b m) -> 'b m
        val fold_right : 'a t -> f:('a -> 'b -> 'b m) -> init:'b -> 'b m
        val reduce : 'a t -> f:('a -> 'a -> 'a m) -> 'a option m
        val exists : 'a t -> f:('a -> bool m) -> bool m
        val for_all : 'a t -> f:('a -> bool m) -> bool m
        val count : 'a t -> f:('a -> bool m) -> int m
        val map_reduce : (module Monoid.S with type t = 'a) -> 'b t -> f:('b -> 'a m) -> 'a m
        val find : 'a t -> f:('a -> bool m) -> 'a option m
        val find_map : 'a t -> f:('a -> 'b option m) -> 'b option m
        val filter : 'a t -> f:('a -> bool m) -> 'a t m
        val filter_map : 'a t -> f:('a -> 'b option m) -> 'b t m
      end
    end

    module Syntax : sig
      module type S = sig
        type 'a t
        val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
        val (>>|) : 'a t -> ('a -> 'b) -> 'b t
        val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
        val (!!)  : 'a -> 'a t
        val (!$)   : ('a -> 'b) -> ('a t -> 'b t)
        val (!$$)  : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)
        val (!$$$) : ('a -> 'b -> 'c -> 'd) -> ('a t -> 'b t -> 'c t -> 'd t)
        val (!$$$$) : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t)
        val (!$$$$$) : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t)
      end

      module type S2 = sig
        type ('a,'e) t
        val (>>=) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
        val (>>|) : ('a,'e) t -> ('a -> 'b) -> ('b,'e) t
        val (>=>) : ('a -> ('b,'e) t) -> ('b -> ('c,'e) t) -> ('a -> ('c,'e) t)
        val (!!) : 'a -> ('a,'e) t
        val (!$)   : ('a -> 'b) -> (('a,'e) t -> ('b,'e) t)
        val (!$$)  : ('a -> 'b -> 'c) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t)
        val (!$$$) : ('a -> 'b -> 'c -> 'd) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t -> ('d,'e) t)
        val (!$$$$) : ('a -> 'b -> 'c -> 'd -> 'e) -> (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t)
        val (!$$$$$) : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
          (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t -> ('f,'s) t)
      end
    end
    module type S = sig
      type 'a t

      val void : 'a t -> unit t
      val sequence : unit t list -> unit t
      val forever : 'a t -> 'b t

      module Fn : sig
        val id : 'a -> 'a t
        val ignore : 'a t -> unit t
        val nothing : unit -> unit t
        val non : ('a -> bool t) -> 'a -> bool t
        val apply_n_times : n:int -> ('a -> 'a t) -> 'a -> 'a t
        val compose : ('b -> 'c t) -> ('a -> 'b t) -> ('a -> 'c t)
      end

      module Pair : sig
        val fst: ('a * 'b) t -> 'a t
        val snd: ('a * 'b) t -> 'b t
      end

      module Triple : sig
        val fst : ('a * 'b * 'c) t -> 'a t
        val snd : ('a * 'b * 'c) t -> 'b t
        val trd : ('a * 'b * 'c) t -> 'c t
      end

      module Lift : sig
        val nullary : 'a -> 'a t
        val unary   : ('a -> 'b) -> ('a t -> 'b t)
        val binary  : ('a -> 'b -> 'c) -> ('a t -> 'b t -> 'c t)
        val ternary : ('a -> 'b -> 'c -> 'd) -> ('a t -> 'b t -> 'c t -> 'd t)
        val quaternary : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t)
        val quinary : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t)
      end

      module Exn : sig
        val expect : ?finally:(unit -> unit t) -> f:(unit -> 'a t) -> catch:(exn -> 'a t) -> 'a t
      end

      module Collection : sig
        module type S = Collection.S with type 'a m := 'a t
        module Eager(T : Collection.Eager) : S with type 'a t := 'a T.t
        module Delay(T : Collection.Delay) : S with type 'a t := 'a T.t
      end

      module List : Collection.S with type 'a t := 'a list
      module Seq : Collection.S with type 'a t := 'a Sequence.t


      include Syntax.S with type 'a t := 'a t
      include Monad.S with type 'a t := 'a t
      module Syntax : Syntax.S with type 'a t := 'a t
    end

    module type S2 = sig
      type ('a,'e) t

      val void : ('a,'e) t -> (unit,'e) t
      val sequence : (unit,'e) t list -> (unit,'e) t
      val forever : ('a,'e) t -> ('b,'e) t


      module Fn : sig
        val id : 'a -> ('a,'e) t
        val ignore : ('a,'e) t -> (unit,'e) t
        val nothing : unit -> (unit,'e) t
        val non : ('a -> (bool,'e) t) -> 'a -> (bool,'e) t
        val apply_n_times : n:int -> ('a -> ('a,'e) t) -> 'a -> ('a,'e) t
        val compose : ('b -> ('c,'e) t) -> ('a -> ('b,'e) t) -> ('a -> ('c,'e) t)
      end

      module Pair : sig
        val fst: (('a * 'b),'e) t -> ('a,'e) t
        val snd: (('a * 'b),'e) t -> ('b,'e) t
      end

      module Triple : sig
        val fst : (('a * 'b * 'c),'e) t -> ('a,'e) t
        val snd : (('a * 'b * 'c),'e) t -> ('b,'e) t
        val trd : (('a * 'b * 'c),'e) t -> ('c,'e) t
      end

      module Lift : sig
        val nullary : 'a -> ('a,'e) t
        val unary   : ('a -> 'b) -> (('a,'e) t -> ('b,'e) t)
        val binary  : ('a -> 'b -> 'c) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t)
        val ternary : ('a -> 'b -> 'c -> 'd) -> (('a,'e) t -> ('b,'e) t -> ('c,'e) t -> ('d,'e) t)
        val quaternary : ('a -> 'b -> 'c -> 'd -> 'e) -> (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t)
        val quinary : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
          (('a,'s) t -> ('b,'s) t -> ('c,'s) t -> ('d,'s) t -> ('e,'s) t -> ('f,'s) t)
      end

      module Exn : sig
        val expect :
          ?finally:(unit -> (unit,'s) t) ->
          f:(unit -> ('a,'s) t) ->
          catch:(exn -> ('a,'s) t) -> ('a,'s) t
      end

      module Collection : sig
        module type S = Collection.S2 with type ('a,'e) m := ('a,'e) t
        module Eager(T : Collection.Eager) : S with type 'a t := 'a T.t
        module Delay(T : Collection.Delay) : S with type 'a t := 'a T.t
      end

      module List : Collection.S with type 'a t := 'a list
      module Seq : Collection.S with type 'a t := 'a Sequence.t


      include Syntax.S2 with type ('a,'e) t := ('a,'e) t
      include Monad.S2 with type ('a,'e) t := ('a,'e) t
      module Syntax : Syntax.S2 with type ('a,'e) t := ('a,'e) t
    end

    module type Core = Monad.S
    module type Core2 = Monad.S2

    module type Minimal = sig
      type 'a t
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
    end

    module type Minimal2 = sig
      type ('a,'e) t
      val return : 'a -> ('a,'e) t
      val bind : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
    end

    module Make(M : Basic) : S with type 'a t := 'a M.t
    module Make2(M : Basic2) : S2 with type ('a,'s) t := ('a,'s) M.t
    module Core(M : Core) : S with type 'a t = 'a M.t
    module Core2(M : Core2) : S2 with type ('a,'e) t = ('a,'e) M.t
    module Minimal(M : Minimal) : S with type 'a t = 'a M.t
    module Minimal2(M : Minimal2) : S2 with type ('a,'e) t = ('a,'e) M.t

    module type Monad = S
    module type Monad2 = S2

    module Ident : Monad with type 'a t = 'a

    module Option : sig
      module type S = sig
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Choice.S  with type 'a t := 'a t
        include Plus.S    with type 'a t := 'a t
        include Fail.S    with type 'a t := 'a t
                           and type 'a error = unit
      end

      module type S2 = sig
        include Trans.S2
        include Monad2   with type ('a,'e) t := ('a,'e) t
        include Choice.S2  with type ('a,'e) t := ('a,'e) t
        include Plus.S2    with type ('a,'e) t := ('a,'e) t
        include Fail.S2    with type ('a,'e) t := ('a,'e) t
                            and type 'a error = unit
      end

      include S with type 'a t = 'a option
                 and type 'a m = 'a
                 and type 'a e = 'a option

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
      module type S = sig
        type err
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Fail.S  with type 'a t := 'a t with type 'a error = err
      end

      module type S2 = sig
        include Trans.S1
        include Monad2 with type ('a,'e) t := ('a,'e) t
        include Fail.S2  with type ('a,'e) t := ('a,'e) t with type 'a error = 'a
      end

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
        module type S = sig
          include S
          val failf : ('a, Format.formatter, unit, unit -> 'b t) format4 -> 'a
        end
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


        include S with type 'a t = 'a Or_error.t
                   and type 'a m = 'a
                   and type 'a e = 'a Or_error.t
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
      module type S = sig
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Choice.S  with type 'a t := 'a t
        include Plus.S    with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2   with type ('a,'e) t := ('a,'e) t
        include Choice.S2  with type ('a,'e) t := ('a,'e) t
        include Plus.S2    with type ('a,'e) t := ('a,'e) t
      end

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
      module type S = sig
        include Trans.S
        include Monad   with type 'a t := 'a t
        include Choice.S  with type 'a t := 'a t
        include Plus.S    with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2   with type ('a,'e) t := ('a,'e) t
        include Choice.S2  with type ('a,'e) t := ('a,'e) t
        include Plus.S2    with type ('a,'e) t := ('a,'e) t
      end

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
      module type S = sig
        type state
        include Trans.S
        val write : state -> unit t
        val read : 'a t -> state t
        val listen : 'a t -> ('a * state) t
        val exec : unit t -> state m
        val ignore : 'a t -> unit t
        val lift : 'a m -> 'a t
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        type state
        include Trans.S2
        val write : state -> (unit,'e) t
        val read : ('a,'e) t -> (state,'e) t
        val listen : ('a,'e) t -> (('a * state),'e) t
        val exec : (unit,'e) t -> (state,'e) m
        val ignore : ('a,'e) t -> (unit,'e) t
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

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

      module type S = sig
        include Trans.S
        type env
        val read : unit -> env t
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S1
        val read : unit -> ('e,'e) t
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

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

      type ('a,'e) storage
      type ('a,'e) state

      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
        type env
        val put : env -> unit t
        val get : unit -> env t
        val gets : (env -> 'r) -> 'r t
        val update : (env -> env) -> unit t
        val modify : 'a t -> (env -> env) -> 'a t
      end

      module type S2 = sig
        include Trans.S1
        include Monad2 with type ('a,'s) t := ('a,'s) t
        val put : 's -> (unit,'s) t
        val get : unit -> ('s,'s) t
        val gets : ('s -> 'r) -> ('r,'s) t
        val update : ('s -> 's) -> (unit,'s) t
        val modify : ('a,'s) t -> ('s -> 's) -> ('a,'s) t
      end

      module Multi : sig
        type status = [`Current | `Live | `Dead]

        type 'a contexts

        type id
        module Id : sig
          include Identifiable with type t = id
          val pp : Format.formatter -> id -> unit
        end

        module type S = sig
          include Trans.S
          type id

          module Id : Identifiable.S with type t = id

          val global : id

          val fork : unit -> unit t
          val switch : id -> unit t
          val parent : unit -> id t
          val ancestor : id list -> id t
          val current : unit -> id t
          val kill : id -> unit t
          val forks : unit -> id Sequence.t t
          val status : id -> status t

          include S with type 'a t := 'a t
                     and type 'a e := 'a e
                     and type 'a m := 'a m
        end

        module type S2 = sig
          include Trans.S1
          type id

          module Id : Identifiable.S with type t = id


          val global : id

          val fork : unit -> (unit,'e) t
          val switch : id -> (unit,'e) t
          val parent : unit -> (id,'e) t
          val ancestor : id list -> (id,'e) t
          val current : unit -> (id,'e) t
          val kill : id -> (unit,'e) t
          val forks : unit -> (id Sequence.t,'e) t
          val status : id -> (status,'e) t

          include S2 with type ('a,'e) t := ('a,'e) t
                      and type ('a,'e) e := ('a,'e) e
                      and type 'a m := 'a m
        end

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
    end

    module Fun : sig
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

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
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
      end

      module type S2 = sig
        include Trans.S2
        include Monad2 with type ('a,'e) t := ('a,'e) t
      end

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
      module type S = sig
        include Trans.S
        include Monad with type 'a t := 'a t
        type  r
        val call : cc:(('a -> _ t) -> 'a t) -> 'a t
      end

      module type S2 = sig
        include Trans.S1
        include Monad2 with type ('a,'e) t := ('a,'e) t
        val call : cc:(('a -> (_,'e) t) -> ('a,'e) t) -> ('a,'e) t
      end
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
  end
end
