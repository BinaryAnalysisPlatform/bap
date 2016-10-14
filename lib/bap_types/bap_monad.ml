open Core_kernel.Std
open Bap_monad_types

module Monoid = struct
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
  module Make(M : Base) = struct
    include M
    let concat = List.fold ~init:zero ~f:plus
    let (@@) = plus
  end

  module Unit : S with type t = unit = struct
    type t = unit
    let zero = ()
    let plus _ _ = ()
    let concat _ = ()
    let (@@) = plus
  end

  module Stack = struct
    module Make(T : T) : S with type t = T.t list = struct
      type t = T.t list
      let zero = []
      let plus = List.rev_append
      let (@@) = plus
      let concat = List.fold ~init:zero ~f:plus
    end
  end

  module String = struct
    module Builder : sig
      include S
      val add : t -> string -> t
      val run : t -> string
    end = struct
      module Base = struct
        type t = Rope.t
        let zero = Rope.of_string ""
        let plus = Rope.(^)
      end
      include Make(Base)
      let add t x = Rope.(t ^ of_string x)
      let run = Rope.to_string
    end

    type t = string
    let zero = ""
    let plus = (^)
    let (@@) = (^)
    let concat xs = String.concat xs
  end

  module Set = struct
    module Make(S : Set.S) = struct
      let zero = S.empty
      let plus = Set.union
      let (@@) = plus
      let concat = List.fold ~init:zero ~f:plus
    end
  end

  module List = struct
    module Make(T : T) : S with type t = T.t list = struct
      type t = T.t list
      let zero = []
      let plus = (@)
      let (@@) = (@)
      let concat = List.concat
    end
  end

  module Int = struct
    module Sum = Make(struct
        type t = int
        let zero = 0
        let plus = (+)
      end)
    module Product = Make(struct
        type t = int
        let zero = 1
        let plus = ( * )
      end)
    include Sum
  end

  module Float = struct
    module Sum = Make(struct
        type t = float
        let zero = 0.
        let plus = (+.)
      end)
    module Product = Make(struct
        type t = float
        let zero = 1.
        let plus = ( *. )
      end)
    include Sum
  end
end

module Plus = struct
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

module Fail = struct
  module type S = sig
    type error
    type 'a t
    val fail : error -> 'a t
    val catch : 'a t -> (error -> 'a t) -> 'a t
  end

  module type S2 = sig
    type error
    type ('a,'e) t
    val fail : error -> ('a,'e) t
    val catch : ('a,'e) t -> (error -> ('a,'e) t) -> ('a,'e) t
  end
end

module Choice = struct
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

  module Make(M : Basic) : S with type 'a t := 'a M.t = struct
    include M
    let accept = pure and reject = zero
    let guard c          = if c then accept () else reject ()
    let on c action      = if c then action    else accept ()
    let unless c action  = if c then accept () else action
  end

  module Make2(M : Basic2) : S2 with type ('a,'e) t := ('a,'e) M.t = struct
    include M
    let accept = pure and reject = zero
    let guard c          = if c then accept () else reject ()
    let on c action      = if c then action    else accept ()
    let unless c action  = if c then accept () else action
  end
end



module Collection = struct
  module type Basic = sig
    type 'a t
    val fold : 'a t -> init:'s -> f:('s -> 'a -> 's) -> 's
    include Plus.S   with type 'a t := 'a t
    include Monad.S with type 'a t := 'a t
  end



  module type S2 = sig
    type ('a,'e) m
    type 'a t
    val all : ('a,'e) m t -> ('a t, 'e) m
    val all_ignore : ('a,'e) m t -> (unit,'e) m
    val rev_map : 'a t -> f:('a -> ('b,'e) m) -> ('b t,'e) m
    val map : 'a t -> f:('a -> ('b,'e) m) -> ('b t,'e) m
    val iter : 'a t -> f:('a -> (unit,'e) m) -> (unit,'e) m
    val fold : 'a t -> init:'b -> f:('b -> 'a -> ('b,'e) m) -> ('b,'e) m
    val reduce : 'a t -> f:('a -> 'a -> ('a,'e) m) -> ('a option,'e) m
    val exists : 'a t -> f:('a -> (bool,'e) m) -> (bool,'e) m
    val for_all : 'a t -> f:('a -> (bool,'e) m) -> (bool,'e) m
    val count : 'a t -> f:('a -> (bool,'e) m) -> (int,'e) m
    val map_reduce : (module Monoid.S with type t = 'a) -> 'b t -> f:('b -> ('a,'e) m) -> ('a,'e) m
    val find : 'a t -> f:('a -> (bool,'e) m) -> ('a option,'e) m
    val find_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b option,'e) m
    val rev_filter : 'a t -> f:('a -> (bool,'e) m) -> ('a t, 'e) m
    val rev_filter_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b t,'e) m
    val filter : 'a t -> f:('a -> (bool,'e) m) -> ('a t,'e) m
    val filter_map : 'a t -> f:('a -> ('b option,'e) m) -> ('b t,'e) m
  end

  module type S = sig
    type 'a m
    type 'a t

    val all : 'a m t -> 'a t m
    val all_ignore : 'a m t -> unit m
    val rev_map : 'a t -> f:('a -> 'b m) -> 'b t m
    val map : 'a t -> f:('a -> 'b m) -> 'b t m
    val iter : 'a t -> f:('a -> unit m) -> unit m
    val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b m) -> 'b m
    val reduce : 'a t -> f:('a -> 'a -> 'a m) -> 'a option m
    val exists : 'a t -> f:('a -> bool m) -> bool m
    val for_all : 'a t -> f:('a -> bool m) -> bool m
    val count : 'a t -> f:('a -> bool m) -> int m
    val map_reduce : (module Monoid.S with type t = 'a) -> 'b t -> f:('b -> 'a m) -> 'a m
    val find : 'a t -> f:('a -> bool m) -> 'a option m
    val find_map : 'a t -> f:('a -> 'b option m) -> 'b option m
    val rev_filter : 'a t -> f:('a -> bool m) -> 'a t m
    val rev_filter_map : 'a t -> f:('a -> 'b option m) -> 'b t m
    val filter : 'a t -> f:('a -> bool m) -> 'a t m
    val filter_map : 'a t -> f:('a -> 'b option m) -> 'b t m
  end


end

module Monad = struct
  module type Basic = Monad.Basic
  module type S = sig
    type 'a t

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
      module Make(T : Collection.Basic) : S with type 'a t := 'a T.t
    end

    module List : Collection.S with type 'a t := 'a list
    module Seq : Collection.S with type 'a t := 'a Sequence.t


    module Syntax : sig
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
    include module type of Syntax
    include Monad.S with type 'a t := 'a t
  end

  module type S2 = sig
    type ('a,'e) t

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
      module Make(T : Collection.Basic) : S with type 'a t := 'a T.t
    end

    module List : Collection.S with type 'a t := 'a list
    module Seq : Collection.S with type 'a t := 'a Sequence.t


    module Syntax : sig
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
    include module type of Syntax
    include Monad.S2 with type ('a,'e) t := ('a,'e) t
  end

  module Make2(M : Monad.Basic2) : S2 with type ('a,'e) t := ('a,'e) M.t = struct
    include Monad.Make2(M)

    let unit = return ()

    module Lift = struct
      let nullary = return
      let unary f a = a >>| f
      let binary f a b = a >>= fun a -> b >>| fun b -> f a b
      let ternary f a b c = a >>= fun a -> b >>= fun b -> c >>| fun c -> f a b c
      let quaternary f a b c d =
        a >>= fun a -> b >>= fun b -> c >>= fun c -> d >>| fun d ->
        f a b c d
      let quinary f a b c d e =
        a >>= fun a -> b >>= fun b -> c >>= fun c -> d >>= fun d -> e >>| fun e ->
        f a b c d e

      module Syntax = struct
        let (!!) = nullary
        let (!$) = unary
        let (!$$) = binary
        let (!$$$) = ternary
        let (!$$$$) = quaternary
        let (!$$$$$) = quinary
      end
    end

    open Lift.Syntax

    module Fn = struct
      let id = return
      let nothing = return
      let ignore m = m >>| ignore
      let non f x = f x >>| not
      let apply_n_times ~n f x =
        let rec loop n x =
          if n <= 0 then return x
          else f x >>= loop (n-1) in
        loop n x

      let compose f g x = g x >>= f
    end

    module Syntax = struct
      include Monad_infix
      include Lift.Syntax
      let (>=>) g f = Fn.compose f g
    end
    open Syntax

    module Pair = struct
      let fst x = !$fst x
      let snd x = !$snd x
    end

    module Triple = struct
      let fst x = !$fst3 x
      let snd x = !$snd3 x
      let trd x = !$trd3 x
    end

    module Exn = struct
      let expect ?(finally=Fn.nothing) ~f ~catch =
        let invoke f x = f x >>= fun x -> finally () >>= fun () -> return x in
        try invoke f () with exn -> invoke catch exn
    end

    module Collection  = struct
      module type S = Collection.S2 with type ('a,'e) m := ('a,'e) M.t
      module Make(T : Collection.Basic) : Collection.S2
        with type 'a t := 'a T.t
         and type ('a,'e) m = ('a,'e) M.t = struct
        type ('a,'e) m = ('a,'e) M.t
        type 'a t = 'a T.t
        type 'a c = 'a t

        let rev xs = T.fold xs ~init:(T.zero ()) ~f:(fun ys x ->
          T.plus (T.return x) ys)

        let fold xs ~init ~f =
          T.fold xs ~init:(return init) ~f:(fun acc x ->
              acc >>= fun acc -> f acc x)

        let map xs ~f =
          let empty = T.zero () in
          fold xs ~init:empty ~f:(fun ys x ->
              f x >>| fun x -> T.plus (T.return x) ys) >>| rev

        let rev_map xs ~f =
          let empty = T.zero () in
          fold xs ~init:empty ~f:(fun ys x ->
              f x >>| fun x -> T.plus (T.return x) ys)

        let map xs ~f =
          rev_map xs ~f >>| rev

        let all = map ~f:ident

        let iter xs ~f = fold xs ~init:() ~f:(fun () x -> f x)


        let all_ignore = iter ~f:Fn.ignore

        let reduce xs ~f = fold xs ~init:None ~f:(fun acc y ->
            match acc with
            | None -> return (Some y)
            | Some x -> f x y >>| fun z -> Some z)

        let exists xs ~f =
          with_return (fun {return=finish} ->
              iter xs ~f:(fun x ->
                  f x >>| function
                  | true -> finish (return true)
                  | false -> ()) >>| fun () -> false)

        let for_all xs ~f = !$not @@ exists xs ~f:(Fn.non f)

        let count xs ~f =
          fold xs ~init:0 ~f:(fun n x ->
              f x >>| function
              | true -> n+1
              | false -> n)

        let map_reduce (type a) (module M : Monoid.S with type t = a) xs ~f =
          fold xs ~init:M.zero ~f:(fun x y -> (f y >>| M.plus x))

        let find_map xs ~f =
          with_return (fun {return=found} ->
              iter xs ~f:(fun x -> f x >>= function
                | None -> return ()
                | x -> found (return x)) >>= fun () ->
              return None)

        let find xs ~f =
          let f x  = f x >>| function
            | true -> Some x
            | false -> None  in
          find_map xs ~f

        let rev_filter_map xs ~f =
          let empty = T.zero () in
          fold xs ~init:empty ~f:(fun ys x -> f x >>| function
            | None -> ys
            | Some y -> T.plus (T.return y) ys)

        let filter_map xs ~f = rev_filter_map xs ~f >>| rev

        let rev_filter xs ~f = rev_filter_map xs ~f:(fun x ->
            f x >>| function
            | true -> Some x
            | false -> None)

        let filter xs ~f = rev_filter xs ~f >>| rev
      end

    end
    module List = Collection.Make(struct
        include List
        let zero () = []
        let plus = (@)
      end)

    module Seq = Collection.Make(struct
        include Sequence
        let zero () = Sequence.empty
        let plus = Sequence.append
      end)
    include Syntax
  end


  module Make(M : Monad.Basic) : S with type 'a t := 'a M.t = struct
    include Make2(struct
        type ('a, 'e) t = 'a M.t
        include (M : Monad.Basic with type 'a t := 'a M.t)
      end)
  end

end

module Ident
  : Monad.S with type 'a t = 'a
= struct
  type 'a t = 'a
  module M = Monad.Make(struct
      type 'a t = 'a
      let return = ident
      let bind m f = m |> f
      let map = `Custom (fun x ~f -> f x)
    end)


  let unit = ()
  module Fn = struct
    let ignore = ignore
    let nothing () = ()
    include Fn
  end

  module Pair = struct
    let fst = fst
    let snd = snd
  end

  module Triple = struct
    let fst = fst3
    let snd = snd3
    let trd = trd3
  end

  module Exn = M.Exn

  module Lift = struct
    let nullary = ident
    let unary = ident
    let binary = ident
    let ternary = ident
    let quaternary = ident
    let quinary = ident
  end

  module Collection = struct
    module type S = Collection.S with type 'a m := 'a t
    module Make(T : Collection.Basic) : S with type 'a t := 'a T.t = struct
      include M.Collection.Make(T)
      let all = ident
      let all_ignore = ignore
      let iter xs ~f = fold xs ~init:() ~f:(fun () x -> f x)
      let fold = fold
    end
  end

  module List = struct
    module Base = Collection.Make(struct
        include List
        let zero () = []
        let plus = (@)
      end)
    include (Base : Collection.S with type 'a t := 'a list)
    include List
    let all = ident
    let all_ignore = ignore
  end

  module Seq = struct
    module Base = Collection.Make(struct
        include Sequence
        let zero () = Sequence.empty
        let plus = Sequence.append
      end)
    include (Base : Collection.S with type 'a t := 'a Sequence.t)
    include Sequence
    let all = ident
    let all_ignore = ignore
  end

  module Syntax = struct
    let (>>=) x f = x |> f
    let (>>|) x f = x |> f
    let (>=>) f g x = g (f x)
    let (!!) = ident
    let (!$) = ident
    let (!$$) = ident
    let (!$$$) = ident
    let (!$$$$) = ident
    let (!$$$$$) = ident
  end

  module Let_syntax = struct
    module Let_syntax = struct
      let return = ident
      let bind x f = x |> f
      let map x ~f = x |> f
      let both x y = x,y
      module Open_on_rhs = struct let return = ident end
      module Open_in_body = struct let return = ident end
    end
  end

  let all_ignore = ignore
  let all = ident
  let ignore_m = ignore
  let join = ident
  module Monad_infix = Syntax
  include Let_syntax.Let_syntax
  include Syntax
end




module OptionT = struct
  module type S = sig
    type 'a t
    type 'a m
    val unwrap : 'a t -> 'a option m

    val lift : 'a m -> 'a t
    include Monad.S   with type 'a t := 'a t
    include Choice.S  with type 'a t := 'a t
    include Fail.S    with type 'a t := 'a t
    include Plus.S    with type 'a t := 'a t
  end

  module type S2 = sig
    type ('a,'e) t
    type ('a,'e) m
    val lift : ('a,'e) m -> ('a,'e) t
    val unwrap : ('a,'e) t -> ('a option,'e) m
    include Monad.S2   with type ('a,'e) t := ('a,'e) t
    include Choice.S2  with type ('a,'e) t := ('a,'e) t
    include Fail.S2    with type ('a,'e) t := ('a,'e) t
    include Plus.S2    with type ('a,'e) t := ('a,'e) t
  end

  module Make(M : Monad.S) :
    S with type 'a t = 'a option M.t
       and type 'a m = 'a M.t
  = struct
    open M.Syntax
    module Basic = struct
      type 'a m = 'a M.t
      type 'a t = 'a option m
      let return x = Option.return x |> M.return
      let bind m f = M.bind m (function
          | Some r -> f r
          | None -> M.return None)
      let map = `Define_using_bind
    end
    type error = unit
    let fail () = M.return None
    let unwrap = ident

    let catch m f = m >>= function
      | None -> f ()
      | other -> M.return other

    let plus m1 m2 = m1 >>= function
      | Some m1 -> M.return (Some m1)
      | None -> m2

    include Choice.Make(struct
        type 'a t = 'a Basic.t
        let pure    = Basic.return
        let zero () = M.return None
      end)

    include Basic
    include Monad.Make(Basic)
    let lift = M.map ~f:Option.return
  end


  module Make2(M : Monad.S2)
    : S2 with type ('a,'e) t = ('a option,'e) M.t
          and type ('a,'e) m = ('a,'e) M.t
  = struct
    open M.Syntax
    module Basic = struct
      type ('a,'e) m = ('a,'e) M.t
      type ('a,'e) t = ('a option,'e) m
      let return x = Option.return x |> M.return
      let bind m f = M.bind m (function
          | Some r -> f r
          | None -> M.return None)
      let map = `Define_using_bind
    end
    type error = unit
    let fail () = M.return None
    let unwrap = ident

    let catch m f = m >>= function
      | None -> f ()
      | other -> M.return other

    let plus m1 m2 = m1 >>= function
      | Some m1 -> M.return (Some m1)
      | None -> m2

    include Choice.Make2(struct
        type ('a,'e) t = ('a,'e) Basic.t
        let pure    = Basic.return
        let zero () = M.return None
      end)
    include Basic
    include Monad.Make2(Basic)
    let lift = M.map ~f:Option.return
  end

  include Make(Ident)
end


module ListT = struct
  module type S = sig
    type 'a t
    type 'a m
    val unwrap : 'a t -> 'a option m

    val lift : 'a m -> 'a t
    include Monad.S   with type 'a t := 'a t
    include Choice.S  with type 'a t := 'a t
    include Fail.S    with type 'a t := 'a t
    include Plus.S    with type 'a t := 'a t
  end

  module type S2 = sig
    type ('a,'e) t
    type ('a,'e) m
    val lift : ('a,'e) m -> ('a,'e) t
    val unwrap : ('a,'e) t -> ('a option,'e) m
    include Monad.S2   with type ('a,'e) t := ('a,'e) t
    include Choice.S2  with type ('a,'e) t := ('a,'e) t
    include Fail.S2    with type ('a,'e) t := ('a,'e) t
    include Plus.S2    with type ('a,'e) t := ('a,'e) t
  end
end
module ListM = struct
  module type S = sig
  end
end


module Writer = struct

  module type S = sig
    type 'a t
    type state
    val write : state -> unit t
    val read : 'a t -> state t
    val listen : 'a t -> ('a * state) t
    val run : 'a t -> ('a * state)
    val exec : unit t -> state
    val ignore : 'a t -> unit t
  end

  module Make(T : Monoid.S)(M : Monad.S) = struct
    open M.Syntax
    let (+) = T.plus
    module Base = struct
      type 'a writer = Writer of ('a * T.t)
      type 'a t = 'a writer M.t
      let writer x = Writer x
      let return x = M.return @@ writer (x,T.zero)
      let bind m f =
        m >>= fun (Writer (x,a)) -> f x >>| fun (Writer (x,b)) ->
        writer (x,a+b)
      let map m ~f = m >>| fun (Writer (x,e)) -> writer (f x,e)
      let map = `Custom map
    end
    include Base

    let returnw x = M.return @@ writer x
    let write x = M.return @@  writer ((), x)
    let read m = m >>= fun (Writer (x,e)) -> returnw (e,e)
    let listen m = m >>= fun (Writer (x,e)) ->  returnw ((x,e),e)
    let run m = m >>| fun (Writer (x,e)) -> (x,e)
    let exec m = m >>| fun (Writer ((),e)) -> e
    let ignore m = m >>= fun (Writer (_,e)) -> returnw ((),e)
    include Monad.Make(Base)
  end

end

module Reader = struct
  module Make(M : Monad.S) = struct
    module Base = struct
      open M.Monad_infix
      type ('a,'e) t = Reader of ('e -> 'a M.t)
      let (=>) (Reader run) x = run x
      let reader comp = Reader comp
      let return x = reader @@ fun _ -> M.return x
      let bind m f = reader @@ fun s -> m => s >>= fun x -> f x => s
      let map m ~f = reader @@ fun s -> m => s >>| f
      let map = `Custom map
    end
    include Base
    include Monad.Make2(Base)
  end
  include Make(Ident)
end

module State = struct
  module Make(M : Monad.S) = struct
    open M.Monad_infix
    type ('a,'b) storage = {
      x : 'a;
      s : 'b;
    }
    type ('a,'s) state =
        State of ('s -> ('a, 's) storage M.t)
    let make run = State run
    let (=>) (State run) x = run x
    type 'a result = 'a M.t
    module Basic = struct
      type nonrec ('a,'s) t = ('a,'s) state
      let return x = make @@ fun s -> M.return {x;s}
      let bind m f = make @@ fun s -> m=>s >>= fun {x;s} -> f x => s
      let map m ~f = make @@ fun s -> m=>s >>| fun {x;s} -> {x=f x;s}
      let map = `Custom map
    end
    let put s    = make @@ fun _ -> M.return {x=();s}
    let get ()   = make @@ fun s -> M.return {x=s;s}
    let gets f   = make @@ fun s -> M.return {x=f s;s}
    let update f = make @@ fun s -> M.return {x=();s = f s}
    let modify m f =
      make @@ fun s -> m=>s >>= fun {x;s} -> M.return {x; s = f s}
    let run m s = M.(m => s >>| fun {x;s} -> (x,s))
    let eval m s = M.(run m s >>| fst)
    let exec m s = M.(run m s >>| snd)
    let lift m = make @@ fun s ->
      M.bind m (fun x -> M.return {x;s})
    include Basic
    include Monad.Make2(Basic)
  end
  module type S = State
  include Make(Ident)
end

(* module Option = struct *)
(*   (\* add guard, when, unless *\) *)
(*   module Make(M : Monad.S) = struct end *)
(* end *)


module T = struct


  module Option = struct
    module Make(M : Monad.S) = struct
      type 'a t = 'a option M.t
      include Monad.Make(struct
          type nonrec 'a t = 'a t
          let return x : 'a t = Option.return x |> M.return
          let bind m f : 'b t  = M.bind m (function
              | Some r -> f r
              | None -> M.return None)
          let map = `Define_using_bind
        end)
      let lift (m : 'a option) : 'a t = M.return m
    end
    module Make2(M : Monad.S2) = struct
      type ('a,'b) t = ('a option,'b) M.t
      include Monad.Make2(struct
          type nonrec ('a,'b) t = ('a,'b) t
          let return x  = Option.return x |> M.return
          let bind m f   = M.bind m (function
              | Some r -> f r
              | None -> M.return None)
          let map = `Define_using_bind
        end)
      let lift (m : 'a option) : ('a,'b) t = M.return m
    end
  end


  module Or_error = struct
    module Make(M : Monad.S) = struct
      type 'a t = 'a Or_error.t M.t
      include Monad.Make(struct
          type nonrec 'a t = 'a t
          let return x = Or_error.return x |> M.return
          let bind m f = M.bind m (function
              | Ok r -> f r
              | Error err -> M.return (Error err))
          let map = `Define_using_bind
        end)
      let lift m = M.return m
    end
    module Make2(M : Monad.S2) = struct
      type ('a,'b) t = ('a Or_error.t,'b) M.t
      include Monad.Make2(struct
          type nonrec ('a,'b) t = ('a,'b) t
          let return x = Or_error.return x |> M.return
          let bind m f = M.bind m (function
              | Ok r -> f r
              | Error err -> M.return (Error err))
          let map = `Define_using_bind
        end)
      let lift m = M.return m
    end
  end


  module Result = struct
    module Make(M : Monad.S) = struct
      type ('a,'e) t = ('a,'e) Result.t M.t
      include Monad.Make2(struct
          type nonrec ('a,'e) t = ('a,'e) t
          let return x = Result.return x |> M.return
          let bind m f = M.bind m (function
              | Ok r -> f r
              | Error err -> M.return (Error err))
          let map = `Define_using_bind
        end)
      let lift m : ('a,'e) t = M.return m
    end
  end

  module State = struct
    module Make = StateT
  end
end
