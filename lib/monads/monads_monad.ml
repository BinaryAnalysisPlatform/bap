open Core_kernel.Std

module Monoid = Monads_monoid
module Types = Monads_types
module Trans = Types.Trans
module type Monad = Types.Monad.S
module type Monad2 = Types.Monad.S2

module Plus = Types.Plus
module Fail = Types.Fail
module Choice = struct
  include Types.Choice
  module Make2(M : Basic2) : S2 with type ('a,'e) t := ('a,'e) M.t = struct
    include M
    let accept = pure and reject = zero
    let guard c          = if c then accept () else reject ()
    let on c action      = if c then action    else accept ()
    let unless c action  = if c then accept () else action
  end

  module Make(M : Basic) : S with type 'a t := 'a M.t =
    Make2(struct
      type ('a,'e) t = 'a M.t
      include (M : Basic with type 'a t := 'a M.t)
    end)
end

module Monad = struct
  include Types.Monad

  module Make2(M : Basic2)
    : S2 with type ('a,'e) t := ('a,'e) M.t
  = struct
    include Monad.Make2(M)


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
      module type S = Types.Collection.S2
        with type ('a,'e) m := ('a,'e) M.t

      module type Base = sig
        include Types.Collection.Basic
        val foldl : 'a t -> init:'b -> f:('b -> 'a -> ('b,'e) M.t) -> ('b,'e) M.t
        val foldr : 'a t -> f:('a -> 'b -> ('b,'e) M.t) -> init:'b -> ('b,'e) M.t
        val find_map : 'a t -> f:('a -> ('b option,'e) M.t) -> ('b option,'e) M.t
      end

      module Eager_base(T : Types.Collection.Eager) = struct
        include T
        let foldl xs ~init ~f =
          T.fold xs ~init:(fun k -> k init)
            ~f:(fun k x k' -> k (fun a -> f a x >>= k')) M.return

        let foldr xs ~f ~init =
          T.fold xs ~init:M.return ~f:(fun k x ->
              (fun a -> f x a >>= k)) init

        let find_map xs ~f =
          foldl xs ~init:None ~f:(fun r x -> match r with
              | None -> f x
              | r -> M.return r)
      end

      module Delay_base( T : Types.Collection.Delay) = struct
        include T
        let foldl xs ~init ~f =
          T.fold xs ~init ~f:(fun a x k -> f a x >>= k) M.return

        let fold xs ~init ~f =
          T.fold xs ~init ~f:(fun a x k -> k (f a x)) ident

        let foldr xs ~f ~init =
          fold xs ~init:M.return ~f:(fun k x a -> f x a >>= k) init

        let find_map xs ~f  =
          T.fold xs ~init:() ~f:(fun () x k -> f x >>= function
            | None -> k ()
            | x -> M.return x) (fun () -> M.return None)
      end


      module Make(T : Base) : Types.Collection.S2
        with type 'a t := 'a T.t
         and type ('a,'e) m = ('a,'e) M.t = struct
        type ('a,'e) m = ('a,'e) M.t
        type 'a t = 'a T.t
        type 'a c = 'a t


        let fold = T.foldl
        let fold_left = T.foldl
        let fold_right = T.foldr
        let find_map = T.find_map

        let prepend ys x = T.plus (T.return x) ys

        let map xs ~f =
          let empty = T.zero () in
          fold_right xs ~init:empty ~f:(fun x ys -> f x >>| prepend ys)

        let all = map ~f:ident

        let iter xs ~f = fold xs ~init:() ~f:(fun () x -> f x)

        let all_ignore = iter ~f:Fn.ignore
        let sequence = all_ignore

        let reduce xs ~f = fold xs ~init:None ~f:(fun acc y ->
            match acc with
            | None -> return (Some y)
            | Some x -> f x y >>| fun z -> Some z)

        let exists xs ~f =
          T.find_map xs ~f:(fun x -> f x >>| function
            | true -> Some ()
            | false -> None) >>| fun x -> x <> None

        let for_all xs ~f = !$not @@ exists xs ~f:(Fn.non f)

        let count xs ~f =
          fold xs ~init:0 ~f:(fun n x ->
              f x >>| function
              | true -> n+1
              | false -> n)

        let map_reduce (type a) (module M : Monoid.S with type t = a) xs ~f =
          fold xs ~init:M.zero ~f:(fun x y -> (f y >>| M.plus x))

        let find xs ~f =
          let f x  = f x >>| function
            | true -> Some x
            | false -> None  in
          find_map xs ~f

        let filter_map xs ~f =
          let empty = T.zero () in
          fold_right xs ~init:empty ~f:(fun x ys -> f x >>| function
            | None -> ys
            | Some y -> prepend ys y)

        let filter xs ~f = filter_map xs ~f:(fun x ->
            f x >>| function
            | true -> Some x
            | false -> None)

      end

      module Delay(B : Types.Collection.Delay) = struct
        module Base = Delay_base(B)
        include Make(Base)
      end

      module Eager(B : Types.Collection.Eager) = struct
        module Base = Eager_base(B)
        include Make(Base)
      end

    end


    module List = Collection.Delay(struct
        type 'a t = 'a list
        let fold xs ~init ~f =
          let rec loop xs k = match xs with
            | [] -> k
            | x :: xs -> fun k' ->  k (fun a -> loop xs (f a x) k') in
          loop xs (fun k -> k init)
        let zero () = []
        let return x = [x]
        let plus = (@)
      end)

    module Seq = Collection.Delay(struct
        type 'a t = 'a Sequence.t
        let fold xs ~init ~f finish =
          Sequence.delayed_fold xs ~init ~f:(fun a x ~k ->
            f a x k) ~finish
        let zero () = Sequence.empty
        let return = Sequence.return
        let plus = Sequence.append
      end)

    let void t = Fn.ignore t
    let sequence = List.sequence
    let rec forever t = bind t (fun _ -> forever t)
    include Syntax
  end

  module Make(M : Monad.Basic) : S with type 'a t := 'a M.t =
    Make2(struct
      type ('a, 'e) t = 'a M.t
      include (M : Monad.Basic with type 'a t := 'a M.t)
    end)

  module Core2(M : Core2) = struct
    type ('a,'e) t = ('a,'e) M.t
    include Make2(struct
        include M
        let map = `Custom map
      end)
    let join = M.join
    let ignore_m = M.ignore_m
    let all = M.all
    let all_ignore = M.all_ignore
  end

  (* the intended usage of this module is

     [F(Core(M))] to upcast a monad of type [Core]
     to a monad of type [S], to make it possible
     we shouldn't erase types from this monad, otherwise
     this functor would be much harder to use.

     Since we can't erase types, we can't implement Core
     module via the Core2. So we need to repeat the code.
     Since, the code doesn't contain any implementation (just
     renaming) it can be considered OK.

 *)
  module Core(M : Core) = struct
    type 'a t = 'a M.t
    include Make(struct
        include M
        let map = `Custom map
      end)
    let join = M.join
    let ignore_m = M.ignore_m
    let all = M.all
    let all_ignore = M.all_ignore
  end

  (* this module provides a fast and dirty translation from a minimal
     monad representation to our maximal. We will not erase types
     from the resulting structure, as this functor is expected to
     be used as a type caster, e.g. [Monad.State.Make(Monad.Minimal(M)]
 *)
  module Minimal( M : Minimal) = struct
    type 'a t = 'a M.t
    include Make(struct
        include M
        let map = `Define_using_bind
      end)
  end
  module Minimal2( M : Minimal2) = struct
    type ('a,'e) t = ('a,'e) M.t
    include Make2(struct
        include M
        let map = `Define_using_bind
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
    module type S = Types.Collection.S with type 'a m := 'a t
    module Eager(T : Types.Collection.Eager) :
      S with type 'a t := 'a T.t = struct
      include M.Collection.Eager(T)
      let all = ident
      let all_ignore = ignore
      let iter xs ~f = fold xs ~init:() ~f:(fun () x -> f x)
      let fold = fold
    end
    module Delay(T : Types.Collection.Delay) :
      S with type 'a t := 'a T.t = struct
      include M.Collection.Delay(T)
      let all = ident
      let all_ignore = ignore
      let iter xs ~f = fold xs ~init:() ~f:(fun () x -> f x)
      let fold = fold
    end
  end

  module List = struct
    module Base = Collection.Delay(struct
        type 'a t = 'a list
        let fold xs ~init ~f =
          let rec loop xs k = match xs with
            | [] -> k
            | x :: xs -> fun k' ->  k (fun a -> loop xs (f a x) k') in
          loop xs (fun k -> k init)
        let zero () = []
        let return x = [x]
        let plus = (@)
      end)
    include (Base : Collection.S with type 'a t := 'a list)
    include List
    let all = ident
    let all_ignore = ignore
  end

  module Seq = struct
    module Base = Collection.Delay(struct
        type 'a t = 'a Sequence.t
        let fold xs ~init ~f finish =
          Sequence.delayed_fold xs ~init ~f:(fun a x ~k ->
            f a x k) ~finish
        let zero () = Sequence.empty
        let return = Sequence.return
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
  let void = ignore
  let rec forever x = forever x
  let sequence = all_ignore
  module Monad_infix = Syntax
  include Let_syntax.Let_syntax
  include Syntax
end

module OptionT = struct
  include Types.Option

  module T1(M : T1) = struct
    type 'a t = 'a option M.t
    type 'a m = 'a M.t
    type 'a e = 'a t
  end

  module T2(M : T2) = struct
    type ('a,'e) t = ('a option, 'e) M.t
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) e = ('a,'e) t
  end

  module Make2(M : Monad.S2)
    : S2 with type ('a,'e) t := ('a,'e) T2(M).t
          and type ('a,'e) m := ('a,'e) T2(M).m
          and type ('a,'e) e := ('a,'e) T2(M).e
  = struct
    open M.Syntax
    module Basic = struct
      include T2(M)
      let return x = M.return (Some x)
      let bind m f = M.bind m (function
          | Some r -> f r
          | None -> M.return None)
      let map = `Define_using_bind
    end
    type 'a error = unit
    let fail () = M.return None
    let run = ident

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

  module Make(M : Monad.S)
    : S with type 'a m := 'a T1(M).m
         and type 'a t := 'a T1(M).t
         and type 'a e := 'a T1(M).e
  = Make2(struct
    type ('a,'e) t = 'a M.t
    type 'a error = unit
    include (M : Monad.S with type 'a t := 'a M.t)
  end)

  include T1(Ident)
  include Make(Ident)
end

module ResultT = struct
  include Types.Result
  type ('a,'e) result = ('a,'e) Result.t =
    | Ok of 'a
    | Error of 'e
  module type Sp = sig
    include Trans.S1
    include Monad.S2   with type ('a,'e) t := ('a,'e) t
    include Fail.S2    with type ('a,'e) t := ('a,'e) t
  end

  module Tp(T : T1)(M : Monad.S) = struct
    type 'a error = 'a T.t
    type 'a m = 'a M.t
    type ('a,'e) t = ('a,'e error) result m
    type ('a,'e) e = ('a,'e error) result m
  end

  module Makep(T : T1)(M : Monad.S) : Sp
    with type ('a,'e) t := ('a,'e) Tp(T)(M).t
     and type 'a m  := 'a Tp(T)(M).m
     and type ('a,'e) e := ('a,'e) Tp(T)(M).e
     and type 'a error = 'a Tp(T)(M).error
  = struct
    open M.Syntax
    module Base = struct
      include Tp(T)(M)
      let return x = M.return (Ok x)
      let bind m f  : ('a,'e) t = m >>= function
        | Ok r -> f r
        | Error err -> M.return (Error err)

      let fail err = M.return (Error err)
      let run = ident
      let catch m f = m >>= function
        | Error err -> f err
        | other -> M.return other

      let lift m = m >>| fun x -> Ok x
      let map = `Define_using_bind

    end
    include Base
    include Monad.Make2(Base)
  end

  module T1(T : T)(M : Monad.S) = struct
    type error = T.t
    type 'a m = 'a M.t
    type 'a t = ('a,error) result m
    type 'a e = ('a,error) result m
  end

  module T2(M : Monad.S) = struct
    type 'a m = 'a M.t
    type ('a,'e) t = ('a,'e) result m
    type ('a,'e) e = ('a,'e) result m
  end

  module Make(T : T)(M : Monad.S) : S
    with type 'a t := 'a T1(T)(M).t
     and type 'a m := 'a T1(T)(M).m
     and type 'a e := 'a T1(T)(M).e
     and type err := T.t
    = struct
      type err = T.t
      include Makep(struct type 'a t = T.t end)(M)
    end

  module Make2(M : Monad.S) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type 'a m     := 'a     T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
    = Makep(struct type 'a t = 'a end)(M)

  module Error = struct
    module T(M : Monad.S) = struct
      type 'a m = 'a M.t
      type 'a t = 'a Or_error.t m
      type 'a e = 'a Or_error.t m
    end

    module type S = sig
      include S
      val failf : ('a, Format.formatter, unit, unit -> 'b t) format4 -> 'a
    end
    module Make(M : Monad.S) : S
      with type 'a t := 'a T(M).t
       and type 'a m := 'a T(M).m
       and type 'a e := 'a T(M).e
       and type err := Error.t
      = struct
        include Make(struct type t = Error.t end)(M)

        let failf fmt =
          let open Format in
          let buf = Buffer.create 512 in
          let ppf = formatter_of_buffer buf in
          let kon ppf () =
            pp_print_flush ppf ();
            let err = Or_error.error_string (Buffer.contents buf) in
            M.return err in
          kfprintf kon ppf fmt

      end
    type 'a t = 'a Or_error.t
    type 'a m = 'a
    type 'a e = 'a Or_error.t
    include Make(Ident)
  end

  module Exception = struct
    module T(M : Monad.S) = struct
      type 'a m = 'a M.t
      type 'a t = ('a,exn) Result.t m
      type 'a e = ('a,exn) Result.t m
    end

    module Make(M : Monad.S) : S
      with type 'a t := 'a T(M).t
       and type 'a m := 'a T(M).m
       and type 'a e := 'a T(M).e
       and type err := exn
      = Make(struct type t = exn end)(M)

    include T(Ident)
    include Make(Ident)
  end

  module Self : S2
    with type ('a,'e) t = ('a,'e) result
     and type 'a m = 'a
     and type ('a,'e) e = ('a,'e) result
    = struct
      include T2(Ident)
      include Make2(Ident)
    end
    include Self
end

module ListT = struct
  include Types.List
  module T1(M : T1) = struct
    type 'a t = 'a list M.t
    type 'a m = 'a M.t
    type 'a e = 'a t
  end

  module T2(M : T2) = struct
    type ('a,'e) t = ('a list, 'e) M.t
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) e = ('a,'e) t
  end

  module Make2(M : Monad.S2)
    : S2 with type ('a,'e) m := ('a,'e) T2(M).m
          and type ('a,'e) t := ('a,'e) T2(M).t
          and type ('a,'e) e := ('a,'e) T2(M).e
  = struct
    open M.Syntax
    type 'a result = 'a list
    module Base = struct
      include T2(M)
      let return x = M.return [x]
      let bind xsm f = xsm >>= fun xs ->
        List.fold xs ~init:(!![]) ~f:(fun ysm x ->
            ysm >>= fun ys -> f x >>| fun xs ->
            List.rev_append xs @@ ys) >>| List.rev
      let map xsm ~f = xsm >>| List.map ~f
      let map = `Custom map
    end
    include Choice.Make2(struct
        type ('a,'e) t = ('a,'e) T2(M).t
        let pure x = M.return [x]
        let zero () = M.return []
      end)

    module MT = Monad.Make2(Base)
    let plus xsm ysm =
      xsm >>= fun xs -> ysm >>= fun ys ->
      M.return (xs @ ys)

    let run = ident
    let lift m = m >>| fun x -> [x]
    include MT
  end

  module Make(M: Monad.S)
    : S with type 'a m := 'a T1(M).m
         and type 'a t := 'a T1(M).t
         and type 'a e := 'a T1(M).e =
    Make2(struct
      type ('a,'e) t = 'a M.t
      include (M : Monad.S with type 'a t := 'a M.t)
    end)

  module ListM :
    S with type 'a t = 'a list and type 'a m = 'a and type 'a e = 'a list = struct
    include T1(Ident)
    include Make(Ident)
  end

  include ListM
end

module Seq = struct
  include Types.List
  module T1(M : T1) = struct
    type 'a t = 'a Sequence.t M.t
    type 'a m = 'a M.t
    type 'a e = 'a t
  end

  module T2(M : T2) = struct
    type ('a,'e) t = ('a Sequence.t, 'e) M.t
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) e = ('a,'e) t
  end

  module Make2(M : Monad.S2)
    : S2 with type ('a,'e) m := ('a,'e) T2(M).m
          and type ('a,'e) t := ('a,'e) T2(M).t
          and type ('a,'e) e := ('a,'e) T2(M).e
  = struct
    open M.Syntax
    type 'a result = 'a Sequence.t
    module Base = struct
      include T2(M)
      let return x = M.return @@ Sequence.singleton x
      let bind xsm f = xsm >>= fun xs ->
        Sequence.fold xs ~init:(!!Sequence.empty) ~f:(fun ysm x ->
            ysm >>= fun ys -> f x >>| fun xs ->
             Sequence.append xs ys)
      let map xsm ~f = xsm >>| Sequence.map ~f
      let map = `Custom map
    end

    include Choice.Make2(struct
        type ('a,'e) t = ('a,'e) T2(M).t
        let pure x = Base.return x
        let zero () = M.return Sequence.empty
      end)

    module MT = Monad.Make2(Base)
    let plus xsm ysm =
      xsm >>= fun xs -> ysm >>= fun ys ->
      M.return (Sequence.append xs ys)

    let run = ident
    let lift m = m >>| Sequence.singleton
    include MT
  end

  module Make(M : Monad.S)
      : S with type 'a m := 'a T1(M).m
           and type 'a t := 'a T1(M).t
           and type 'a e := 'a T1(M).e
    = Make2(struct
      type ('a,'e) t = 'a M.t
      include (M : Monad.S with type 'a t := 'a M.t)
    end)

  module SeqM : S
    with type 'a t = 'a Sequence.t
     and type 'a m = 'a
     and type 'a e = 'a Sequence.t =
  struct
    include T1(Ident)
    include Make(Ident)
  end
  include SeqM
end

module Writer = struct
  include Types.Writer

  type ('a,'s) writer = Writer of ('a * 's)

  module T1(T : Monoid.S)(M : Monad.S) = struct
    type state = T.t
    type 'a m = 'a M.t
    type 'a t = ('a,state) writer m
    type 'a e = ('a * state) m
  end

  module T2(T : Monoid.S)(M : Monad.S2) = struct
    type state = T.t
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) t = (('a,state) writer,'e) M.t
    type ('a,'e) e = ('a * state, 'e) m
  end

  module Make2(T : Monoid.S)(M : Monad.S2)
    : S2 with type ('a,'e) m := ('a,'e) T2(T)(M).m
          and type ('a,'e) t := ('a,'e) T2(T)(M).t
          and type ('a,'e) e := ('a,'e) T2(T)(M).e
          and type state := T2(T)(M).state
  = struct
    open M.Syntax
    let (+) = T.plus
    module Base = struct
      include T2(T)(M)
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
    let lift m = M.map m ~f:(fun x -> Writer (x,T.zero))
    include Monad.Make2(Base)
  end

  module Make(T : Monoid.S)(M : Monad.S)
    : S with type 'a m := 'a T1(T)(M).m
         and type 'a t := 'a T1(T)(M).t
         and type 'a e := 'a T1(T)(M).e
         and type state := T1(T)(M).state
  = struct
    module M = struct
      type ('a,'e) t = 'a M.t
      include (M : Monad.S with type 'a t := 'a M.t)
    end
    type state = T.t
    include Make2(T)(M)
  end
end

module Reader = struct
  include Types.Reader
  type ('a,'e) reader = Reader of ('e -> 'a)

  module type Sp = sig
    type 'a env
    include Trans.S1
    val read : unit -> ('e env, 'e ) t
    include Monad.S2 with type ('a,'e) t := ('a,'e) t
  end


  module Tp(T : T1)(M : Monad.S) = struct
    type 'a env = 'a T.t
    type 'a m = 'a M.t
    type ('a,'e) t = ('a m, 'e env) reader
    type ('a,'e) e = 'e env -> 'a m
  end

  module Makep(T : T1)(M : Monad.S) : Sp
    with type ('a,'e) t := ('a,'e) Tp(T)(M).t
     and type 'a m     := 'a     Tp(T)(M).m
     and type ('a,'e) e := ('a,'e) Tp(T)(M).e
     and type 'a env   := 'a Tp(T)(M).env
  = struct
    module Base = struct
      include Tp(T)(M)
      open M.Monad_infix
      let (=>) (Reader run) x = run x
      let reader comp = Reader comp
      let return x = reader @@ fun _ -> M.return x
      let bind m f = reader @@ fun s -> m => s >>= fun x -> f x => s
      let map m ~f = reader @@ fun s -> m => s >>| f
      let read () = reader @@ fun s -> M.return s
      let lift m  = reader @@ fun s -> m
      let run m s = m => s
      let map = `Custom map
    end
    include Base
    include Monad.Make2(Base)
  end

  module T1(T : T)(M : Monad.S) = struct
    type env = T.t
    type 'a m = 'a M.t
    type 'a t = ('a m, env) reader
    type 'a e = env -> 'a m
  end

  module T2(M : Monad.S) = struct
    type 'a m = 'a M.t
    type ('a,'e) t = ('a m,'e) reader
    type ('a,'e) e = 'e -> 'a m
  end

  module Make(T : T)(M : Monad.S): S
    with type 'a t := 'a T1(T)(M).t
     and type 'a m := 'a T1(T)(M).m
     and type 'a e := 'a T1(T)(M).e
     and type env := T.t
    = Makep(struct type 'a t = T.t end)(M)

  module Make2(M : Monad.S) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type 'a m     := 'a     T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
    = Makep(struct type 'a t = 'a end)(M)

  module Self : S2
    with type ('a,'e) t = ('a,'e) reader
     and type 'a m = 'a
     and type ('a,'e) e = 'e -> 'a
  = struct
    include T2(Ident)
    include Make2(Ident)
  end
  include Self
end


module State = struct
  include Types.State

  module type Sp = sig
    include Trans.S1
    include Monad.S2 with type ('a,'s) t := ('a,'s) t
    type 'a env
    val put : 's env -> (unit,'s) t
    val get : unit -> ('s env,'s) t
    val gets : ('s env -> 'r) -> ('r,'s) t
    val update : ('s env -> 's env) -> (unit,'s) t
    val modify : ('a,'s) t -> ('s env -> 's env) -> ('a,'s) t
    val eval : ('a,'s) t -> 's env -> 'a m
    val exec : ('a,'s) t -> 's env -> 's env m
  end


  type ('a,'b) storage = {
    x : 'a;
    s : 'b;
  }

  type ('a,'e) state = State of ('e -> 'a)


  module Tp(T : T1)(M : Monad.S) = struct
    type 'a env = 'a T.t
    type 'a m = 'a M.t
    type ('a,'e) t = (('a,'e env) storage m, 'e env) state
    type ('a,'e) e = 'e env -> ('a * 'e env) m
  end

  module Makep(T : T1)(M : Monad.S) : Sp
    with type ('a,'e) t := ('a,'e) Tp(T)(M).t
     and type 'a m     := 'a     Tp(T)(M).m
     and type ('a,'e) e := ('a,'e) Tp(T)(M).e
     and type 'a env   := 'a Tp(T)(M).env
  = struct
    open M.Monad_infix
    let make run = State run
    let (=>) (State run) x = run x
    type 'a result = 'a M.t
    module Basic = struct
      include Tp(T)(M)
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

  module T1(T : T)(M : Monad.S) = struct
    type env = T.t
    type 'a m = 'a M.t
    type 'a t = (('a,env) storage m, env) state
    type 'a e = env -> ('a * env) m
  end

  module T2(M : Monad.S) = struct
    type 'a m = 'a M.t
    type ('a,'e) t = (('a,'e) storage m, 'e) state
    type ('a,'e) e = 'e -> ('a * 'e) m
  end

  module Make(T : T)(M : Monad.S): S
    with type 'a t := 'a T1(T)(M).t
     and type 'a m := 'a T1(T)(M).m
     and type 'a e := 'a T1(T)(M).e
     and type env := T.t
    = Makep(struct type 'a t = T.t end)(M)

  module Make2(M : Monad.S) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type 'a m     := 'a     T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
    = Makep(struct type 'a t = 'a end)(M)

  module Multi = struct
    include Types.Multi
    module Id = struct
      type t = int
      let zero = Int.zero
      let succ = Int.succ
      include Identifiable.Make(struct
          type t = int [@@deriving compare, bin_io, sexp]
          let hash = Int.hash
          let module_name = "Monads.Std.Multi.Id"
          let to_string = Int.to_string
          let of_string = Int.of_string
        end)
    end
    type id = Id.t

    type 'e contexts = {
      created : id;             (* the id of last created fork *)
      current : id;             (* the id of current context   *)
      parents : id Id.Map.t;    (* tree of forks               *)
      children : Id.Set.t Id.Map.t;
      init  : 'e;                (* father of all forks         *)
      forks : 'e Id.Map.t;       (* all forks of the Father     *)
    }

    module ST1 = T1
    module ST2 = T2
    module STp = Tp

    module type Sp = sig
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

      include Sp with type ('a,'e) t := ('a,'e) t
                  and type ('a,'e) e := ('a,'e) e
                  and type 'a m := 'a m
    end

    module T1(T : T)(M : Monad.S) = struct
      type env = T.t
      type 'a m = 'a M.t
      type 'a t = (('a,env contexts) storage m, env contexts) state
      type 'a e = env -> ('a * env) m
    end

    module T2(M : Monad.S) = struct
      type 'a m = 'a M.t
      type ('a,'e) t = (('a,'e contexts) storage m, 'e contexts) state
      type ('a,'e) e = 'e -> ('a * 'e) m
    end

    module Tp(T : T1)(M : Monad.S) = struct
      type 'a env = 'a T.t
      type 'a m = 'a M.t
      type ('a,'e) t = (('a,'e env contexts) storage m, 'e env contexts) state
      type ('a,'e) e = 'e env -> ('a * 'e env) m
    end

    module Makep(T : T1)(M : Monad.S) : Sp
      with type ('a,'e) t := ('a,'e) Tp(T)(M).t
       and type ('a,'e) e := ('a,'e) Tp(T)(M).e
       and type 'a m := 'a Tp(T)(M).m
       and type 'a env := 'a Tp(T)(M).env
       and type id := id
       and module Id := Id
    = struct
      module SM = struct
        module Env = struct
          type 'a t = 'a T.t contexts
        end
        include STp(Env)(M)
        include Makep(Env)(M)
      end

      open SM.Syntax

      include Tp(T)(M)
      type id = Id.t

      let global = Id.zero
      let init ctxt = {
        init = ctxt;
        created = global;
        current = global;
        parents = Id.Map.empty;
        children = Id.Map.empty;
        forks = Id.Map.empty;
      }

      let rec alive_parent k child =
        match Map.find k.parents child with
        | None -> global
        | Some p when Map.mem k.forks p -> p
        | Some zombie -> alive_parent k zombie

      let rec ancestor k cs =
        match List.map cs ~f:(alive_parent k) with
        | [] -> global
        | p :: ps when List.for_all ps ~f:(fun p' -> p = p') -> p
        | ps -> ancestor k ps

      let ancestor cs = SM.gets @@ fun k -> ancestor k cs

      let alive k id : id  =
        if Map.mem k.forks id then id else alive_parent k id

      let forks () = SM.gets (fun k ->
          let fs = Map.to_sequence k.forks |> Seq.map ~f:fst in
          Sequence.shift_right fs global)

      let siblings () =
        SM.gets (fun k ->
            match Map.find k.children (alive_parent k k.current) with
            | None -> Id.Set.empty
            | Some cs -> Set.filter cs ~f:(Map.mem k.forks))

      let context k =
        let id = alive k k.current in
        if id = global then k.init
        else Map.find_exn k.forks k.current

      let fork () = SM.update @@ fun k ->
        let ctxt = context k in
        let current = Id.succ k.created in
        let parents = Map.add k.parents ~key:current ~data:k.current in
        let forks = Map.add k.forks ~key:current ~data:ctxt in
        {k with created=current; current; parents; forks}

      let switch id = SM.update @@ fun k -> {k with current = alive k id}
      let current () = SM.gets @@ fun k -> alive k k.current

      let get () = SM.get () >>| context
      let put ctxt = SM.update @@ fun k ->
        let id = alive k k.current in
        if id = global then {k with init=ctxt} else {
          k with forks = Map.add k.forks ~key:id ~data:ctxt
        }

      let parent () = SM.get () >>| fun k -> alive_parent k k.current

      let status id = SM.get () >>| fun k ->
        if id = k.current then `Current else
        if Map.mem k.forks id then `Live else `Dead

      let remove_dead_parents k =
        let parents =
          Map.fold k.forks ~init:Id.Map.empty ~f:(fun ~key ~data:_ ps ->
              Map.add ps ~key ~data:(alive_parent k key)) in
        let children = Map.filter_keys k.children ~f:(Map.mem parents) in
        {k with children; parents}

      let gc k =
        if k.created mod 1024 = 0 then remove_dead_parents k else k

      let kill id = SM.update @@ fun k ->
        gc {
          k with
          forks = Map.remove k.forks id;
          current = if id = k.current then alive_parent k id else id;
        }

      let gets f = get () >>| f
      let update f = get () >>= fun x -> put (f x)
      let modify m f = m >>= fun x -> update f >>= fun () -> SM.return x


      let run m = fun ctxt -> M.bind (SM.run m (init ctxt)) @@ fun (x,cs) ->
        M.return (x,cs.init)

      include Monad.Make2(struct
          type nonrec ('a,'e) t = ('a,'e) t
          let return = SM.return
          let bind = SM.bind
          let map = `Custom SM.map
        end)

      let lift m = SM.lift m
      let eval m s = M.map (run m s) ~f:fst
      let exec m s = M.map (run m s) ~f:snd
      module Id = Id
    end

    module Make(T : T)(M : Monad.S): S
      with type 'a t := 'a T1(T)(M).t
       and type 'a m := 'a T1(T)(M).m
       and type 'a e := 'a T1(T)(M).e
       and type env := T.t
       and type id := id
       and module Id := Id
      = Makep(struct type 'a t = T.t end)(M)

    module Make2(M : Monad.S) : S2
      with type ('a,'e) t := ('a,'e) T2(M).t
       and type 'a m     := 'a     T2(M).m
       and type ('a,'e) e := ('a,'e) T2(M).e
       and type id := id
       and module Id := Id
      = Makep(struct type 'a t = 'a end)(M)



    include T2(Ident)
    include Make2(Ident)
  end


  include T2(Ident)
  include Make2(Ident)
  let eval m s = fst (run m s)
  let exec m s = snd (run m s)
end

module Fun = struct
  include Types.Fun
  type 'a thunk = Thunk of (unit -> 'a)

  module T1(M : Monad.S) = struct
    type 'a m = 'a M.t
    type 'a t = 'a m thunk
    type 'a e = 'a m
  end

  module T2(M : Monad.S2) = struct
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) t = ('a,'e) m thunk
    type ('a,'e) e = ('a,'e) m
  end

  let thunk f = Thunk f

  module Make2(M : Monad.S2) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type ('a,'e) m := ('a,'e) T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
  = struct
    open M.Syntax
    module Base = struct
      include T2(M)
      let run (Thunk f) = f ()
      let return x = thunk @@ fun () -> M.return x
      let bind m f = thunk @@ fun () -> run m >>= fun x -> run (f x)
      let map m ~f = thunk @@ fun () -> run m >>| f
      let lift m   = thunk @@ fun () -> m
      let map = `Custom map
    end
    include Base
    include Monad.Make2(Base)
  end

  module Make(M : Monad.S) : S
    with type 'a t := 'a T1(M).t
     and type 'a m := 'a T1(M).m
     and type 'a e := 'a T1(M).e
    = Make2(struct
      type ('a,'e) t = 'a M.t
      include (M : Monad.S with type 'a t := 'a M.t)
    end)


  include T1(Ident)
  include Make(Ident)

end

module LazyT = struct
  include Types.Lazy
  module T1(M : Monad.S) = struct
    type 'a m = 'a M.t
    type 'a t = 'a m Lazy.t
    type 'a e = 'a m
  end

  module T2(M : Monad.S2) = struct
    type ('a,'e) m = ('a,'e) M.t
    type ('a,'e) t = ('a,'e) m Lazy.t
    type ('a,'e) e = ('a,'e) m
  end

  module Make2(M : Monad.S2) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type ('a,'e) m := ('a,'e) T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
  = struct
    open M.Syntax
    module Base = struct
      include T2(M)
      let return x = lazy (M.return x)
      let bind m f = lazy (Lazy.force m >>= fun x ->
                           Lazy.force (f x))
      let map = `Define_using_bind
      let run = Lazy.force
      let lift x = lazy x
    end
    include Base
    include Monad.Make2(Base)
  end

  module Make(M : Monad.S) : S
    with type 'a t := 'a T1(M).t
     and type 'a m := 'a T1(M).m
     and type 'a e := 'a T1(M).e
    = Make2(struct
      type ('a,'e) t = 'a M.t
      include (M : Monad.S with type 'a t := 'a M.t)
    end)

  module Self : S
    with type 'a t = 'a Lazy.t
     and type 'a m = 'a
     and type 'a e = 'a
    = struct
      type 'a t = 'a Lazy.t
      type 'a m = 'a
      type 'a e = 'a
      include Make(Ident)
    end
    include Self
end

module Cont = struct
  include Types.Cont

  type ('a,'r) cont = Cont of (('a -> 'r) -> 'r)

  module T(M : Monad.S) = struct
    type 'a m = 'a M.t
    type ('a,'e) t = ('a, 'e m) cont
    type ('a,'e) e = ('a -> 'e m) -> 'e m
  end


  let cont k = Cont k

  module Tp(T : T1)(M : Monad.S) = struct
    type 'a r = 'a T.t
    type 'a m = 'a M.t
    type ('a,'e) t = ('a, 'e r m) cont
    type ('a,'e) e = ('a -> 'e r m) -> 'e r m
  end

  module type Sp = sig
    include Trans.S1
    include Monad.S2 with type ('a,'e) t := ('a,'e) t
    type 'a r
    val call : cc:(('a -> ('r,'e r) t) -> ('a,'e r) t) -> ('a,'e r) t
  end

  module Makep(T : T1)(M : Monad.S) : Sp
    with type ('a,'e) t := ('a,'e) Tp(T)(M).t
     and type ('a,'e) e := ('a,'e) Tp(T)(M).e
     and type 'a m := 'a Tp(T)(M).m
     and type 'a r := 'a Tp(T)(M).r
  = struct
    open M.Syntax
    module Base = struct
      include Tp(T)(M)
      let run (Cont k) f = k f
      let return x = cont @@ fun k -> k x
      let lift m = cont @@ fun k -> m >>= k
      let bind m f = cont @@ fun k ->
        run m @@ fun x -> run (f x) k
      let call ~cc = cont @@ fun k ->
        run (cc (fun x -> cont @@ fun _ -> k x)) k
      let map = `Define_using_bind
    end
    include Base
    include Monad.Make2(Base)
  end

  module T1(T : T)(M : Monad.S) = struct
    type r = T.t
    type 'a m = 'a M.t
    type 'a t = ('a,r m) cont
    type 'a e = ('a -> r m) -> r m
  end

  module T2(M : Monad.S) = struct
    type 'a m = 'a M.t
    type ('a,'e) t = ('a, 'e m) cont
    type ('a,'e) e = ('a -> 'e m) -> 'e m
  end

  module Make(T : T)(M : Monad.S): S
    with type 'a t := 'a T1(T)(M).t
     and type 'a m := 'a T1(T)(M).m
     and type 'a e := 'a T1(T)(M).e
     and type r := T.t
    = Makep(struct type 'a t = T.t end)(M)

  module Make2(M : Monad.S) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type 'a m     := 'a     T2(M).m
     and type ('a,'e) e := ('a,'e) T2(M).e
    = Makep(struct type 'a t = 'a end)(M)

  module Self :
    S2 with type ('a,'e) t = ('a,'e) cont
        and type 'a m = 'a
        and type ('a,'e) e = (('a -> 'e) -> 'e)
    = struct
      type ('a,'e) t = ('a,'e) T(Ident).t
      type 'a m = 'a
      type ('a,'e) e = ('a -> 'e) -> 'e
      include Make2(Ident)
    end
  include Self
end

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
    module Make = State
  end
end

module Lazy = LazyT
module List = ListT
module Result = ResultT
module Option = OptionT
include Monad
module Collection = Types.Collection
