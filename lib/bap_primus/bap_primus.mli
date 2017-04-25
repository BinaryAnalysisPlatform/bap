open Core_kernel.Std
open Bap.Std
open Monads.Std
open Format

module Std : sig
  module Primus : sig
    type error = ..
    type 'a observation
    type 'a statement
    type ('a,'e) result = ('a,'e) Monad.Result.result =
      | Ok of 'a
      | Error of 'e

    type generator

    module Observation : sig
      val provide : ?inspect:('a -> Sexp.t) -> string -> 'a observation * 'a statement

      val name : 'a observation -> string
      val inspect : 'a observation -> 'a -> Sexp.t
    end

    module Context : sig
      module Level : sig
        type nil
        type top = program

        type ('a,'b) level = {
          me : 'a term;
          up : 'b;
        }

        type level3 = (top,nil) level
        type level2 = (sub,level3) level
        type 'a level1 = ('a,level2) level
        type 'a level0 = ('a,blk level1) level

        type t =
          | Top of level3
          | Sub of level2
          | Arg of arg level1
          | Blk of blk level1
          | Phi of phi level0
          | Def of def level0
          | Jmp of jmp level0

        val to_string : t -> string

        val next : t -> ('p,'t) cls -> 't term -> (t,error) Monad.Result.result
      end

      type level = Level.t [@@deriving sexp_of]


      class t :
        ?envp: string array ->
        ?argv: string array -> ?main:sub term -> project ->
        object('s)
          inherit Biri.context
          method argv : string array
          method envp : string array
          method project : project
          method with_project : project -> 's
          method current : tid
          method level : level
          method with_level : level -> 's
        end
    end

    class type context = Context.t





    module Machine : sig
      val finished : unit observation

      module State : sig
        type ('a,'c) t
        type ('a,'c) state = ('a,'c) t
        type void
        type uuid = (void,void,void) format

        val declare :
          ?inspect:('a -> Sexp.t) ->
          uuid:uuid ->
          name:string ->
          ('c -> 'a) -> ('a,'c) t

        val inspect : ('a,'c) t -> 'a -> Sexp.t
        val name : ('a,'c) t -> string
      end

      type 'a state = ('a,Context.t) State.t

      module type State = sig
        type ('a,'e) m
        type 'a t

        val get : 'a t -> ('a,#Context.t) m
        val put : 'a t -> 'a -> (unit,#Context.t) m
        val update : 'a t -> f:('a -> 'a) -> (unit,#Context.t) m
      end

      module type S = sig
        type ('a,'e) t
        type 'a m

        module Observation : sig
          val observe : 'a observation -> ('a -> (unit,'e) t) -> (unit,'e) t
          val make : 'a statement -> 'a -> (unit,'e) t
        end

        module Syntax : sig
          include Monad.Syntax.S2 with type ('a,'e) t := ('a,'e) t
          val (>>>) : 'a observation -> ('a -> (unit,'e) t) -> (unit,'e) t
        end

        include Monad.State.Multi.S2 with type ('a,'e) t := ('a,'e) t
                                      and type 'a m := 'a m
                                      and type ('a,'e) e = 'e -> (('a, error) result * 'e) m
                                      and module Syntax := Syntax
        module Local  : State with type ('a,'e) m := ('a,'e) t
                               and type 'a t := 'a state
        module Global : State with type ('a,'e) m := ('a,'e) t
                               and type 'a t := 'a state

        include Monad.Fail.S2 with type ('a,'e) t := ('a,'e) t
                               and type 'a error = error
      end


      module type Component = functor (Machine : S) -> sig
        val init : unit -> (unit,#Context.t) Machine.t
      end

      type component = (module Component)

      module Make(M : Monad.S) : S with type 'a m = 'a M.t
      module Main(M : S) : sig
        val run : ('a,#Context.t as 'e) M.t -> 'e -> (('a,error) result * 'e) M.m
      end
      val add_component : component -> unit
    end

    type 'a state = 'a Machine.state
    type component = Machine.component

    module Interpreter : sig
      val enter_term : tid observation
      val leave_term : tid observation
      val enter_level : Context.level observation
      val leave_level : Context.level observation

      val enter_top : program term observation
      val enter_sub : sub term observation
      val enter_arg : arg term observation
      val enter_blk : blk term observation
      val enter_phi : phi term observation
      val enter_def : def term observation
      val enter_jmp : jmp term observation

      val leave_top : program term observation
      val leave_sub : sub term observation
      val leave_arg : arg term observation
      val leave_blk : blk term observation
      val leave_phi : phi term observation
      val leave_def : def term observation
      val leave_jmp : jmp term observation

      val variable_access : var observation
      val variable_read : (var * Bil.result) observation
      val variable_written : (var * Bil.result) observation

      val address_access : addr observation
      val address_read : (addr * word) observation
      val address_written : (addr * word) observation

      module Make (Machine : Machine.S) : sig
        module Biri : Biri.S
          with type ('a,'e) state = ('a,'e) Machine.t
        class ['a] t : object
          inherit ['a] Biri.t
          constraint 'a = #context
        end
      end
    end

    module Iterator : sig
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
    end

    module Generator : sig
      type t = generator [@@deriving sexp_of]

      val create :
        (module Iterator.Infinite
          with type t = 'a
           and type dom = int) -> 'a -> t

      val static : int -> t

      val unfold : ?min:int -> ?max:int -> ?seed:int ->
        f:('a * int -> 'a * int) -> 'a -> t

      module Random : sig
        val lcg : ?min:int -> ?max:int -> int -> t
        val byte : int -> t
        module Seeded : sig
          val create : (int -> t) -> t
          val lcg : ?min:int -> ?max:int -> unit -> t
          val byte : t
        end
      end

      module Make( Machine : Machine.S) : sig
        val next : t -> (int,#Context.t) Machine.t
      end
    end


    module Linker : sig
      type name = [
        | `tid of tid
        | `addr of addr
        | `symbol of string
      ] [@@deriving sexp_of]

      module type Code = functor (Machine : Machine.S) -> sig
        val exec : (#Context.t as 'a) Biri.Make(Machine).t -> (unit,'a) Machine.t
      end

      type code = (module Code)


      module Make(Machine : Machine.S) : sig
        type ('a,'e) m = ('a,'e) Machine.t
        module Biri : Biri.S
          with type ('a,'e) state = ('a,'e) Machine.t

        val link :
          ?addr:addr ->
          ?name:string ->
          ?tid:tid ->
          code -> (unit,#Context.t) m

        val exec : name -> (#Context.t as 'a) #Biri.t -> (unit,'a) m

        val is_linked : name -> (bool,#Context.t) m
      end
    end

    module Env : sig
      module Make(Machine : Machine.S) : sig
        type ('a,'e) m = ('a,'e) Machine.t
        val get : var -> (Bil.result,#Context.t) m
        val set : var -> word -> (unit,#Context.t) m
        val add : var -> Generator.t -> (unit,#Context.t) m
      end
    end

    module Memory : sig
      val segmentation_fault : addr observation

      module Make(Machine : Machine.S) : sig
        type ('a,'e) m = ('a,'e) Machine.t

        val load : addr -> (word,#Context.t) m
        val save : addr -> word -> (unit,#Context.t) m

        val add_text : mem -> (unit,#Context.t) m
        val add_data : mem -> (unit,#Context.t) m

        val allocate :
          ?readonly:bool ->
          ?executable:bool ->
          ?generator:Generator.t ->
          addr -> int -> (unit,#Context.t) m

        val map :
          ?readonly:bool ->
          ?executable:bool ->
          mem -> (unit,#Context.t) m
      end
    end

    module Lisp : sig
      module Primitive : sig
        type 'a t
        val create : ?docs:string -> string -> (word list -> 'a) -> 'a t
      end

      module type Primitives = functor (Machine : Machine.S) ->  sig
        val defs : unit -> (Word.t,#Context.t) Machine.t Primitive.t list
      end

      type primitives = (module Primitives)

      type error += Runtime_error of string

      module Make (Machine : Machine.S) : sig
        val failf : ('a, unit, string, unit -> ('b, 'c) Machine.t) format4 -> 'a
        val link_primitives : primitives -> (unit, #Context.t) Machine.t
      end

      val init : ?log:formatter -> ?paths:string list -> string list -> unit

    end

    module Error : sig
      type t = error = ..
      val to_string : t -> string
      val add_printer : (t -> string option) -> unit
    end
  end
end
