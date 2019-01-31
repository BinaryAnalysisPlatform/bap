open Core_kernel
open Bap.Std
open Monads.Std
open Bap_c_type_mapper_intf


(** include visitor/mapper with the monad stripped away. *)
include S with type ('a,'e) m = 'a

(** Search Monad.

    A monad for searching with abnormal exit, i.e., the computation
    terminates as soon as an item is found.  *)
module Search : sig
  include Monad.S2


  (** [finished needle] is called when a search is finished, it will
      terminate the search with the [needle] as a result.  *)
  val finished : 'e -> ('a,'e) t


  (** [result s] runs the computation [s] and extracts the result.   *)
  val result : ('a,'e) t -> 'e option
end


(** the mapper lifted into a regular state monad.   *)
module State : S with type ('a,'e) m = ('a,'e) Monad.State.t


(** the visitor lifted into the search monad.

    For example, the following code will find the first pointer:

    {[
      module Search = C.Type.Mapper.Search

      let find_pointer t = Search.result @@ (object
          inherit [C.Type.t] C.Type.Mapper.Finder.base
          method! enter_pointer = Search.finished
        end)#run t
    ]} *)
module Finder : S with type ('a,'e) m = ('a,'e) Search.t


(** [Make(M)] lifts the visitor into monad [M].  *)
module Make( M : Monad.S2) : S with type ('a,'e) m = ('a,'e) M.t
