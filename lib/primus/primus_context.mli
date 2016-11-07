open Core_kernel.Std
open Bap.Std
open Monads.Std

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

  val next : t -> ('p,'t) cls -> 't term -> (t,Primus_error.t) Monad.Result.result
end

type level = Level.t


class t : ?main:sub term -> project ->
  object('s)
    inherit Biri.context
    method project : project
    method with_project : project -> 's
    method current : tid
    method level : level
    method with_level : level -> 's
  end
