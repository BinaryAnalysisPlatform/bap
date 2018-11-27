open Core_kernel
open Monads.Std

module Domain = Knowledge_domain
module Label = Knowledge_label
module Semantics = Knowledge_semantics

type semantics = Semantics.t
type 'a domain = 'a Semantics.domain
type label = Label.t
type conflict = ..

module Conflict = struct
  type t = conflict = ..
end

module Base = struct
  type t = {
    data : semantics Label.Map.t;
  }
end

type state = Base.t
let empty = {Base.data = Label.Map.empty}

module State = struct
  include Monad.State.T1(Base)(Monad.Ident)
  include Monad.State.Make(Base)(Monad.Ident)
end

module Knowledge = struct
  type 'a t = ('a,conflict) result State.t
  include Monad.Result.Make(Conflict)(State)
end

open Knowledge.Syntax

let declare = Semantics.declare

let get () = Knowledge.lift (State.get ())
let put s = Knowledge.lift (State.put s)
let gets f = Knowledge.lift (State.gets f)
let update f = Knowledge.lift (State.update f)


let provide : type a. a domain -> label -> a -> _ =
  fun tag id info ->
    update @@ fun {data} -> {
      data = Map.update data id ~f:(function
          | None -> Semantics.put tag Semantics.empty info
          | Some sema -> Semantics.put tag sema info)
    }

let collect tag id=
  gets @@ fun {data} ->
  let sema = Map.find data id |> function
    | None -> Semantics.empty
    | Some sema -> sema in
  Semantics.get tag sema

include Knowledge
type 'a knowledge = 'a t

let run x s = match State.run x s with
  | Ok x,s -> Ok (x,s)
  | Error err,_ -> Error err
