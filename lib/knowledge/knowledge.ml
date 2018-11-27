open Core_kernel
open Monads.Std

module Domain = Knowledge_domain
module Label = Knowledge_label
module Semantics = Knowledge_semantics

type semantics = Semantics.t
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


let provide tag id info =
  update @@ fun {data} -> {
    data = Map.update data id ~f:(function
        | None -> Semantics.put tag Semantics.empty info
        | Some sema -> Semantics.put tag sema info)
  }

let promise tag p =
  Semantics.promise tag p

let collect tag id =
  (* we can memoize here, but let's it do later *)
  Knowledge.List.iter (Semantics.promises tag) ~f:(fun p ->
      p id >>= fun info ->
      provide tag id info) >>= fun () ->
  gets @@ fun {data} ->
  let sema = Map.find data id |> function
    | None -> Semantics.empty
    | Some sema -> sema in
  Semantics.get tag sema

include Knowledge
type 'a knowledge = 'a t
type 'a data = ('a,Label.t -> 'a t) Semantics.data

let declare = Semantics.declare

let run x s = match State.run x s with
  | Ok x,s -> Ok (x,s)
  | Error err,_ -> Error err
