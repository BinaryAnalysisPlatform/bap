open Core_kernel.Std
open Bap.Std
open Primus_types

module Context = Primus_context
module Observation = Primus_observation


type error += Undefined_var of var

let undefined_variable,undefined =
  Observation.provide ~inspect:sexp_of_var "undefined-variable"


module type S = sig
  type ('a,'e) m

  module Generator : Primus_generator.S with type ('a,'e) m := ('a,'e) m
  val get : var -> (word,#Context.t) m
  val set : var -> word -> (unit,#Context.t) m
  val add : var -> Generator.policy -> (unit,#Context.t) m
end


module Make(Machine : Machine) :
  S with type ('a,'e) m := ('a,'e) Machine.t
  = struct
    open Machine.Syntax
    module Generator = Primus_generator.Make(Machine)

    type ('a,'e) m = ('a,'e) Machine.t
    type policy = Generator.policy

    type t = {
      values : word Var.Map.t;
      random : policy Var.Map.t;
    }

    let state = Machine.Local.create ~name:"environment" (fun _ -> {
          values = Var.Map.empty;
          random = Var.Map.empty;
        })

    let add var policy =
      Machine.Local.update state ~f:(fun s -> {
        s with random = Map.add s.random ~key:var ~data:policy
      })

    let set var word =
      Machine.Local.update state ~f:(fun s -> {
            s with values = Map.add s.values ~key:var ~data:word
          })



    let word = Word.of_int ~width:8

    let rec generate var = function
      | `static value -> Machine.return value
      | `random (Some value) -> Generator.next value >>| word
      | `random None ->
        Generator.Seeded.byte () >>= fun rng ->
        let policy = `random (Some rng) in
        add var policy >>= fun () ->
        generate var policy

    let get var =
      Machine.Local.get state >>= fun t ->
      match Map.find t.values var with
      | Some value -> Machine.return value
      | None -> match Map.find t.random var with
        | None ->
          Machine.Observation.make undefined var >>= fun () ->
          Machine.fail (Undefined_var var)
        | Some policy ->
          generate var policy >>= fun value ->
          set var value >>= fun () ->
          Machine.return value
end
