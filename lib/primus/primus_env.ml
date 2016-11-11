open Core_kernel.Std
open Bap.Std
open Primus_types

module Context = Primus_context
module Observation = Primus_observation
module Generator = Primus_generator


type error += Undefined_var of var

let undefined_variable,undefined =
  Observation.provide ~inspect:sexp_of_var "undefined-variable"



type t = {
  values : word Var.Map.t;
  random : Generator.t Var.Map.t;
}

let state = Primus_machine.State.declare
    ~uuid:"44b24ea4-48fa-47e8-927e-f7ba65202743"
    ~name:"environment" (fun _ -> {
          values = Var.Map.empty;
          random = Var.Map.empty;
        })

let inspect_environment {values;random} =
  let keys =
    Set.union
      (Var.Set.of_list (Map.keys values))
      (Var.Set.of_list (Map.keys random)) in
  let sexp_of_var sexp_of_value var value = Sexp.(List [
      Atom (Var.name var);
      sexp_of_value value;
      Atom (Type.to_string (Var.typ var))
    ]) in
  let sexp_of_word x = Sexp.Atom (Word.string_of_value x) in
  let sexp_of_policy = Generator.sexp_of_t in
  let bindings =
    Set.fold keys ~init:[] ~f:(fun acc var ->
        match Map.find values var with
        | Some value -> sexp_of_var sexp_of_word var value :: acc
        | None -> match Map.find random var with
          | Some policy -> sexp_of_var sexp_of_policy var policy ::acc
          | None -> assert false)  in
  Sexp.List bindings

    let word = Word.of_int ~width:8


module Make(Machine : Machine) = struct
    open Machine.Syntax
    type ('a,'e) m = ('a,'e) Machine.t

    module Generator = Primus_generator.Make(Machine)

    let add var policy =
      Machine.Local.update state ~f:(fun s -> {
        s with random = Map.add s.random ~key:var ~data:policy
      })

    let set var word =
      Machine.Local.update state ~f:(fun s -> {
            s with values = Map.add s.values ~key:var ~data:word
          })

    let get var =
      Machine.Local.get state >>= fun t ->
      match Map.find t.values var with
      | Some value -> Machine.return value
      | None -> match Map.find t.random var with
        | None ->
          Machine.Observation.make undefined var >>= fun () ->
          Machine.fail (Undefined_var var)
        | Some gen -> Generator.next gen >>| word

end
