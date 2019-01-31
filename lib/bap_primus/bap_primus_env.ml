open Core_kernel
open Bap_knowledge
open Bap_core_theory

open Bap_primus_types
open Format
open Bap_primus_sexp

module Observation = Bap_primus_observation
module Generator = Bap_primus_generator
module Machine = Bap_primus_machine
module Value = Bap_primus_value
module Seq = Sequence

open Machine.Syntax

type exn += Undefined_var of Var.ident

let () = Exn.add_printer (function
    | Undefined_var v ->
      Some (sprintf "undefined variable `%s'" (Var.Ident.to_string v))
    | _ -> None)

type t = {
  values : value Var.Ident.Map.t;
  random : Generator.t Var.Ident.Map.t;
}

let sexp_of_values values =
  Sexp.List (Map.to_sequence values |> Seq.map ~f:(fun (v,{value}) ->
      Sexp.List [
        Sexp.Atom "set-var";
        Sexp.Atom (Var.Ident.to_string v);
        Sexp.Atom (asprintf "%a" Word.pp_hex value)
      ]) |> Seq.to_list_rev)

let sexp_of_random map =
  Sexp.List (Map.to_sequence map |> Seq.map ~f:(fun (v,gen) -> Sexp.List [
      Sexp.Atom "gen-var";
      Sexp.Atom (Var.Ident.to_string v);
      Generator.sexp_of_t gen;
    ]) |> Seq.to_list_rev)


let sexp_of_env {values; random} = Sexp.List [
    sexp_of_values values;
    sexp_of_random random;
  ]

let state = Bap_primus_machine.State.declare
    ~inspect:sexp_of_env
    ~uuid:"44b24ea4-48fa-47e8-927e-f7ba65202743"
    ~name:"environment" @@ Knowledge.return {
    values = Var.Ident.Map.empty;
    random = Var.Ident.Map.empty;
  }

let inspect_environment {values;random} =
  let keys =
    Set.union
      (Var.Ident.Set.of_list (Map.keys values))
      (Var.Ident.Set.of_list (Map.keys random)) in
  let sexp_of_var sexp_of_value var value = Sexp.(List [
      Atom (Var.Ident.to_string var);
      sexp_of_value value;
    ]) in
  let sexp_of_policy = Generator.sexp_of_t in
  let bindings =
    Set.fold keys ~init:[] ~f:(fun acc var ->
        match Map.find values var with
        | Some value -> sexp_of_var sexp_of_value var value :: acc
        | None -> match Map.find random var with
          | Some policy -> sexp_of_var sexp_of_policy var policy ::acc
          | None -> assert false)  in
  Sexp.List bindings




let add var policy =
  Machine.Local.update state ~f:(fun s -> {
        s with random = Map.add s.random ~key:var ~data:policy
      })

let set var x =
  Machine.Local.update state ~f:(fun s -> {
        s with values = Map.add s.values ~key:var ~data:x
      })

let null s = Value.zero s

let get var =
  Machine.Local.get state >>= fun t ->
  match Map.find t.values var with
  | Some res -> Machine.return res
  | None -> match Map.find t.random var with
    | None -> Machine.raise (Undefined_var var)
    | Some gen -> Generator.next gen >>= Value.of_word

let has var =
  Machine.Local.get state >>| fun t ->
  Map.mem t.values var || Map.mem t.random var

let keys dic = Map.to_sequence dic |> Seq.map ~f:fst

let all =
  Machine.Local.get state >>| fun {values; random} ->
  Seq.append (keys values) (keys random)
