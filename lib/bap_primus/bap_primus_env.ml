open Core_kernel.Std
open Bap.Std
open Bap_primus_types
open Format
open Bap_primus_sexp

module Observation = Bap_primus_observation
module Generator = Bap_primus_generator

type exn += Undefined_var of var

let () = Exn.add_printer (function
    | Undefined_var v ->
      Some (sprintf "undefined variable `%s'" (Var.name v))
    | _ -> None)

type t = {
  values : value Var.Map.t;
  random : Generator.t Var.Map.t;
}

let sexp_of_values values =
  Sexp.List (Map.to_sequence values |> Seq.map ~f:(fun (v,{value}) ->
      Sexp.List [
        Sexp.Atom "set-var";
        Sexp.Atom (Var.name v);
        Sexp.Atom (Type.to_string (Var.typ v));
        Sexp.Atom (asprintf "%a" Word.pp_hex value)
      ]) |> Seq.to_list_rev)

let sexp_of_random map =
  Sexp.List (Map.to_sequence map |> Seq.map ~f:(fun (v,gen) -> Sexp.List [
      Sexp.Atom "gen-var";
      Sexp.Atom (Var.name v);
      Generator.sexp_of_t gen;
    ]) |> Seq.to_list_rev)


let sexp_of_env {values; random} = Sexp.List [
    sexp_of_values values;
    sexp_of_random random;
  ]

let state = Bap_primus_machine.State.declare
    ~inspect:sexp_of_env
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
  let sexp_of_policy = Generator.sexp_of_t in
  let bindings =
    Set.fold keys ~init:[] ~f:(fun acc var ->
        match Map.find values var with
        | Some value -> sexp_of_var sexp_of_value var value :: acc
        | None -> match Map.find random var with
          | Some policy -> sexp_of_var sexp_of_policy var policy ::acc
          | None -> assert false)  in
  Sexp.List bindings

let word = Word.of_int ~width:8


module Make(Machine : Machine) = struct
  open Machine.Syntax

  module Generator = Bap_primus_generator.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)

  let add var policy =
    Machine.Local.update state ~f:(fun s -> {
          s with random = Map.add s.random ~key:var ~data:policy
        })

  let set var x =
    Machine.Local.update state ~f:(fun s -> {
          s with values = Map.add s.values ~key:var ~data:x
        })

  let gen_word gen width =
    assert (width > 0);
    let rec next x =
      if Word.bitwidth x >= width
      then Machine.return (Word.extract_exn ~hi:(width-1) x)
      else Generator.next gen >>= fun y ->
        next (Word.concat x (word y)) in
    Generator.next gen >>| word >>= next

  let null = Machine.get () >>| Project.arch >>| Arch.addr_size >>= fun s ->
    Value.zero (Size.in_bits s)

  let get var =
    Machine.Local.get state >>= fun t ->
    match Map.find t.values var with
    | Some res -> Machine.return res
    | None -> match Var.typ var with
      | Type.Mem (_,_) -> null
      | Type.Imm width -> match Map.find t.random var with
        | None -> Machine.raise (Undefined_var var)
        | Some gen -> gen_word gen width >>= Value.of_word

end
