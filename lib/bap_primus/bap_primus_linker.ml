open Core_kernel
open Regular.Std
open Format
open Bap_knowledge
open Bap_core_theory
open Bap_primus_types

module Machine = Bap_primus_machine

open Machine.Syntax
open Link.Syntax

type exn += Unbound_name of Label.t


type t = {
  codes : unit Machine.t Label.Map.t;
  unresolved : label;
}

let empty unresolved = {
  codes = Label.Map.empty;
  unresolved;
}


let unresolved_handler_name = "__primus_linker_unresolved_call"

include struct
  open Sexp

  let string_of_name = Label.to_string
  let sexp_of_name n = Sexp.Atom (string_of_name n)
  let sexp_of_value {value=x} = Atom (asprintf "%a" Word.pp_hex x)
  let sexp_of_args = List.map ~f:sexp_of_value
  let sexp_of_call (dst,args) =
    List (Atom dst :: sexp_of_args args)
end

let unbound_name,needs_unbound =
  Bap_primus_observation.provide ~inspect:sexp_of_name
    "linker-unbound"

module Trace = struct
  module Observation = Bap_primus_observation
  let exec,will_exec = Observation.provide
      ~inspect:sexp_of_name
      "linker-exec"

  let unresolved,will_fail = Observation.provide
      ~inspect:sexp_of_name
      "linker-unresolved"

  let call,call_entered =
    Observation.provide ~inspect:sexp_of_call "call"

  let return,call_returned =
    Observation.provide ~inspect:sexp_of_call "call-return"
end
include Trace

let () = Exn.add_printer (function
    | Unbound_name name ->
      Some (asprintf "unbound function %s" (string_of_name name))
    | _ -> None)

let state = Bap_primus_machine.State.declare
    ~uuid:"38bf35bf-1091-4220-bf75-de79db9de4d2"
    ~name:"linker" @@
  Knowledge.map ~f:empty @@
  link_name unresolved_handler_name

let unresolved_handler =
  Machine.Local.get state >>| fun s -> s.unresolved


(* returns the best name for the label *)

let linker_error s = Machine.raise (Unbound_name s)

let is_linked name =
  Machine.Local.get state >>| fun {codes} -> Map.mem codes name


let run name code =
  Machine.Observation.make will_exec name >>= fun () ->
  code

let fail name =
  Machine.Observation.make will_fail name >>= fun () ->
  Machine.Local.get state >>= fun s ->
  match Map.find s.codes name with
  | Some code -> run name code
  | None -> match Map.find s.codes s.unresolved with
    | None -> linker_error name
    | Some code -> run name code


let link name code =
  Machine.Local.update state ~f:(fun s -> {
        s with codes = Map.set s.codes ~key:name ~data:code
      })

let unlink name =
  Machine.Local.update state ~f:(fun s -> {
        s with codes = Map.remove s.codes name
      })

let lookup name =
  Machine.Local.get state >>| fun {codes} -> Map.find codes name

let exec name =
  lookup name >>= function
  | None -> fail name
  | Some code -> run name code
