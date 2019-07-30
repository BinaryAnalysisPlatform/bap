open Core_kernel
open Bap_knowledge
open Knowledge.Syntax

let package = "bap.std-internal"
type 'a t = 'a
type env = Knowledge.state ref
type main = Main
type 'p var = (main,'p option) Knowledge.slot

let state = ref Knowledge.empty
let slots = ref 0
let env = state

let set s = state := s
let reset () = state := Knowledge.empty
let current () = !state

exception Internal_runtime_error of Knowledge.conflict [@@deriving sexp]
exception Not_found [@@deriving sexp]

let try_eval slot exp =
  let cls = Knowledge.Slot.cls slot in
  match Knowledge.run cls exp !state with
  | Ok (v,s) ->
    state := s;
    Ok (Knowledge.Value.get slot v)
  | Error conflict -> Error conflict

let eval slot exp =
  try_eval slot exp |> function
  | Ok v -> v
  | Error conflict ->
    raise (Internal_runtime_error conflict)

let main = Knowledge.Class.declare ~package "main" Main


let var name =
  incr slots;
  let name = sprintf "%s%d" name !slots in
  let order x y : Knowledge.Order.partial = match x, y with
    | Some _, Some _ | None, None -> EQ
    | None,Some _ -> LT
    | Some _,None -> GT in
  let dom = Knowledge.Domain.define ~empty:None ~order "any" in
  Knowledge.Class.property ~package main name dom

let this =
  Knowledge.Symbol.intern ~package "main" main

let try_exec stmt =
  let stmt = stmt >>= fun () -> this in
  match Knowledge.run main stmt !state with
  | Ok (_,s) -> Ok (state := s)
  | Error conflict -> Error conflict

let exec stmt =
  try_exec stmt |> function
  | Ok () -> ()
  | Error err -> raise (Internal_runtime_error err)

let put slot exp = exec @@begin
    exp >>= fun v -> this >>= fun x ->
    Knowledge.provide slot x (Some v)
  end

let get slot = eval slot this |> function
  | None -> raise Not_found
  | Some x -> x
