open Core_kernel[@@warning "-D"]
open Bap_knowledge
open Knowledge.Syntax

let package = "bap"
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

exception Conflict of Knowledge.conflict
exception Not_found

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
  | Error x -> raise (Conflict x)

let main = Knowledge.Class.declare ~package "toplevel" Main


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
  | Error err -> raise (Conflict err)

let put slot exp = exec @@begin
    exp >>= fun v -> this >>= fun x ->
    Knowledge.provide slot x (Some v)
  end

let get slot = eval slot this |> function
  | None -> raise Not_found
  | Some x -> x


let () = Caml.Printexc.register_printer @@ function
  | Conflict err ->
    Some (Format.asprintf "%a" Knowledge.Conflict.pp err)
  | _ -> None
