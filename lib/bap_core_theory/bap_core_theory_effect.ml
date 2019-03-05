open Core_kernel
open Bap_knowledge

type effects = Set.M(String).t
type base = Effect
type 'a spec = effects
type data = Data
type ctrl = Ctrl
type full = Full
type 'a t = ('a spec -> unit) Knowledge.Class.t
type 'a value = ('a spec -> unit) Knowledge.value knowledge

let package = "core-theory"

let base =
  Knowledge.Class.abstract @@
  Knowledge.Class.declare ~package "effect"
    ~desc:"denotation of a result of effectful computation"
    Effect

let single = Set.singleton (module String)


let make name = Knowledge.Class.refine base (single name)


let define name  _ =
  Knowledge.Class.refine base (single name)

let data = Knowledge.Class.data

let add x y =
  Knowledge.Class.refine base @@
  Set.union (data x) (data y)

let (+) = add

let refine name other : 'a t = make name + other

let unknown : 'a t = Knowledge.Class.refine base (Set.empty (module String))

let sum xs = List.reduce xs ~f:add |> function
  | Some x -> x
  | None -> unknown

let join xs ys = sum xs + sum ys

let order x y : Knowledge.Order.partial =
  let x = data x and y = data y in if
    Set.equal x y then EQ else if
    Set.is_subset x ~of_:y then LT else if
    Set.is_subset y ~of_:x then GT else NC

let rreg = define "rreg" Data
let wreg = define "wreg" Data
let rmem = define "rmem" Data
let wmem = define "wmem" Data
let barr = define "barr" Data
let fall = define "fall" Ctrl
let jump = define "jump" Ctrl
let cjmp = define "cjmp" Ctrl

let data = Data
let ctrl = Ctrl
let full = Full
