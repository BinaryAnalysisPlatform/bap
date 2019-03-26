open Core_kernel
open Bap_knowledge

type effects = Top | Set of Set.M(String).t
type 'a spec = effects
type +'a t = 'a spec Knowledge.Class.t
type data = Data
type ctrl = Ctrl

let package = "core-theory"

let base : 'a spec Knowledge.Class.abstract Knowledge.Class.t =
  Knowledge.Class.abstract ~package "effect"
    ~desc:"denotation of a result of effectful computation"

let single eff = Set (Set.singleton (module String) eff)
let make name = Knowledge.Class.refine base (single name)

let define name =
  Knowledge.Class.refine base (single name)

let data = Knowledge.Class.data

let both x y =
  Knowledge.Class.refine base @@ match data x, data y with
  | Top,_ | _,Top -> Top
  | Set x, Set y -> Set (Set.union x y)

let (&&) = both

let refine name other : 'a t = make name && other

let top = Knowledge.Class.refine base Top
let bot = Knowledge.Class.refine base (Set (Set.empty (module String)))

let union xs = List.reduce xs ~f:both |> function
  | Some x -> x
  | None -> bot

let join xs ys = union xs && union ys

let order x y : Knowledge.Order.partial =
  match data x, data y with
  | Top,Top -> EQ
  | Top,_ -> GT
  | _,Top -> LT
  | Set x, Set y ->
    if
      Set.equal x y then EQ else if
      Set.is_subset x ~of_:y then LT else if
      Set.is_subset y ~of_:x then GT else NC

let rreg = define "rreg"
let wreg = define "wreg"
let rmem = define "rmem"
let wmem = define "wmem"
let barr = define "barr"
let fall = define "fall"
let jump = define "jump"
let cjmp = define "cjmp"

let data = make
let ctrl = make
