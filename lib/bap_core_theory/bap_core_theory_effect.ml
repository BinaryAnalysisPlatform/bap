open Core_kernel
open Bap_knowledge

module KB = Knowledge

type cls = Effects

let package = "core-theory"

module Sort = struct
  type effects = Top | Set of Set.M(String).t
  type +'a t = effects
  type data = Data
  type ctrl = Ctrl
  let single eff = Set (Set.singleton (module String) eff)
  let make name = single name
  let define name = make name

  let both x y = match x, y with
    | Top,_ | _,Top -> Top
    | Set x, Set y -> Set (Set.union x y)

  let (&&) = both

  let refine name other : 'a t = make name && other

  let top = Top
  let bot = Set (Set.empty (module String))

  let union xs = List.reduce xs ~f:both |> function
    | Some x -> x
    | None -> bot

  let join xs ys = union xs && union ys

  let order x y : Knowledge.Order.partial =
    match x, y with
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
end

type +'a sort = 'a Sort.t
let cls : (cls, unit) Knowledge.Class.t =
  Knowledge.Class.declare ~package "effect"
    ~desc:"denotation of a result of effectful computation"
    ()

type 'a t = (cls,'a sort) KB.cls KB.value
let empty s = KB.Value.empty (KB.Class.refine cls s)
let sort v = KB.Class.sort (KB.Value.cls v)
