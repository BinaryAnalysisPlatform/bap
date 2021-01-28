open Core_kernel
open Bap_core_theory
open Bap_primus.Std
open KB.Syntax
open KB.Let

type KB.conflict += Illformed of string

let illformed fmt =
  Format.kasprintf (fun msg ->
      KB.fail (Illformed msg)) fmt

let nothing = KB.Value.empty Theory.Semantics.cls


let size = Theory.Bitv.size
let forget x = x >>| Theory.Value.forget
let to_bitv x = Theory.Value.resort Theory.Bitv.refine x
let empty s = Theory.Value.(forget @@ empty s)
let fresh = KB.Object.create Theory.Program.cls

module Primitives(CT : Theory.Core) = struct

  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let undefined = seq []
  let pass = seq []
  let skip = seq []

  let negone s =
    CT.int s @@ Bitvec.(ones mod modulus (size s))

  let one s = CT.int s @@ Bitvec.one
  let int s x = CT.int s @@ Bitvec.(int x mod modulus (size s))


  let require_one = function
    | [x] -> !!x
    | _ -> illformed "requires exactly one argument"

  let require_two xs f = match xs with
    | [x; y] -> f x y
    | _ -> illformed "requires exactly two arguments"

  let require_bitv x = match to_bitv x with
    | Some x -> !!x
    | None -> illformed "requires a bitvector"

  let all_bitv = KB.List.map ~f:require_bitv

  let monoid s f init xs =
    all_bitv xs >>= function
    | [] -> forget@@init s
    | x :: xs ->
      KB.List.fold ~init:x xs ~f:(fun res x ->
          CT.cast s CT.b0 !!x >>= fun x ->
          f !!res !!x) |>
      forget

  let is_one x = CT.(inv@@is_zero x)

  let rec is_ordered f = function
    | [] | [_] -> CT.b1
    | x :: (y :: _ as rest) ->
      match to_bitv x, to_bitv y with
      | Some x, Some y ->
        f !!x !!y >>= fun r ->
        is_ordered f rest >>= fun r' ->
        CT.and_ !!r !!r'
      | _ -> CT.unk Theory.Bool.t

  let order f xs = forget@@is_ordered f xs

  let all f xs =
    CT.b1 >>= fun init ->
    KB.List.fold ~init xs ~f:(fun r x ->
        match to_bitv x with
        | None -> illformed "requires bitvec"
        | Some x ->
          CT.and_ !!r (f !!x)) |>
    forget

  let unary f xs =
    require_one xs >>= require_bitv >>= fun x ->
    forget@@f !!x

  let full eff res =
    res >>= fun res ->
    eff >>| fun eff ->
    KB.Value.put Theory.Semantics.value eff res

  let pure res = full (seq []) res

  let static = pure
  let ctrl eff =
    let* lbl = fresh in
    CT.blk lbl (seq []) eff
  let data eff =
    let* lbl = fresh in
    CT.blk lbl eff (seq [])

  let memory eff res =
    let* lbl = fresh in
    full CT.(blk lbl (perform eff) skip) res

  let loads = memory Theory.Effect.Sort.rmem
  let stores = memory Theory.Effect.Sort.wmem
  let loads = pure

  let is_negative x = CT.msb x
  let is_positive x =
    CT.(and_ (non_zero x) (inv (is_negative x)))

  let word_width s xs =
    let bits x = size @@ Theory.Value.sort x in
    all_bitv xs >>= fun xs ->
    List.max_elt xs ~compare:(fun x y ->
        Int.compare (bits x) (bits y)) |>
    Option.value_map ~f:(fun x ->
        int s (bits x))
      ~default:(int s (size s)) |>
    forget

  let exec_addr xs =
    require_one xs >>=
    require_bitv >>= fun dst ->
    CT.jmp !!dst

  let memory_read t xs =
    require_one xs >>=
    require_bitv >>= fun src ->
    CT.(load (var (Theory.Target.data t)) !!src) |>
    forget

  let memory_write t xs =
    require_two xs @@ fun dst data ->
    require_bitv dst >>= fun dst ->
    require_bitv data >>= fun data ->
    let mem = Theory.Target.data t in
    let (:=) = CT.set in
    CT.(mem := store (var mem) !!dst !!data)

  let dispatch lbl name args =
    Theory.Label.target lbl >>= fun t ->
    let s = Theory.Bitv.define (Theory.Target.bits t) in
    match name with
    | "+" -> pure@@monoid s CT.add CT.zero args
    | "-" -> pure@@monoid s CT.sub CT.zero args
    | "*" -> pure@@monoid s CT.mul one args
    | "/" -> pure@@monoid s CT.div one args
    | "s/" -> pure@@monoid s CT.sdiv one args
    | "mod" -> pure@@monoid s CT.modulo one args
    | "signed-mod" -> pure@@monoid s CT.smodulo one args
    | "lshift" -> pure@@monoid s CT.lshift one args
    | "rshift" -> pure@@monoid s CT.rshift one args
    | "arshift" -> pure@@monoid s CT.arshift one args
    | "logand" -> pure@@monoid s CT.logand negone args
    | "logor" -> pure@@monoid s CT.logor CT.zero args
    | "logxor" -> pure@@monoid s CT.logxor CT.zero args
    | "=" -> pure@@order CT.eq args
    | "/=" -> pure@@order CT.neq args
    | "<" -> pure@@order CT.ult args
    | ">" -> pure@@order CT.ugt args
    | "<=" -> pure@@order CT.ule args
    | ">=" -> pure@@order CT.uge args
    | "is-zero" | "not" -> pure@@all CT.is_zero args
    | "is-positive" -> pure@@all is_positive args
    | "is-negative" -> pure@@all is_negative args
    | "word-width" -> static@@word_width s args
    | "exec-addr" -> ctrl@@exec_addr args
    | "memory-read" -> pure@@memory_read t args
    | "memory-write" -> data@@memory_write t args
    | _ -> !!nothing
end

module Sema = Primus.Lisp.Semantics

let provide () =
  KB.promise Theory.Semantics.slot @@ fun obj ->
  KB.collect Sema.primitive obj >>= function
  | None -> !!nothing
  | Some p ->
    Theory.instance () >>= Theory.require >>= fun (module CT) ->
    let module P = Primitives(CT) in
    let name = Sema.Primitive.name p
    and args = Sema.Primitive.args p in
    P.dispatch obj name args
