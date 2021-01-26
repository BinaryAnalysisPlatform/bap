open Bap_core_theory
open Bap_primus.Std
open KB.Syntax

type KB.conflict += Illformed of string

let illformed fmt =
  Format.kasprintf (fun msg ->
      KB.fail (Illformed msg)) fmt

let nothing = KB.Value.empty Theory.Semantics.cls


module Primitives(CT : Theory.Core) = struct

  let forget x = x >>| Theory.Value.forget
  let to_bitv x = Theory.Value.resort Theory.Bitv.refine x
  let empty s = Theory.Value.(forget @@ empty s)
  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let undefined = seq []

  let negone s =
    CT.int s @@ Bitvec.(ones mod modulus (Theory.Bitv.size s))

  let one s = CT.int s @@ Bitvec.one

  let monoid s f init = function
    | [] -> forget@@init s
    | x :: xs ->
      KB.List.fold ~init:x  xs ~f:(fun res x ->
          match to_bitv res, to_bitv x with
          | Some res, Some x  ->
            CT.cast s CT.b0 !!x >>= fun x ->
            Format.eprintf "Reducing: %a with %a@\n%!"
              KB.Value.pp res KB.Value.pp x;
            f !!res !!x >>|
            Theory.Value.forget
          | _ -> forget@@CT.unk s)

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

  let require_one = function
    | [x] -> !!x
    | _ -> illformed "requires exactly one argument"

  let unary f xs =
    require_one xs >>= fun x ->
    match to_bitv x with
    | Some x -> forget@@f !!x
    | None -> illformed "requires a bitvector"

  let pure res =
    res >>= fun res ->
    seq [] >>| fun empty ->
    let insn =
      KB.Value.put Theory.Semantics.value empty res in
    Format.eprintf "%a@\n%!" KB.Value.pp insn;
    insn



  (* 1. calls must be made via the invoke subroutine;
     2. calls to lisp programs will be also reified to
     but possibly with a different calling convention.
     3. alternatively, we can inline those calls.
  *)

  let dispatch s name args = match name with
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
    | other ->
      Format.eprintf "ignoring %S@\n%!" other;
      !!nothing
end

module Sema = Primus.Lisp.Semantics

let provide () =
  KB.promise Theory.Semantics.slot @@ fun obj ->
  KB.collect Sema.primitive obj >>= function
  | None -> !!nothing
  | Some p ->
    Theory.Label.target obj >>= fun t ->
    Theory.instance () >>= Theory.require >>= fun (module CT) ->
    let module P = Primitives(CT) in
    let s = Theory.Bitv.define (Theory.Target.bits t) in
    let name = Sema.Primitive.name p
    and args = Sema.Primitive.args p in
    Format.eprintf "Got a primitive %S, dispatching!\n%!" name;
    P.dispatch s name args
