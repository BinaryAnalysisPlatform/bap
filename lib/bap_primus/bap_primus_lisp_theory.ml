
open Core_kernel
open Bap.Std
open Bap_core_theory

open Bap_primus_lisp_types

open KB.Syntax
open KB.Let


type words

type 'a value = 'a Theory.Bitv.t Theory.Value.t
type effect = unit Theory.Effect.t

type s = S : {
    eff : effect;
    res : 'a value;
  } -> s

module Lifter(CT : Theory.Core) = struct
  let word_size = 32

  module Word = Bitvec.M32


  let prims _ = None


  let bits = Theory.Bitv.define
  let words = bits word_size


  let label = KB.Object.create Theory.Program.cls

  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let skip = seq []
  let pass = seq []

  let pure res =
    label >>= fun lbl ->
    res >>= fun res ->
    CT.blk lbl (seq []) (seq []) >>| fun eff ->
    S {eff; res}

  let bigint x m =
    let s = bits m in
    let m = Bitvec.modulus m in
    let x = Bitvec.(bigint x mod m) in
    CT.int s x

  let (:=) = CT.set

  let full eff res =
    seq eff >>= fun eff ->
    res >>| fun res ->
    S {eff;res}

  let data xs =
    label >>= fun lbl ->
    CT.blk lbl (seq xs) (seq [])

  let ctrl xs =
    label >>= fun lbl ->
    CT.blk lbl (seq []) (seq xs)

  let blk lbl xs =
    seq [CT.blk lbl pass skip; seq xs]

  let sort = Theory.Value.sort
  let size x = Theory.Bitv.size (sort x)

  let cast s x =
    CT.cast (bits s) CT.b0 !!x

  let unified x y f =
    let s = Int.max (size x) (size y) in
    cast s x >>= fun x ->
    cast s y >>= fun y ->
    f x y

  let zero = bigint Z.zero 1

  let nil = pure @@ zero

  let var n m =
    CT.var@@Theory.Var.define (bits m) n


  let lisp_machine =
    Theory.Effect.Sort.(join [data "unrepresented-lisp-machine"] [top])

  let undefined =
    full [CT.perform lisp_machine] zero


  let rec eval : ast -> s KB.t = function
    | {data=Int {data={exp=x; typ=Type m}}} -> pure@@bigint x m
    | {data=Var {data={exp=n; typ=Type m}}} -> pure@@var n m
    | {data=Ite (cnd,yes,nay)} -> ite cnd yes nay
    | {data=Let ({data={exp=n; typ=Type t}},x,y)} -> let_ n t x y
    | {data=App (Dynamic name,args)} -> app name args
    | {data=Seq xs} -> seq_ xs
    | {data=Set ({data={exp=n; typ=Type t}},x)} -> set_ n t x
    | {data=Rep (cnd,body)} -> rep cnd body
    | _ -> undefined
  and ite cnd yes nay =
    let* S {eff=ceff; res=cres} = eval cnd in
    let* S {eff=yeff; res=yres} = eval yes in
    let* S {eff=neff; res=nres} = eval nay in
    unified yres nres @@ fun yres nres ->
    Theory.Var.fresh Theory.Bool.t >>= fun cnd ->
    full [
      !!ceff;
      data [cnd := CT.non_zero !!cres];
      CT.branch (CT.var cnd) !!yeff !!neff;
    ] @@
    CT.ite (CT.var cnd) !!yres !!nres
  and rep cnd body =
    let* S {eff=ceff; res=cres} = eval cnd in
    let* S {eff=beff} = eval body in
    let* head = label and* loop = label and* tail = label in
    full [
      blk head [ctrl [CT.goto tail]];
      blk loop [!!beff];
      blk tail [!!ceff; ctrl [
          CT.branch (CT.non_zero !!cres)
            (CT.goto head) skip
        ]]
    ] !!cres
  and app name xs = match prims name with
    | None ->
      KB.List.map xs ~f:eval >>= fun xs ->
      let* S {eff; res} = args name xs in
      let* dst = Theory.Label.for_name name in
      full [
        !!eff;
        ctrl [CT.goto dst]
      ] !!res
    | Some s -> s
  and seq_ xs =
    nil >>= fun init ->
    KB.List.fold ~init xs ~f:(fun (S {eff}) x  ->
        let* S {eff=eff'; res} = eval x in
        full [!!eff; !!eff'] !!res)
  and set_ n t x =
    let* S {eff; res} = eval x in
    let v = Theory.Var.define (bits t) n in
    full [!!eff; data [v := !!res]] !!res
  and let_ v t x b =
    let* S {eff=effx; res=resx} = eval x in
    let* S {eff=effb; res=resb} = eval b in
    let v = Theory.Var.define (bits t) v in
    cast t resx >>= fun x ->
    full [
      !!effx;
      data [v := !!x];
      !!effb;
    ] !!resb
  and args _name _xs = assert false

end
