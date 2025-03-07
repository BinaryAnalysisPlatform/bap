open Core_kernel[@@warning "-D"]
open Bap_knowledge
open Bap_core_theory_definition

module KB = Knowledge

open KB.Syntax
open KB.Let

module Target = Bap_core_theory_target
module Program = Bap_core_theory_program
module Effect = Bap_core_theory_effect
module Origin = Target.Origin
module Var = Bap_core_theory_var
module Val = Bap_core_theory_value


module type trans = functor (_ : Core) -> Core

type pass = (module trans)

let passes = Hashtbl.create (module KB.Name)
let info = Hashtbl.create (module KB.Name)

let no_such_pass name =
  invalid_argf "Unknown core theory pass %s"
    (KB.Name.show name) ()

let name_is_taken name =
  invalid_argf "The name %s is already taken, please
    select a unique name" (KB.Name.show name) ()

let register ?desc ?package name pass =
  let name = KB.Name.create ?package name in
  if Hashtbl.mem passes name then name_is_taken name;
  Option.iter desc ~f:(fun desc -> Hashtbl.add_exn info name desc);
  Hashtbl.add_exn passes name pass


let lookup name = match Hashtbl.find passes name with
  | None -> no_such_pass name
  | Some p -> p

let compose : pass -> pass -> pass =
  fun (module T1) (module T2) ->
  let module T(X : Core) = T2(T1(X)) in
  (module T)

let apply names : (module Core) -> (module Core) =
  fun (module CT) ->
  match List.map names ~f:lookup with
  | [] -> (module CT)
  | ps ->
    let (module T) = List.reduce_balanced_exn ps ~f:compose in
    (module T(CT))

module Scope = struct
  let vars = KB.Context.declare ~package:"core" "desugar-scope-vars"
      !!(Map.empty (module Var.Ident))

  let update slot obj f =
    KB.collect slot obj >>| f >>=
    KB.provide slot obj

  let push var =
    KB.Context.update vars @@ fun vars ->
    Map.update vars (Var.ident var) ~f:(function
        | None -> 1
        | Some n -> n + 1)

  let pop var =
    KB.Context.update vars @@ fun vars ->
    Map.change vars (Var.ident var) ~f:(function
        | None | Some 1 -> None
        | Some n -> Some (n-1))

  let mem var =
    KB.Context.get vars >>| fun vars ->
    Map.mem vars (Var.ident var)
end


module Desugar(CT : Core) : Core = struct

  module Delta = struct
    let target =
      KB.Object.scoped Program.cls Program.Label.target

    let pass = Effect.empty Effect.Sort.bot

    let take what bits x =
      what (Val.Bitv.define bits) (CT.var x)

    let assign_sub dst src off =
      src >>= fun src ->
      let s = Var.sort dst in
      let dst_len = Val.Bitv.size s
      and src_len = Val.Bitv.size @@ Val.sort src in
      let ulen = dst_len - src_len - off in
      CT.set dst @@ match ulen,off with
      | 0,0 -> !!src
      | _,0 -> CT.append s (take CT.high ulen dst) !!src
      | 0,_ -> CT.append s !!src (take CT.low off dst)
      | _,_ -> CT.concat s [
          take CT.high ulen dst;
          !!src;
          take CT.low off dst
        ]

    let pos x =
      let module Pos = Bitvec.M32 in
      CT.int (Val.Bitv.define 32) (Pos.int x)

    let assign_multi size cast lhs rhs =
      rhs >>= fun rhs ->
      let total = Val.(Bitv.size (sort rhs)) in
      fst@@List.fold lhs ~init:(!!pass,total+1) ~f: (fun (data,hi) lhs ->
          let s = Var.sort lhs in
          let bits = size s in
          let lo = hi - bits in
          let s' = Val.Bitv.define bits in
          let rhs = cast (CT.extract s' (pos hi) (pos lo) !!rhs) in
          CT.(seq data (set lhs rhs),hi - bits))

    let assign_regs lhs rhs = assign_multi Val.Bitv.size Fn.id lhs rhs
    let assign_bits lhs rhs =
      assign_multi (Fn.const 1) CT.(fun v ->ite (is_zero v) b0 b1) lhs rhs

    (* module Alias ensures that only bitvec registers
       are involved in aliasing *)
    let cast_val v = v >>| fun v ->
      match Val.resort Val.Bitv.refine (Val.forget v) with
      | None -> assert false
      | Some v -> v

    let set v x =
      Scope.mem v >>= function
      | true -> CT.set v x
      | false ->
        let* t = target in
        if Target.has_roles t [Target.Role.Register.constant] v
        then !!pass
        else match Target.unalias t v with
          | None -> CT.set v x
          | Some origin ->
            let x = cast_val x in
            match Origin.cast_sub origin with
            | Some s when Origin.is_alias s ->
              CT.set (Origin.reg s) x
            | Some s -> assign_sub (Origin.reg s) x (Origin.lo s)
            | None -> match Origin.cast_sup origin with
              | Some s -> assign_regs (Origin.regs s) x
              | None -> match Origin.cast_set origin with
                | Some s -> assign_bits (Origin.bits s) x
                | None -> !!pass


    let const role value ret t r s =
      if Target.has_roles t [role] r
      then match Val.Bitv.refine (Val.Sort.forget s) with
        | None -> Some (CT.unk s)
        | Some s' -> Some (ret@@CT.int s' value)
      else None

    let consts = [
      Target.Role.Register.zero, Bitvec.zero;
      Target.Role.Register.one, Bitvec.one;
    ]

    let var (type a) r  =
      Scope.mem r >>= function
      | true -> CT.var r
      | false ->
        let* t = target in
        let s = Var.sort r in
        let ret x = x >>| fun x -> KB.Value.refine x s in
        List.find_map consts ~f:(fun (role,value) ->
            const role value ret t r s) |> function
        | Some v -> v
        | None ->
          match Target.unalias t r with
          | None -> CT.var r
          | Some origin ->
            match Origin.cast_sub origin with
            | Some sub when Origin.is_alias sub ->
              CT.var (Var.resort (Origin.reg sub) s)
            | Some sub ->
              let hi = Origin.hi sub and lo = Origin.lo sub in
              let bs = Val.Bitv.define (hi-lo+1) in
              ret @@
              CT.extract bs (pos hi) (pos lo) (CT.var (Origin.reg sub))
            | None -> match Origin.cast_sup origin with
              | Some sup ->
                let regs = Origin.regs sup in
                let total = List.sum (module Int) regs ~f:(fun r ->
                    Val.Bitv.size (Var.sort r)) in
                let bs = Val.Bitv.define total in
                ret @@ CT.concat bs (List.map regs ~f:CT.var)
              | None ->
                match Origin.cast_set origin with
                | Some set ->
                  let bits = Origin.bits set in
                  let n = List.length bits in
                  let bs = Val.Bitv.define n in
                  let us = Val.Bitv.define 1 in
                  let m0 = CT.int us Bitvec.zero
                  and m1 = CT.int us Bitvec.one in
                  ret @@ CT.concat bs (List.map bits ~f:(fun v ->
                      CT.(ite (var v) m1 m0)))
                | none -> CT.unk s


    let let_ v x y =
      x >>= fun x ->
      Scope.push v >>= fun () ->
      y >>= fun y ->
      Scope.pop v >>= fun () ->
      CT.let_ v !!x !!y

  end

  include CT
  include Delta
end

let () = register "desugar-variables" (module Desugar)
    ~package:"core"
    ~desc:"desugars assignments and access to register aliases"
