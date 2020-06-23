open Core_kernel
open Bap.Std
open Monads.Std

module SM = Monad.State
open SM.Monad_infix

[@@@warning "-D"]

type policy = [`Random | `Fixed of int64 | `Interval of int64 * int64 ]
[@@deriving sexp_of]

let rand64 lo hi = Int64.(Random.int64 (hi+(hi-lo)) + lo)


let rec generate = function
  | `Fixed x -> Word.of_int64 x
  | `Random -> generate (`Interval (Int64.min_value, Int64.max_value))
  | `Interval (lo,hi) -> generate (`Fixed (rand64 lo hi))

class ['a] main
    ?(memory=fun _ -> None)
    ?(lookup=fun _ -> None)
    ?random_seed
    ?(reg_policy=`Random)
    ?(mem_policy=`Random) () =

  object(self)
    inherit ['a] expi as super

    initializer Option.iter random_seed ~f:Random.init

    method! eval_unknown _ t = self#emit reg_policy t

    method private emit_word w =
      SM.get () >>= fun ctxt ->
      let ctxt,r = ctxt#create_word w in
      SM.put ctxt >>= fun () ->
      SM.return r

    method! lookup v =
      super#lookup v >>= fun r ->
      match Bil.Result.value r with
      | Bil.Imm _ | Bil.Mem _ -> SM.return r
      | Bil.Bot -> match lookup v with
        | Some w -> self#emit_word w
        | None -> self#emit reg_policy (Var.typ v)

    method! load s a =
      super#load s a >>= fun r -> match Bil.Result.value r with
      | Bil.Imm _ | Bil.Mem _ -> SM.return r
      | Bil.Bot -> match memory a with
        | None -> self#emit_const mem_policy 8
        | Some w -> self#emit_word w

    method private emit policy = function
      | Type.Imm sz -> self#emit_const policy sz
      | Type.Mem _ | Type.Unk -> self#emit_empty

    method private emit_const policy sz =
      SM.get () >>= fun ctxt ->
      let const = generate policy in
      let const = Word.extract_exn ~lo:0 ~hi:(sz-1) const in
      let ctxt,r = ctxt#create_word const in
      SM.put ctxt >>= fun () ->
      SM.return r

    method private emit_empty =
      SM.get () >>= fun ctxt ->
      let ctxt,r = ctxt#create_storage self#empty in
      SM.put ctxt >>= fun () ->
      SM.return r
  end
