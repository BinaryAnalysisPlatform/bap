open Core_kernel
open Bap_primus.Std
open Bap.Std
open Monads.Std
open Bap_knowledge
open Bap_core_theory
open Theory
open Knowledge.Syntax

include Self()

module CT = Theory.Manager

let word_of_float x = Word.of_int64 (Int64.bits_of_float x)
let floats = Array.map ~f:word_of_float
let table = Hashtbl.of_alist_exn (module String) [
                "sin", floats [|1.0; 2.0; 3.0; 4.0|];
                "cos", floats [|0.0 ; 1.0|];
              ]



let knowledge_of_word sort w = CT.int sort w

module Horner
: sig
  (* [run op sz v] computes an approximation of [op] unit*)
  val build : Bap.Std.Var.t -> word array -> exp option
end
  =  struct
  open CT
  let bits fsort = Floats.(Format.bits (format fsort))

  let with_fresh_var exp body =
    exp >>= fun a ->
    let sort = Value.sort a in
    Var.scoped sort @@ fun v ->
    CT.let_ v !!a (body v)

  let (>>>=) = with_fresh_var

  let (>>->) x f =
    x >>= fun x ->
    f (Value.sort x) x

  let fone fs =
    let bs = Floats.size fs in
    let one = Word.ones (Bits.size bs) in
    float fs (int bs  one)

  let fzero fs =
    let bs = Floats.size fs in
    let zero = Word.zero (Bits.size bs) in
    float fs (int bs zero)

  let fadd1 rm x =
    x >>-> fun s x ->
           fadd rm !!x (fone s)

  let fceil rm x =
    fround rm x >>>= fun ix ->
    ite (is_fzero (fsub rm x (var ix))) x (fadd1 rm (var ix))

  let fmod r x y =
    let d = fdiv r x y in
    let c = fceil r d in
    fsub r x (fmul r y c)

  let make_float_value fsort x =
    let core_theory_i = CT.int (IEEE754.Sort.bits fsort) x in
    CT.float fsort core_theory_i

  let pi_mul_2 fsort =
    let wf = word_of_float (2.0*.3.14159265358979323846) in
    let float_create = make_float_value fsort in
    float_create wf

  let pi_div_2 fsort =
    let wf = word_of_float (3.14159265358979323846/.2.0) in
    let float_create = make_float_value fsort in
    float_create wf

  let pi fsort =
    let wf = word_of_float 3.14159265358979323846 in
    let float_create = make_float_value fsort in
    float_create wf

  let sign fsort =
    let wf = word_of_float 1.0 in
    let float_create = make_float_value fsort in
    float_create wf

  let sign_negative fsort =
    let wf = word_of_float (-1.0) in
    let float_create = make_float_value fsort in
    float_create wf

  let reduce_to_pos_angle rm x =
    x >>-> fun s _ ->
    pi_mul_2 s >>>= fun pi_2 ->
    ite (is_fneg x) (fsub rm (var pi_2) x) x

  (* Sine is an odd function. *)
  let odd_function_reduce rm x =
    x >>-> fun sort _ ->
    pi sort >>>= fun p ->
    ite (is_fpos (fsub rm x (var p))) (fsub rm x (var p)) x

  (* Sine is an odd function. There is redundate checks. Need to fix by merging
  this function with odd_function_reduce *)
  let odd_function_sign rm x =
    x >>-> fun sort _ ->
    sign sort >>>= fun s ->
    pi sort >>>= fun p ->
    ite (is_fpos (fsub rm x (var p))) (sign_negative sort) (var s)

  let sin_range_reduce rm x =
    x >>-> fun sort _ ->
           pi_mul_2 sort >>>= fun pi2 ->
           fmod rm x (var pi2) >>>= fun n ->
           reduce_to_pos_angle rm (var n) >>>= fun pn ->
           odd_function_reduce rm (var pn) >>>= fun reduced_n ->
           var reduced_n

  let sin_determine_sign rm x =
    x >>-> fun sort _ ->
           pi_mul_2 sort >>>= fun pi2 ->
           fmod rm x (var pi2) >>>= fun n ->
           reduce_to_pos_angle rm (var n) >>>= fun pn ->
           odd_function_sign rm (var pn) >>>= fun current_sign ->
           var current_sign

  let sin_range_reduce rm x =
    x >>-> fun sort _ ->
           pi_mul_2 sort >>>= fun pi2 ->
           fmod rm x (var pi2) >>>= fun n ->
           reduce_to_pos_angle rm (var n) >>>= fun pn ->
           odd_function_reduce rm (var pn) >>>= fun reduced_n ->
           var reduced_n

  let approximate ~coefs x  =
    let rank = Array.length coefs - 1 in
    let rec sum i y =
      if i >= 0 then
        fmul rne x y >>>= fun y ->
        fadd rne (var y) coefs.(i) >>>= fun y ->
        sum (i - 1) (var y)
      else y in
    sum (rank-1) coefs.(rank)

  let size_of_var var =
    let size = Bap.Std.Var.typ var in
    match size with
    | Type.Imm size -> size
    | _ -> assert false

  let exp x =
    let open Knowledge.Syntax in
    let x = x >>| Value.semantics in
    match Knowledge.run x Knowledge.empty with
    | Error _ -> assert false
    | Ok (s,_) -> Semantics.get Bil.Domain.exp s


  let build var coefs =
    let size = size_of_var var in
    let fsort = IEEE754.Sort.define (IEEE754.binary size |> Option.value_exn) in
    let float_create = make_float_value fsort in
    let c = Array.map ~f:float_create coefs in
    let v = Var.define fsort "v" in
    let formula = approximate c CT.(var v) in
    exp formula
end






module Approximate(Machine : Primus.Machine.S) = struct

  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let int_of_value x =
    Primus.Value.to_word x |> Word.to_int_exn

  [@@@warning "-P"]
  let run [name; size; x;] =
    let open Machine.Syntax in
    let size = int_of_value size in
    Value.Symbol.of_value name >>= fun name ->
    let coefficients =
    match Hashtbl.find table name with
    | Some coefs -> coefs
    | _ -> assert false in
    let vx = Bap.Std.Var.create "v" (Type.imm size) in
    let exp = Horner.build vx coefficients in
    match exp with
    | None -> assert false
    | Some e ->
       Eval.set vx x >>= fun () ->
       Eval.exp e
end

(* Symbol.c*)

module Main(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let def name types closure =
    Lisp.define ~types name closure

  let init () =
    Machine.sequence [
        def "approximate" (tuple [sym; int; int] @-> int) (module Approximate);
      ]
end




let main () =
  Primus.Machine.add_component (module Main)

let () = Config.when_ready (fun _ -> main ())
