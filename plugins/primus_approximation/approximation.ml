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

(* Remez's coefficients for sin over 0 to pi *)
let table = Hashtbl.of_alist_exn (module String) [
                "sin", floats [|-2.6049442541122577e-7; 4.0918788476303817e-6;
                               -4.1639895115337191e-6; -1.8998652204112551e-4;
                               -1.1282260464920005e-5; 8.3432342039387503e-3;
                               -5.4940340462839474e-6; -1.6666487069033565e-1;
                               -3.0302439009453449e-7; 1.0000000200128274;
                               -2.1872506537704514e-10 |];
                 "cos", floats [|-2.6049442541122577e-7; 4.0918788476303817e-6;
                                -4.1639895115337191e-6; -1.8998652204112551e-4;
                                -1.1282260464920005e-5; 8.3432342039387503e-3;
                                -5.4940340462839474e-6; -1.6666487069033565e-1;
                                -3.0302439009453449e-7; 1.0000000200128274;
                                -2.1872506537704514e-10 |];
              ]

type op = Sin | Cos [@@deriving sexp]
let parse_op : string -> op option = fun s ->
  Option.try_with (fun () -> op_of_sexp (Sexp.of_string s))

let knowledge_of_word sort w = CT.int sort w

module Horner
: sig
  (* [run op v coef] computes an approximation of [op] using coef*)
  val build : op -> Bap.Std.Var.t -> word array -> exp option
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


  let approximate ~coefs ?rm:(rm=rne) x  =
    let rank = Array.length coefs - 1 in
    let rec sum i y =
      if i >= 0 then
        fmul rm x y >>>= fun y ->
        fadd rm (var y) coefs.(i) >>>= fun y ->
        sum (i - 1) (var y)
      else y in
    sum (rank-1) coefs.(rank)

  let size_of_var var =
    let size = Bap.Std.Var.typ var in
    match size with
    | Type.Imm size -> size
    | _ -> assert false

  let make_float_value fsort x =
    let core_theory_i = int (IEEE754.Sort.bits fsort) x in
    float fsort core_theory_i

  let polynomial var coefs =
    let size = size_of_var var in
    let fsort = IEEE754.Sort.define (IEEE754.binary size |> Option.value_exn) in
    let float_create = make_float_value fsort in
    let c = Array.map ~f:float_create coefs in
    let v = Var.define fsort "v" in
    approximate c CT.(var v)

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

  let sine_range_reduce rm x return =
    x >>-> fun sort _ ->
    pi_mul_2 sort >>>= fun pi2 ->
    fmod rm x (var pi2) >>>= fun n ->
    reduce_to_pos_angle rm (var n) >>>= fun pn ->
    odd_function_reduce rm (var pn) >>>= fun reduced_n ->
    odd_function_sign rm (var pn) >>>= fun current_sign ->
    return (var reduced_n) (var current_sign)

  let sine_range_recons rm sign x =
      fmul rm sign x

  let sine ?rm:(rm=rne) x c =
    sine_range_reduce rm x @@ fun n sign ->
    sine_range_recons rm sign (approximate c n)

  let exp x =
    let open Knowledge.Syntax in
    let x = x >>| Value.semantics in
    match Knowledge.run x Knowledge.empty with
    | Error _ -> assert false
    | Ok (s,_) -> Semantics.get Bil.Domain.exp s

  let build func var coefs =
    let size = size_of_var var in
    let fsort = IEEE754.Sort.define (IEEE754.binary size |> Option.value_exn) in
    let float_create = make_float_value fsort in
    let c = Array.map ~f:float_create coefs in
    let v = Var.define fsort "v" in
    let formula = match func with
      | Sin -> sine CT.(var v) c
      | Cos -> sine CT.(fadd rne (var v) (pi_div_2 fsort)) c in
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
    let elementry_function =
      match parse_op name with
      | Some op -> op
      | _ -> assert false in
    let vx = Bap.Std.Var.create "v" (Type.imm size) in
    let exp = Horner.build elementry_function vx coefficients in
    match exp with
    | None -> assert false
    | Some e ->
       Eval.set vx x >>= fun () ->
       Eval.exp e
end

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
