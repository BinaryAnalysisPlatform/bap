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

(* Remez's coefficients for sin over 0 to pi. Cos is implmented using cos(x) =
 * sin(x + pi/2) *)
let table = Hashtbl.of_alist_exn (module String) [
                "sin", floats [| -2.1872506537704514e-10; 1.0000000200128274;
                                 -3.0302439009453449e-7; -1.6666487069033565e-1;
                                 -5.4940340462839474e-6; 8.3432342039387503e-3;
                                 -1.1282260464920005e-5; -1.8998652204112551e-4;
                                 -4.1639895115337191e-6; 4.0918788476303817e-6;
                                 -2.6049442541122577e-7;
                              |];
              ]

type op = Sin | Cos [@@deriving sexp]
let parse_op : string -> op option = fun s ->
  Option.try_with (fun () -> op_of_sexp (Sexp.of_string s))

let with_fresh_var exp body =
  exp >>= fun a ->
  let sort = Value.sort a in
  Var.scoped sort @@ fun v ->
  CT.let_ v !!a (body v)

let (>>>=) = with_fresh_var

let (>>->) x f =
  x >>= fun x ->
  f (Value.sort x) x

let size_of_var var =
  let size = Bap.Std.Var.typ var in
  match size with
  | Type.Imm size -> size
  | _ -> assert false

let make_float_value fsort x =
  let core_theory_i = CT.int (IEEE754.Sort.bits fsort) x in
  CT.float fsort core_theory_i

module Reduction_Constants = struct

  let pi = 3.141592653589793115997963468544185161590576171875

  let pi_mul_2 fsort =
    let wf = word_of_float (2.0*.pi) in
    let float_create = make_float_value fsort in
    float_create wf

  let pi_div_2 fsort =
    let wf = word_of_float (pi/.2.0) in
    let float_create = make_float_value fsort in
    float_create wf

  let pi fsort =
    let wf = word_of_float pi in
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

  let fone fs =
    let bs = Floats.size fs in
    let one = Word.one (Bits.size bs) in
    CT.(float fs (int bs one))

  let fzero fs =
    let bs = Floats.size fs in
    let z = Word.zero (Bits.size bs) in
    CT.(float fs (int bs z))
end

module Range_Reduction = struct
  open CT

  let fadd1 rm x =
    x >>= fun y ->
    let sort = Value.sort y in
    fadd rm x (Reduction_Constants.fone sort)

  let fceil rm x =
    fround rm x >>>= fun ix ->
    ite (is_fpos (fsub rm x (var ix))) x (fadd1 rm (var ix))

  let fmod r x y =
    let d = fdiv r x y in
    let c = fceil r d in
    fsub r x (fmul r y c)

  let reduce_to_pos_angle rm x =
    x >>= fun y ->
    let sort = Value.sort y in
    Reduction_Constants.pi_mul_2 sort >>>= fun pi_2 ->
    ite (is_fneg x) (fsub rm (var pi_2) x) x

  (* Sine is an odd function. *)
  let odd_function_reduce rm x =
    x >>= fun y ->
    let sort = Value.sort y in
    Reduction_Constants.pi sort >>>= fun p ->
    ite (is_fpos (fsub rm x (var p))) (fsub rm x (var p)) x

  (* Sine is an odd function. There is redundate checks. Need to fix by merging
  this function with odd_function_reduce *)
  let odd_function_sign rm x =
    x >>= fun y ->
    let sort = Value.sort y in
    Reduction_Constants.sign sort >>>= fun s ->
    Reduction_Constants.pi sort >>>= fun p ->
    ite (is_fpos (fsub rm x (var p))) (Reduction_Constants.sign_negative sort) (var s)

  let sin_range_reduce rm x return =
    x >>= fun y ->
    Reduction_Constants.pi_mul_2 (Value.sort y) >>>= fun pi2 ->
    fmod rm x (var pi2) >>>= fun n ->
    reduce_to_pos_angle rm (var n) >>>= fun pn ->
    odd_function_reduce rm (var pn) >>>= fun reduced_n ->
    odd_function_sign rm (var pn) >>>= fun current_sign ->
    return (var reduced_n) (var current_sign)
  end


module Range_Reconstruction = struct
  let sin_range_recons rm sign x =
    CT.fmul rm sign x
end

module Sin = struct
  open CT
  let build ?rm:(rm=rne) x c poly_eval  =
    Range_Reduction.sin_range_reduce rm x @@ fun n sign ->
    Range_Reconstruction.sin_range_recons rm sign (poly_eval c n)
end

module Horner
 : sig
    (* [run op v coef] computes an approximation of [op] using coef with variable v *)
    val build : op -> Bap.Std.Var.t -> word array -> exp option
  end
  =  struct
  open CT
  let bits fsort = Floats.(Format.bits (format fsort))

  let exp x =
    let open Knowledge.Syntax in
    let x = x >>| Value.semantics in
    match Knowledge.run x Knowledge.empty with
    | Error _ -> assert false
    | Ok (s,_) -> Semantics.get Bil.Domain.exp s

  let approximate ~coefs ?rm:(rm=rne) x  =
    let rank = Array.length coefs - 1 in
    let rec sum i y =
      if i >= 0 then
        fmul rm x y >>>= fun y ->
        fadd rm (var y) coefs.(i) >>>= fun y ->
        sum (i - 1) (var y)
      else y in
    sum (rank-1) coefs.(rank)

  let build func var coefs =
    let size = size_of_var var in
    let fsort = IEEE754.Sort.define (IEEE754.binary size |> Option.value_exn) in
    let float_create = make_float_value fsort in
    let c = Array.map ~f:float_create coefs in
    let v = Var.define fsort "v" in
    let formula = match func with
      | Sin -> Sin.build CT.(var v) c (fun c n -> approximate c n)
      | Cos ->
         let shifted_var = CT.(fadd rne (var v) (Reduction_Constants.pi_div_2 fsort)) in
         Sin.build shifted_var c (fun c n -> approximate c n) in
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
    let elementry_function =
      match parse_op name with
      | Some op -> op
      | _ -> assert false in
    let coefficients =
      let name = match elementry_function with
        | Sin | Cos -> "sin" in
      match Hashtbl.find table name with
      | Some coefs -> coefs
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
