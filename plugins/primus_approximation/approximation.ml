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

let exp x =
  let open Knowledge.Syntax in
  let x = x >>| Value.semantics in
  match Knowledge.run x Knowledge.empty with
  | Error _ -> assert false
  | Ok (s,_) -> Semantics.get Bil.Domain.exp s

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

  let approximate ~coefs x  =
    let rank = Array.length coefs - 1 in
    let rec sum i y =
      if i >= 0 then
        fmul rne x y >>>= fun y ->
        fadd rne (var y) coefs.(i) >>>= fun y ->
        sum (i - 1) (var y)
      else y in
    sum (rank-1) coefs.(rank)

  let make_float_value fsort x =
    let core_theory_i = CT.int (IEEE754.Sort.bits fsort) x in
    CT.float fsort core_theory_i

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
