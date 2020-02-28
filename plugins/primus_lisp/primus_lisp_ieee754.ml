open Core_kernel
open Bap.Std
open Bap_primus.Std


let word_of_float width x = match width with
  | 32 -> Word.of_int32 (Int32.bits_of_float x)
  | 64 -> Word.of_int64 (Int64.bits_of_float x)
  | _ -> invalid_arg "word_of_float"

let float_of_word x = match Word.bitwidth x with
  | 32 -> Int32.float_of_bits (Word.to_int32_exn x)
  | 64 -> Int64.float_of_bits (Word.to_int64_exn x)
  | d -> invalid_argf "float_of_word: %d" d ()

module Primitives(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)

  type 'a op =
    | Inf : (float -> float -> float) op
    | Pre : (float -> float) op
    | Cti : (float -> int64) op
    | Ord : (float -> float -> bool) op

  let inf = Inf
  let pre = Pre
  let cti = Cti
  let ord = Ord

  let type_of_op (type a) (op : a op) =
    let open Primus.Lisp.Type.Spec in
    match op with
    | Inf -> tuple [int; a] // all a @-> a
    | Pre -> tuple [int; a; a] @-> a
    | Cti -> tuple [int; a] @-> b
    | Ord -> tuple [int; a; a] @-> bool

  let desc_of_op (type a) (op : a op) name =
    match op with
    | Inf -> sprintf "reduces the list of operands with %s" name
    | Pre -> sprintf "applies %s to the operand" name
    | Cti -> sprintf "truncates to the nearest integer"
    | Ord -> sprintf "returns true if all operands are ordered \
                      with the %s order" name


  let define (type f) (k : f op) name (f : f) =
    let module Op(Machine : Primus.Machine.S) = struct
      open Machine.Syntax

      module Lisp = Primus.Lisp.Make(Machine)
      module Value = Primus.Value.Make(Machine)

      let if_empty ops f =
        if List.is_empty ops
        then f
        else Lisp.failf "%s: expects exactly three arguments"
            name ()

      let rec ordered order = function
        | [] | [_] -> true
        | x :: (y :: _ as rest) -> order x y && ordered order rest

      let value_of_word x = float_of_word (Value.to_word x)

      let run = function
        | [] | [_] | [_;_] ->
          Lisp.failf "%s: type error - expects 2 or more arguments" name ()
        | sz :: op :: ops -> match Word.to_int (Value.to_word sz) with
          | Error _ ->
            Lisp.failf "%s: expects format as the first a" name ()
          | Ok n when n <> 32 && n <> 64 ->
            Lisp.failf "%s: only 32 or 64 bit binary formats are supported"
              name ()
          | Ok width ->
            let op = value_of_word op in
            match k with
            | Inf ->
              List.fold ~init:op ops ~f:(fun res op ->
                  f res (value_of_word op)) |>
              word_of_float width |>
              Value.of_word
            | Pre ->
              if_empty ops @@ Value.of_word @@
              word_of_float width (f op)
            | Cti ->
              if_empty ops @@
              Value.of_word @@
              Word.of_int64 ~width (f op)
            | Ord ->
              Value.of_word @@
              Word.of_bool @@
              ordered f (op::List.map ~f:value_of_word ops)

    end in
    Lisp.define ("ieee754-" ^ name) (module Op)
      ~docs:(desc_of_op k name)
      ~types:(type_of_op k)

  let init () = Machine.sequence Caml.[
      define inf "add" ( +. );
      define inf "sub" ( -. );
      define inf "div" ( /. );
      define inf "mul" ( *. );
      define inf "pow" ( **);
      define pre "sqrt" ( sqrt );
      define pre "neg"  (~-.);
      define pre "pos"  (~+.);
      define pre "exp" exp;
      define pre "expm1" expm1;
      define pre "log" log;
      define pre "log10" log10;
      define pre "log1p" log1p;
      define pre "cos" cos;
      define pre "sin" sin;
      define pre "tan" tan;
      define pre "cosh" cosh;
      define pre "sinh" sinh;
      define pre "tanh" tanh;
      define pre "acos" acos;
      define pre "asin" asin;
      define pre "atan" atan;
      define inf "atan2" atan2;
      define inf "hypot" hypot;
      define pre "ceil" ceil;
      define pre "floor" floor;
      define pre "abs" abs_float;
      define inf "mod" mod_float;
      define cti "cti" Int64.of_float;
      define ord "lt" (<.);
      define ord "le" (<=.);
      define ord "eq" (=.);
      define ord "ne" (<>.);
      define ord "gt" (>.);
      define ord "ge" (>=.);
    ]
end


let init () =
  Primus.Machine.add_component (module Primitives) [@warning "-D"];
  Primus.Components.register_generic "ieee754" (module Primitives)
    ~package:"primus-lisp"
    ~desc:"provides primitives for IEE754 floating-point arithemtic"
