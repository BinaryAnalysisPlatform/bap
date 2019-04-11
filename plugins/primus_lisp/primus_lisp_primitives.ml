open Core_kernel
open Bap.Std
open Bap_primus.Std

module Lib(Machine : Primus.Machine.S) = struct
  module Eval = Primus.Interpreter.Make(Machine)
  module Lisp = Primus.Lisp.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  include Machine.Syntax
  let all f args = List.exists args ~f |> Value.of_bool
  let addr_width =
    Machine.arch >>| Arch.addr_size >>| Size.in_bits
  let endian =
    Machine.arch >>| Arch.endian
  let null = addr_width >>= Value.zero
  let negone = null >>= Value.lnot
  let false_ = Value.zero 1
  let true_  = Value.one 1
  let reduce null op = function
    | [] -> null
    | x :: xs -> Machine.List.fold xs ~init:x ~f:op

  let signed_reduce null op xs =
    Machine.List.map xs ~f:Value.signed >>=
    reduce (null >>= Value.signed) op

  let ordered order xs =
    let rec ordered = function
      | [] | [_] -> true
      | x :: (y :: _ as rest) -> order x y && ordered rest in
    if ordered xs then true_ else false_

end

module IsZero(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = all Value.is_zero
end

module IsPositive(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = all Value.is_positive
end

module IsNegative(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = all Value.is_negative
end

module WordWidth(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run args =
    addr_width >>= fun width ->
    match args with
    | [] -> Value.of_int ~width width
    | x :: _ -> Value.of_int ~width (Value.bitwidth x)
end

module ExitWith(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = all Value.is_negative
  let run _ =
    Eval.halt >>|
    Nothing.unreachable_code >>= fun () ->
    Value.b0
end

module MemoryAllocate(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let negone = Value.one 8
  let zero = Value.zero 8

  let make_static_generator x = match Word.to_int x with
    | Ok x when x >= 0 && x < 256 ->
      Ok (Some (Primus.Generator.static x))
    | _ -> Or_error.errorf "memory-allocate: fill in value must fit into byte"

  let run = function
    | addr :: size :: gen ->
      let n = Word.to_int (Value.to_word size) in
      let gen = match gen with
        | []  -> Ok None
        | [x] -> make_static_generator (Value.to_word x)
        | _ -> Or_error.errorf "memory-allocate requires two or three arguments" in
      if Result.is_error n || Result.is_error gen
      then negone
      else
        let generator = Or_error.ok_exn gen in
        Memory.allocate ?generator (Value.to_word addr) (Or_error.ok_exn n) >>=
        fun () -> zero
    | _ -> Lisp.failf "allocate requires two arguments" ()
end


module MemoryRead(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x] ->
      endian >>= fun e -> Eval.load x e `r8
    | _ -> Lisp.failf "memory-read requires one argument" ()
end

module MemoryWrite(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [a;x] ->
      endian >>= fun e ->
      Eval.store a x e `r8 >>= fun () ->
      Value.succ a
    | _ -> Lisp.failf "memory-write requires two arguments" ()
end

module GetPC(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [] -> Eval.pc >>= Value.of_word
    | _ -> Lisp.failf
             "get-current-program-counter requires zero arguments" ()
end

module Add(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null (Eval.binop Bil.PLUS)
end

module Sub(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null (Eval.binop Bil.MINUS)
end

module Div(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null (Eval.binop Bil.DIVIDE)
end

module SDiv(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = signed_reduce null (Eval.binop Bil.SDIVIDE)
end

module Mul(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null (Eval.binop Bil.TIMES)
end

module Mod(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null (Eval.binop Bil.MOD)
end

module SignedMod(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = signed_reduce null (Eval.binop Bil.SMOD)
end

module Lshift(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x;y] -> Eval.binop Bil.lshift x y
    | _ -> Lisp.failf "Type error: lshift expects two arguments" ()
end

module Rshift(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x;y] -> Eval.binop Bil.rshift x y
    | _ -> Lisp.failf "Type error: rshift expects two arguments" ()
end

module Arshift(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x;y] -> Eval.binop Bil.arshift x y
    | _ -> Lisp.failf "Type error: arshift expects two arguments" ()
end

module Equal(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [] -> true_
    | x :: xs -> all (Value.equal x) xs
end

module NotEqual(Machine : Primus.Machine.S) = struct
  include Equal(Machine)
  let run xs = run xs >>= (Eval.unop Bil.NOT)
end


module Logand(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce negone (Eval.binop Bil.AND)
end

module Logor(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null (Eval.binop Bil.OR)
end

module Logxor(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null (Eval.binop Bil.XOR)
end

module Concat(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce false_ Eval.concat
end

module Extract(Machine : Primus.Machine.S) = struct
  include Lib(Machine)

  let to_int e = match Word.to_int (Value.to_word e) with
    | Ok x -> Machine.return x
    | Error _ -> Lisp.failf "expected smallint" ()

  let run = function
    | [hi; lo; x] ->
      to_int hi >>= fun hi ->
      to_int lo >>= fun lo ->
      Eval.extract ~hi ~lo x
    | _ -> Lisp.failf "extract expects exactly three arguments" ()
end

module Not(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x] -> if Value.is_zero x then Value.b1 else Value.b0
    | _ -> Lisp.failf "not expects only one argument" ()
end

module Lnot(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x] -> Eval.unop Bil.NOT x
    | _ -> Lisp.failf "lnot expects only one argument" ()
end

module Neg(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x] -> Eval.unop Bil.NEG x
    | _ -> Lisp.failf "neg expects only one argument" ()
end

module Less(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let rec run = ordered Value.(<)
end

module Greater(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let rec run = ordered Value.(>)
end

module LessEqual(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let rec run = ordered Value.(<=)
end

module GreaterEqual(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let rec run = ordered Value.(>=)
end

module SymbolConcat(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run syms =
    Machine.List.map syms ~f:Value.Symbol.of_value >>= fun strs ->
    Value.Symbol.to_value (String.concat strs)

end

module Stub(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = Lisp.failf "not implemented"
end

module Primitives(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Lisp = Primus.Lisp.Make(Machine)

  let init () =
    let open Primus.Lisp.Type.Spec in
    let def name types closure docs =
      Lisp.define ~types ~docs name closure  in
    Machine.sequence [
      def "is-zero" (all any @-> bool) (module IsZero)
        "(is-zero X Y ...) returns true if all arguments are zeros";
      def "is-positive" (all any @-> bool) (module IsPositive)
        "(is-positive X Y ...) returns true if all arguments are positive";
      def "is-negative" (all any @-> bool) (module IsNegative)
        "(is-negative X Y ...) returns true if all arguments are negative";
      def "word-width" (unit @-> int)  (module WordWidth)
        "(word-width) returns machine word width in bits";
      def "exit-with" (one int @-> any) (module ExitWith)
        "(exit-with N) terminates program with the exit codeN";
      def "memory-read" (one int @-> byte) (module MemoryRead)
        "(memory-read A) loads one byte from the address A";
      def "memory-write" (tuple [int; byte] @-> int) (module MemoryWrite)
        "(memory-write A X) stores by X to A";
      def "memory-allocate" (tuple [int; int] // all byte @-> byte) (module MemoryAllocate)
        "(memory-allocate P N V?) maps memory region [P,P+N), if V is
         provided, then fills the newly mapped region with the value V";
      def "get-current-program-counter" (unit @-> int) (module GetPC)
        "(get-current-program-counter) returns current program cunnter";
      def "+" (all a @-> a) (module Add)
        "(+ X Y ...) returns the sum of arguments, or 0 if there are
         no arguments,";
      def "-" (all a @-> a) (module Sub)
        "(- X Y Z ...) returns X - Y - Z - ..., or 0 if there are no
         arguments.";
      def "*" (all a @-> a) (module Mul)
        "(* X Y Z ...) returns the product of arguments or 1 if the list
        of arguments is empty";
      def "/" (all a @-> a) (module Div)
        "(/ X Y Z ...) returns X / Y / Z / ... or 0 if the list of
         arguments is empty";
      def "s/" (all a @-> a) (module SDiv)
        "(s/ X Y Z ...) returns X s/ Y s/ Z s/ ... or 0 if the list of
         arguments is empty, where s/ is the signed division operation";
      def "mod" (all a @-> a) (module Mod)
        "(mod X Y Z ...) returns X % Y % Z % ... or 0 if the list of
         arguments is empty, where % is the modulo operation";
      def "signed-mod" (all a @-> a) (module SignedMod)
        "(signed-mod X Y Z ...) returns X % Y % Z % ... or 0 if the list of
         arguments is empty, where % is the signed modulo operation";
      def "lshift" (tuple [a; b] @-> a) (module Lshift)
        "(lshift X N) logically shifts X left by N bits";
      def "rshift" (tuple [a; b] @-> a) (module Rshift)
        "(rshift X N) logically shifts X right by N bits";
      def "arshift" (tuple [a; b] @-> a) (module Arshift)
        "(arshift X N) arithmetically shifts X right by N bits";
      def "=" (all a @-> bool) (module Equal)
        "(= X Y Z ...) returns true if all arguments are equal. True
        if the list of arguments is empty";
      def "/=" (all a @-> bool) (module NotEqual)
        "(/= X Y Z ...) returns true if at least one argument is not
         equal to another argument. Returns false if the list of
         arguments is empty";
      def "logand" (all a @-> a) (module Logand)
        "(logand X Y Z ...) returns X & Y & Z & ... or 0 if the list of
         arguments is empty, where & is the bitwise AND
         operation. Returns ~0 if the list of arguments is empty";
      def "logor" (all a @-> a) (module Logor)
        "(logor X Y Z ...) returns X | Y | Z | ... or 0 if the list of
         arguments is empty, where | is the bitwise OR operation";
      def "logxor" (all a @-> a) (module Logxor)
        "(logxor X Y Z ...) returns X ^ Y ^ Z ^ ... or 0 if the list of
         arguments is empty, where ^ is the bitwise XOR operation";
      def "concat" (all any @-> any) (module Concat)
        "(concat X Y Z ...) concatenates words X, Y, Z, ... into one
         big word";
      def "extract" (tuple [any; any; any] @-> any) (module Extract)
        "(extract HI LO X) extracts bits from HI to LO (including
           both) from the word X ";
      def "lnot" (one a @-> a) (module Lnot)
        "(lnot X) returns the one complement of X";
      def "not" (one a @-> a) (module Not)
        "(not X) returns true if X is zero";
      def "neg" (one a @-> a) (module Neg)
        "(neg X) returns the two complement of X";
      def "<" (all a @-> bool) (module Less)
        "(< X Y Z ...) is true if the list of arguments is an
         strict ascending chain or if it is empty";
      def ">" (all a @-> bool) (module Greater)
        "(< X Y Z ...) is true if the list of arguments is a
         strict descending chain or if it is empty";
      def "<=" (all a @-> bool) (module LessEqual)
        "(< X Y Z ...) is true if the list of arguments is an
         ascending chain or if it is empty";
      def ">=" (all a @-> bool) (module GreaterEqual)
        "(< X Y Z ...) is true if the list of arguments is a
         descending chain or if it is empty";
      def "symbol-concat" (all sym @-> sym) (module SymbolConcat)
        "(symbol-concat X Y Z ...) returns a new symbol that is a
        concatenation of symbols X,Y,Z,... "
      ;
    ]
end

let init () = Primus.Machine.add_component (module Primitives)
