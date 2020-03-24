open Core_kernel
open Bap.Std
open Bap_primus.Std

let registers = Primus.Machine.State.declare
    ~uuid:"3dd2b905-c019-4466-800f-95b13d7da85e"
    ~name:"backend-registers" @@ fun proj ->
  Project.disasm proj |> Disasm.insns |>
  Seq.fold ~init:Int.Map.empty ~f:(fun regs (_,insn) ->
      Insn.ops insn |>
      Array.fold ~init:regs ~f:(fun regs -> function
          | Op.Reg r ->
            Map.set regs (Reg.code r) (Reg.name r)
          | _ -> regs))

module Make(Machine : Primus.Machine.S) = struct
  module Eval = Primus.Interpreter.Make(Machine)
  module Lisp = Primus.Lisp.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)

  open Machine.Syntax

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

module RegName(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib
  let run = function
    | [] | _::_::_ ->
      Lisp.failf "reg-name expects only one argument" ()
    | [code] ->
      Machine.Local.get registers >>= fun regs ->
      let code = Word.to_int_exn @@ Primus.Value.to_word code in
      match Map.find regs code with
      | None -> Lisp.failf "unresolved intruction register %#x" code ()
      | Some name -> Value.Symbol.to_value name
end


module ExecAddr(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib
  let run = function
    | _ :: _ :: _ | [] ->
      Lisp.failf "Lisp Type Error: exec-address expects one argument" ()
    | [addr] ->
      Linker.exec (`addr (Primus.Value.to_word addr)) >>= fun () ->
      Eval.halt >>=
      never_returns
end

module ExecSym(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib
  let run = function
    | _ :: _ :: _ | [] ->
      Lisp.failf "Lisp Type Error: exec-address expects one argument" ()
    | [name] ->
      Value.Symbol.of_value name >>= fun name ->
      Linker.exec (`symbol name) >>= fun () ->
      Eval.halt >>=
      never_returns
end

module IsZero(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib
  let run = all Value.is_zero
end

module IsPositive(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib
  let run = all Value.is_positive
end

module IsNegative(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib
  let run = all Value.is_negative
end

module WordWidth(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run args =
    addr_width >>= fun width ->
    match args with
    | [] -> Value.of_int ~width width
    | x :: _ -> Value.of_int ~width (Value.bitwidth x)
end

module ExitWith(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = all Value.is_negative
  let run _ =
    Eval.halt >>|
    Nothing.unreachable_code >>= fun () ->
    Value.b0
end

module MemoryAllocate(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let negone = Value.one 8
  let zero = Value.zero 8

  let value_to_int x = Value.to_word x |> Word.to_int

  let make_static_generator width x = match Word.to_int x with
    | Ok x -> Ok (Some (Primus.Generator.static ~width x))
    | _ -> Or_error.errorf "memory-allocate: fill in value must fit into int"

  let is_nil x = Word.equal Word.b0 (Value.to_word x)

  let make_uniform_generator ~width ~min ~max =
    let nomin = is_nil min and nomax = is_nil max in
    if nomin && nomax then Ok None
    else match value_to_int  min, value_to_int max with
      | Ok min, Ok max ->
        let min = if nomin then None else Some min
        and max = if nomax then None else Some max in
        let g = Primus.Generator.Random.Seeded.lcg ~width ?min ?max () in
        Ok (Some g)
      | _ -> Or_error.errorf "memory-allocate: random values do not fit into int"


  let run = function
    | addr :: size :: gen ->
      Machine.gets Project.arch >>= fun arch ->
      let width = Arch.addr_size arch |> Size.in_bits in
      let n = Word.to_int (Value.to_word size) in
      let gen = match gen with
        | []  -> Ok None
        | [x] -> make_static_generator width (Value.to_word x)
        | [min; max] -> make_uniform_generator width min max
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
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [x] ->
      endian >>= fun e -> Eval.load x e `r8
    | _ -> Lisp.failf "memory-read requires one argument" ()
end

module MemoryWrite(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [a;x] ->
      endian >>= fun e ->
      Eval.store a x e `r8 >>= fun () ->
      Value.succ a
    | _ -> Lisp.failf "memory-write requires two arguments" ()
end

module GetPC(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [] -> Eval.pc >>= Value.of_word
    | _ -> Lisp.failf
             "get-current-program-counter requires zero arguments" ()
end


module Add(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce null (Eval.binop Bil.PLUS)
end

module Sub(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce null (Eval.binop Bil.MINUS)
end

module Div(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce null (Eval.binop Bil.DIVIDE)
end

module SDiv(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = signed_reduce null (Eval.binop Bil.SDIVIDE)
end

module Mul(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce null (Eval.binop Bil.TIMES)
end

module Mod(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce null (Eval.binop Bil.MOD)
end

module SignedMod(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = signed_reduce null (Eval.binop Bil.SMOD)
end

module Lshift(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [x;y] -> Eval.binop Bil.lshift x y
    | _ -> Lisp.failf "Type error: lshift expects two arguments" ()
end

module Rshift(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [x;y] -> Eval.binop Bil.rshift x y
    | _ -> Lisp.failf "Type error: rshift expects two arguments" ()
end

module Arshift(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [x;y] -> Eval.binop Bil.arshift x y
    | _ -> Lisp.failf "Type error: arshift expects two arguments" ()
end

module Equal(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [] -> true_
    | x :: xs -> all (Value.equal x) xs
end

module NotEqual(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  module Equal = Equal(Machine)
  open Machine.Syntax

  let run xs = Equal.run xs >>= (Lib.Eval.unop Bil.NOT)
end


module Logand(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce negone (Eval.binop Bil.AND)
end

module Logor(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce null (Eval.binop Bil.OR)
end

module Logxor(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce null (Eval.binop Bil.XOR)
end

module Concat(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = reduce false_ Eval.concat
end

module Extract(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib


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
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [x] -> if Value.is_zero x then Value.b1 else Value.b0
    | _ -> Lisp.failf "not expects only one argument" ()
end

module Lnot(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [x] -> Eval.unop Bil.NOT x
    | _ -> Lisp.failf "lnot expects only one argument" ()
end

module Neg(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [x] -> Eval.unop Bil.NEG x
    | _ -> Lisp.failf "neg expects only one argument" ()
end

module Less(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let rec run = ordered Value.(<)
end

module Greater(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let rec run = ordered Value.(>)
end

module LessEqual(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let rec run = ordered Value.(<=)
end

module GreaterEqual(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let rec run = ordered Value.(>=)
end

module SymbolConcat(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run syms =
    Machine.List.map syms ~f:Value.Symbol.of_value >>= fun strs ->
    Value.Symbol.to_value (String.concat strs)
end

module SetSymbol(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib

  let run = function
    | [reg; x] ->
      Value.Symbol.of_value reg >>= fun reg ->
      let typ = Type.imm (Word.bitwidth (Value.to_word x)) in
      let var = Var.create reg typ in
      Eval.set var x >>| fun () -> x
    | _ -> Lisp.failf "set-symbol-value expects exactly two arguments" ()
end


module Stub(Machine : Primus.Machine.S) = struct
  module Lib = Make(Machine)
  open Machine.Syntax
  open Lib
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
      def "exec-addr" (one int @-> any) (module ExecAddr)
        "(exec-addr D) passes the control flow to D and never returns";
      def "exec-symbol" (one sym @-> any) (module ExecSym)
        "(exec-symbol D) passes the control flow to D and never returns";
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
        "(* X Y Z ...) returns the product of arguments or 0 if the list
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
        concatenation of symbols X,Y,Z,... ";
      def "set-symbol-value" (tuple [sym; a] @-> a) (module SetSymbol)
        "(set-symbol-value S X) sets the value of the symbol S to X.
         Returns X";
      def "reg-name" (one int @-> sym) (module RegName)
        "(reg-name N) returns the name of the register with the index N"
      ;
    ]
end

let init () =
  Primus.Machine.add_component (module Primitives) [@warning "-D"];
  Primus.Components.register_generic "lisp-primitives" (module Primitives)
    ~package:"bap"
    ~desc:"Provides the core set of Primus Lisp primitives.";
  Primus_lisp_ieee754.init ()
