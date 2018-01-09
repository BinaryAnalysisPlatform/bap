open Core_kernel.Std
open Bap.Std
open Bap_primus.Std

module Lib(Machine : Primus.Machine.S) = struct
  module Eval = Primus.Interpreter.Make(Machine)
  module Primitive = Primus.Lisp.Primitive
  module Lisp = Primus.Lisp.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  include Machine.Syntax
  let all f args = List.exists args ~f |> Value.of_bool
  let addr_width =
    Machine.arch >>| Arch.addr_size >>| Size.in_bits
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
    | x :: xs -> Value.of_int ~width (Value.bitwidth x)
end

module ExitWith(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = all Value.is_negative
  let run _ =
    Eval.halt >>|
    Nothing.unreachable_code >>= fun () ->
    Value.b0
end

module OutputChar(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let machine_int x = addr_width >>= fun width -> Value.of_int ~width x
  let run = function
    | [] | [_] -> machine_int 0
    | fd :: words ->
      if Value.is_zero fd
      then begin
        List.iter words ~f:(fun w ->
            Word.enum_chars (Value.to_word w) LittleEndian |>
            Seq.hd |> Option.iter ~f:(Out_channel.output_char stdout));
        Out_channel.flush stdout;
      end;
      machine_int (List.length words)
end

module MemoryAllocate(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let negone = Value.one 8
  let zero = Value.zero 8

  let run = function
    | [addr; size] ->
      let n = Word.to_int (Value.to_word size) in
      if Result.is_error n
      then negone
      else Memory.allocate (Value.to_word addr) (Or_error.ok_exn n) >>=
        fun () -> zero
    | _ -> Lisp.failf "allocate requires two arguments" ()
end


module MemoryRead(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x] -> Eval.load x LittleEndian `r8
    | _ -> Lisp.failf "memory-read requires one argument" ()
end

module MemoryWrite(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [a;x] ->
      Eval.store a x LittleEndian `r8 >>= fun () ->
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
  let run = reduce null Value.add
end

module Sub(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null Value.sub
end

module Div(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null Value.div
end

module SDiv(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = signed_reduce null Value.div
end

module Mul(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null Value.Syntax.( * )
end

module Mod(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null Value.modulo
end

module SignedMod(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = signed_reduce null Value.modulo
end

module Lshift(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x;y] -> Value.lshift x y
    | _ -> Lisp.failf "Type error: shift-left expects two arguments" ()
end

module Rshift(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x;y] -> Value.rshift x y
    | _ -> Lisp.failf "Type error: shift-left expects two arguments" ()
end

module Arshift(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x;y] -> Value.arshift x y
    | _ -> Lisp.failf "Type error: shift-left expects two arguments" ()
end

module Equal(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [] -> true_
    | x :: xs -> all (Value.equal x) xs
end

module NotEqual(Machine : Primus.Machine.S) = struct
  include Equal(Machine)
  let run xs = run xs >>= Value.lnot
end


module Logand(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce negone Value.logand
end

module Logor(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null Value.logor
end

module Logxor(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce null Value.logxor
end

module Concat(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = reduce false_ Value.concat
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
      Value.extract ~hi ~lo x
    | _ -> Lisp.failf "extract expects exactly three arguments" ()
end

module Not(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x] -> Value.lnot x
    | _ -> Lisp.failf "not expects only one argument" ()
end

module Neg(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = function
    | [x] -> Value.neg x
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



module Stub(Machine : Primus.Machine.S) = struct
  include Lib(Machine)
  let run = Lisp.failf "not implemented"
end

module Primitives(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Lisp = Primus.Lisp.Make(Machine)

  let init () =
    let open Primus.Lisp.Type.Spec in
    let def name types closure =
      Lisp.define ~types name closure in
    Machine.sequence [
      def "is-zero" (all any @-> bool) (module IsZero);
      def "is-positive" (all any @-> bool) (module IsPositive);
      def "is-negative" (all any @-> bool) (module IsNegative);
      def "int-width" (unit @-> int)  (module WordWidth);
      def "output-char" (one int // all byte @-> int) (module OutputChar);
      def "exit-with" (one int @-> any) (module ExitWith);
      def "memory-read" (one int @-> byte) (module MemoryRead);
      def "memory-write" (tuple [int; byte] @-> int) (module MemoryWrite);
      def "memory-allocate" (tuple [int; int] @-> byte) (module MemoryAllocate);
      def "get-current-program-counter" (unit @-> int) (module GetPC);
      def "+" (all a @-> a) (module Add);
      def "-" (all a @-> a) (module Sub);
      def "*" (all a @-> a) (module Mul);
      def "/" (all a @-> a) (module Div);
      def "s/" (all a @-> a) (module SDiv);
      def "mod" (all a @-> a) (module Mod);
      def "signed-mod" (all a @-> a) (module SignedMod);
      def "lshift" (tuple [a; a] @-> a) (module Lshift);
      def "rshift" (tuple [a; a] @-> a) (module Rshift);
      def "arshift" (tuple [a; a] @-> a) (module Arshift);
      def "=" (all a @-> bool) (module Equal);
      def "/=" (all a @-> bool) (module NotEqual);
      def "logand" (all a @-> a) (module Logand);
      def "logor" (all a @-> a) (module Logor);
      def "logxor" (all a @-> a) (module Logxor);
      def "concat" (all any @-> any) (module Concat);
      def "extract" (tuple [any; any; any] @-> any) (module Extract);
      def "not" (one a @-> a) (module Not);
      def "neg" (one a @-> a) (module Neg);
      def "<" (all a @-> bool) (module Less);
      def ">" (all a @-> bool) (module Greater);
      def "<=" (all a @-> bool) (module LessEqual);
      def ">=" (all a @-> bool) (module GreaterEqual);
    ]
end

let () = Primus.Machine.add_component (module Primitives)
