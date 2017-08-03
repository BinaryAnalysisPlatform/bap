open Core_kernel.Std
open Bap.Std
open Bap_primus.Std

module Primitives(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Eval = Primus.Interpreter.Make(Machine)
  module Primitive = Primus.Lisp.Primitive
  module Lisp = Primus.Lisp.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let all f args = List.exists args ~f |> Value.of_bool
  let is_zero args = all Value.is_zero args
  let is_positive args = all Value.is_positive args
  let is_negative args = all Value.is_negative args
  let addr_width =
    Machine.arch >>| Arch.addr_size >>| Size.in_bits

  let word_width args =
    addr_width >>= fun width ->
    match args with
    | [] -> Value.of_int ~width width
    | x :: xs -> Value.of_int ~width (Value.bitwidth x)

  let negone = Value.one 8
  let zero = Value.zero 8

  let exit _ =
    Eval.halt >>|
    Nothing.unreachable_code >>= fun () ->
    Value.b0

  let allocate = function
    | [addr; size] ->
      let n = Word.to_int (Value.to_word size) in
      if Result.is_error n
      then negone
      else Memory.allocate (Value.to_word addr) (Or_error.ok_exn n) >>=
        fun () -> zero
    | _ -> Lisp.failf "allocate requires two arguments" ()

  let machine_int x = addr_width >>= fun width -> Value.of_int ~width x

  let output_char = function
    | [] | [_] -> machine_int 0
    | fd :: words ->
      if Value.is_zero fd
      then
        List.iter words ~f:(fun w ->
            Word.enum_chars (Value.to_word w) LittleEndian |>
            Seq.hd |> Option.iter ~f:(print_char));
      machine_int (List.length words)

  let memory_read = function
    | [x] -> Eval.load x LittleEndian `r8
    | _ -> Lisp.failf "memory-read requires one argument" ()

  let memory_write = function
    | [a;x] ->
      Eval.store a x LittleEndian `r8 >>= fun () ->
      Value.succ a
    | _ -> Lisp.failf "memory-write requires two arguments" ()


  let primitive name code = Primitive.create name code

  let defs () = [
    primitive "is-zero" is_zero;
    primitive "is-positive" is_positive;
    primitive "is-negative" is_negative;
    primitive "word-width"  word_width;
    primitive "output-char" output_char;
    primitive "exit-with" exit;
    primitive "memory-read" memory_read;
    primitive "memory-write" memory_write;
    primitive "memory-allocate" allocate;
  ]
end


module Component(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  let init () = Lisp.link_primitives (module Primitives)
end

let () = Primus.Machine.add_component (module Component)
