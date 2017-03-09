open Core_kernel.Std
open Bap.Std
open Primus.Std

module Primitives(Machine : Machine.S) = struct
  open Machine.Syntax
  module Primitive = Lisp.Primitive
  module Lisp = Lisp.Make(Machine)
  module Memory = Memory.Make(Machine)

  let all f args = Machine.return (Word.of_bool (List.exists args ~f))
  let is_zero args = all Word.is_zero args
  let is_positive args = all Word.is_positive args
  let is_negative args = all Word.is_negative args
  let width_of_ctxt ctxt =
    Project.arch ctxt#project |> Arch.addr_size |> Size.in_bits
  let word_width args =
    Machine.get () >>| width_of_ctxt >>| fun width ->
    match args with
    | [] -> Word.of_int ~width width
    | x :: xs -> Word.of_int ~width (Word.bitwidth x)

  let negone = Word.ones 8
  let zero = Word.zero 8

  let exit = function
    | [rval] ->
      Machine.update (fun ctxt -> ctxt#set_next None) >>= fun () ->
      Machine.return rval
    | _ -> Lisp.failf "exit requires only one argument" ()

  let machine_int x =
    Machine.get () >>| width_of_ctxt >>| fun width -> Word.of_int ~width x

  let output_char = function
    | [] | [_] -> machine_int 0
    | fd :: words ->
      if Word.is_zero fd
      then
        List.iter words ~f:(fun w ->
            Word.enum_chars w LittleEndian |>
            Seq.hd |> Option.iter ~f:(print_char));
      machine_int (List.length words)

  let memory_read = function
    | [x] -> Memory.load x
    | _ -> Lisp.failf "memory-read requires one argument" ()

  let memory_write = function
    | [a;x] -> Memory.save a x >>| fun () -> Addr.succ a
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
  ]
end


module Component(Machine : Machine.S) = struct
  module Lisp = Lisp.Make(Machine)
  let init () =
    Lisp.link_primitives (module Primitives)
end

let () = Machine.add_component (module Component)
