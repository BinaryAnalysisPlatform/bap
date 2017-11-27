open Core_kernel
open Bap.Std
open Bap_primus_lisp_types

module Value = Bap_primus_value


let read_exn s = Type (int_of_string (String.strip s))

let read s = Option.try_with (fun () -> read_exn s)

module Check = struct
  let value arch typ w =
    let word_size = Size.in_bits (Arch.addr_size arch) in
    let size = Word.bitwidth (Value.to_word w) in
    match typ with
    | Word -> size = word_size
    | Type n -> size = n

  let arg arch typ arg =
    let word_size = Size.in_bits (Arch.addr_size arch) in
    match typ, Var.typ (Arg.lhs arg) with
    | Word, Type.Imm s -> word_size = s
    | Type n, Type.Imm m -> m = n
    | _ -> false

end


include Comparable.Make(struct
    type t = typ [@@deriving sexp, compare]
  end)

type t = typ [@@deriving sexp,compare]
