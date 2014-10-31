open Core_kernel.Std
open Bap_common

module T = struct
  open Arch
  type t = arch
  with bin_io, compare, sexp

  let hash = Hashtbl.hash

  let module_name = "Bap_arch"

  let to_string = function
    | X86_32 -> "X86_32"
    | X86_64 -> "X86_64"
    | ARM    -> "ARM"

  let of_string s =
    match Fn.compose String.uppercase String.strip s with
    | "X86" | "X86-32" | "X86_32" | "IA32" | "IA-32" | "I386" -> X86_32
    | "X86-64" | "X86_64" | "AMD64" | "x64" -> X86_64
    | "arm" | "ARM" -> ARM
    | s -> failwithf "Arch.of_string: Unknown arch '%s'" s ()
end

(* derive Identifiable interface from Core *)
include T
include Identifiable.Make(T)
