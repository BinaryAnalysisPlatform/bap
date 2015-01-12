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

  let pp ch arch = Format.fprintf ch "%s" (to_string arch)

  let of_string s =
    match Fn.compose String.uppercase String.strip s with
    | "X86" | "X86-32" | "X86_32" | "IA32" | "IA-32" | "I386" -> Some X86_32
    | "X86-64" | "X86_64" | "AMD64" | "x64" -> Some X86_64
    | "arm" | "ARM" -> Some ARM
    | s -> None
end

(* derive Identifiable interface from Core *)
include T
include Regular.Make(T)
