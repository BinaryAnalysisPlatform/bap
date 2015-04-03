open Core_kernel.Std
open Bap_common

module T = struct
  open Arch
  type t = arch
  with bin_io, compare, sexp

  let hash = Hashtbl.hash

  let module_name = "Bap_arch"

  let to_string = function
    | `x86 -> "i386"
    | arch -> Sexp.to_string (sexp_of_t arch)
  let pp ch arch = Format.fprintf ch "%s" (to_string arch)

  let normalize s =
    match Fn.compose String.lowercase String.strip s with
    | "x86" | "x86-32" | "x86_32" | "ia32" | "ia-32"
    | "i386"| "i486" | "i586" | "i686" -> "x86"
    | "x86-64" | "x86_64" | "amd64" | "x64" | "x86_64h" -> "x86_64"
    | "powerpc" -> "ppc"
    | "arm" -> "armv7"
    | "xscale"  -> "arm"
    | "thumbv4t" -> "armv4t"
    | "armv5t" | "armv5e" | "thumbv5" | "thumbv5e" -> "armv5"
    | "thumbv6" -> "armv6"
    | "thumbv7" -> "armv7"
    | "powerpc64" | "ppu" -> "ppc64"
    | "sparc64" -> "sparcv9"
    | "s390x" -> "systemz"
    | "arm64" -> "aarch64"
    | "arm64_be" -> "aarch64_be"
    | s -> s

  let of_string_exn s =
    t_of_sexp (sexp_of_string (normalize s))

  let of_string s =
    Option.try_with (fun () -> of_string_exn s)

  let addr_size : t -> addr_size = function
    | #arm | `x86 | `ppc | `mips | `mipsel | `sparc
    | `nvptx | `hexagon |  `r600 | `xcore  -> `r32

    | #aarch64 | `mips64el |`sparcv9|`ppc64le|`x86_64
    | `ppc64|`nvptx64 | `systemz | `mips64 -> `r64

  let endian : t -> endian = function
    | `armeb  | `aarch64_be | `thumbeb -> BigEndian
    | `mipsel | `mips64el | `ppc64le -> LittleEndian
    | #arm | #x86 | #aarch64 | #r600
    | #hexagon | #nvptx | #xcore -> LittleEndian
    | #ppc | #mips | #sparc | #systemz   -> BigEndian
end

(* derive Identifiable interface from Core *)
include T
include Regular.Make(T)
