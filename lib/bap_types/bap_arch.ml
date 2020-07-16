open Core_kernel
open Bap_core_theory
open Regular.Std
open Bap_common

module T = struct
  open Arch
  type t = arch
  [@@deriving bin_io, compare, sexp]

  let hash = Hashtbl.hash

  let module_name = Some "Bap.Std.Arch"
  let version = "1.0.0"

  let to_string = function
    | `x86 -> "i386"
    | `systemz -> "s390x"
    | `ppc -> "powerpc"
    | `ppc64 -> "powerpc64"
    | `ppc64le -> "powerpc64le"
    | arch -> Sexp.to_string (sexp_of_t arch)
  let pp ch arch = Format.fprintf ch "%s" (to_string arch)

  let normalize s =
    match Fn.compose String.lowercase String.strip s with
    | "x86" | "x86-32" | "x86_32" | "ia32" | "ia-32"
    | "i386"| "i486" | "i586" | "i686" -> "x86"
    | "x86-64" | "x86_64" | "amd64" | "x64" | "x86_64h" -> "x86_64"
    | "powerpc" -> "ppc"
    | "arm" -> "armv7"
    | "thumb" -> "thumbv7"
    | "armeb" -> "armv7eb"
    | "thumbeb" -> "thumbv7eb"
    | "xscale"  -> "arm"
    | "powerpc64" | "ppu" -> "ppc64"
    | "powerpc64le" -> "ppc64le"
    | "sparc64" -> "sparcv9"
    | "s390x" -> "systemz"
    | "arm64" -> "aarch64"
    | "arm64_be" -> "aarch64_be"
    | s -> s

  let of_string_exn s =
    t_of_sexp (sexp_of_string (normalize s))

  let of_string s =
    Option.try_with (fun () -> of_string_exn s)

  let from_string s = match of_string s with
    | None -> `unknown
    | Some s -> s

  let addr_size : t -> addr_size = function
    | #arm | #armeb | #thumb | #thumbeb
    | `x86 | `ppc | `mips | `mipsel | `sparc
    | `nvptx | `hexagon |  `r600 | `xcore  -> `r32
    | `unknown -> `r32

    | #aarch64 | `mips64el |`sparcv9|`ppc64le|`x86_64
    | `ppc64|`nvptx64 | `systemz | `mips64 -> `r64

  let endian : t -> endian = function
    | #armeb  | `aarch64_be | #thumbeb -> BigEndian
    | `mipsel | `mips64el | `ppc64le -> LittleEndian
    | #arm | #thumb | #x86 | #aarch64 | #r600
    | #hexagon | #nvptx | #xcore | #unknown -> LittleEndian
    | #ppc | #mips | #sparc | #systemz   -> BigEndian

  let equal x y = compare_arch x y = 0

  let domain =
    KB.Domain.flat ~empty:`unknown ~equal ~inspect:sexp_of_t "arch"

  let slot = KB.Class.property ~package:"bap"
      Theory.Program.cls "arch" domain
      ~persistent:(KB.Persistent.of_binable (module struct
                     type t = arch [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"an ISA of the program"


  let _arch_of_unit_ : unit =
    KB.Rule.(declare ~package:"bap" "arch-of-unit" |>
             require Theory.Label.unit |>
             require Theory.Unit.Target.arch |>
             provide slot |>
             comment "compute arch from the unit target defintions");
    let open KB.Syntax in
    let (>>=?) x f =
      x >>= function
      | None -> KB.return `unknown
      | Some x -> f x in
    KB.promise slot @@ fun obj ->
    KB.collect Theory.Label.unit obj >>=? fun unit ->
    KB.collect Theory.Unit.Target.arch unit >>=? fun arch ->
    KB.return (from_string arch)


end

include T
include Regular.Make(T)
