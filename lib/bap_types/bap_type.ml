open Core_kernel
open Regular.Std
open Format

open Bap_common
open Type

module T = struct
  type t = typ [@@deriving bin_io, compare, sexp]
  let module_name = Some "Bap.Std.Type"
  let version = "1.0.0"


  let pp fmt = function
    | Unk -> fprintf fmt "unk"
    | Imm n -> fprintf fmt "u%u" n
    | Mem (idx, elm) ->
      fprintf fmt "%a?%a" Bap_size.pp (idx :> size) Bap_size.pp elm

  let hash = Hashtbl.hash
end

module Export = struct
  let bool_t  = imm  1
  let reg4_t  = imm  4
  let reg8_t  = imm  8
  let reg16_t = imm 16
  let reg32_t = imm 32
  let reg64_t = imm 64
  let reg128_t = imm 128
  let reg256_t = imm 256
  let mem32_t = mem `r32
  let mem64_t = mem `r64
end


include Regular.Make(T)
