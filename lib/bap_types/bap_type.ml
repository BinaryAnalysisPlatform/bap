open Core_kernel.Std
open Format

open Bap_common
open Type

module T = struct
  type t = typ with bin_io, compare, sexp
  let module_name = "Bap_type"

  let pp fmt = function
    | Bool  -> fprintf fmt "bool"
    | Reg r -> fprintf fmt "%a" Bap_size.pp r
    | TMem (idx, elm) ->
      fprintf fmt "%a?%a" Bap_size.pp (idx :> size) Bap_size.pp elm

  let hash = Hashtbl.hash
end

module Export = struct
  let bool_t  = bool
  let reg8_t  = reg `r8
  let reg16_t = reg `r16
  let reg32_t = reg `r32
  let reg64_t = reg `r64
  let mem32_t = tmem `r32
  let mem64_t = tmem `r64
end


include Regular.Make(T)
