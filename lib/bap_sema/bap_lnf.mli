open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std

type t with sexp_of
type tree with sexp_of
type block = Block.Cfg.Block.t with sexp_of
type blocks = Block.Cfg.Block.Set.t with sexp_of

val trees : t -> tree list
val children : tree -> t

val headers : tree -> blocks
val bodies : tree -> blocks


val create : ?bound:mem -> Block.t -> t
(* val of_cfg : Block.Cfg.t -> t *)
