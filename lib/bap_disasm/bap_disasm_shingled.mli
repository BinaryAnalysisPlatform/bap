open Core_kernel.Std
open Bap_arch
open Bap_types.Std

module Memory = Bap_memory
open Memory
module Memmap = Bap_memmap
module Insn= Bap_disasm_insn

module Dis = Bap_disasm_basic

(** This is a conservative byte offset disassembler; everything that *)
    (** is returned in a list of type mem * insn option. It is tail
    recursive  *)
val all_shingles : ('a, 'b) Dis.t -> Memory.t -> init:('c) ->
  at:('c -> Memory.t * (Dis.asm, Dis.kinds) Dis.insn option -> 'c) ->
  'c

(** Applies a couple of techniques to try and sheer off noise, *)
    (** dropping obviously recognizable data, and attempting a maximal
    recognition backward propagation of fall through and
    subsuequently a probabilistic finite state machine for selecting
    maximally probable execution sequences based on a corpus of
    trianing data. *)
val sheered_shingles :?backend:string -> arch ->
  ?dis:(Dis.empty, Dis.empty) Dis.t -> Memory.t -> Insn.t Memmap.t
