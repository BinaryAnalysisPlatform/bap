open Bap.Std
open Core

module Dis = Disasm_expert.Basic
type elem = mem * Dis.full_insn option

type t = Superset_impl.t

module ISG : sig
  open Graphlib.Std

  (** Returns a list of those addresses for which the argument
  address could be the immediate next instruction of. *)
  val ancestors : t -> addr -> addr list

  (** Returns a list of those addresses for which the argument
  address could potentially lead to. *)
  val descendants : t -> addr -> addr list

  val mem_vertex : t -> addr -> bool

  val iter_vertex : t -> (addr -> unit) -> unit

  val fold_vertex : t -> (addr -> 'a -> 'a) -> 'a -> 'a

  val fold_edges : t -> (addr -> addr -> 'a -> 'a) -> 'a -> 'a

  val check_connected : t -> addr -> addr -> bool

  (** Adds an associated directed link from src to dst, tracking
  addresses if they are not already. *)
  val link : t -> addr -> addr -> t

  (** Removes a link between two addresses, but not stop tracking
  those addresses even if they each have no more links *)
  val unlink : t -> addr -> addr -> t

  (** Uses strongly connected components to determine loop lists, but
  does no filtering. *)
  val raw_loops : t -> addr list list

  val dfs_fold :
    ?visited:Addr.Hash_set.t -> t -> pre:('a -> addr -> 'a) ->
    post:('a -> addr -> 'a) -> 'a -> addr -> 'a

  val dfs : ?terminator:(addr -> bool) -> ?visited:Addr.Hash_set.t ->
            ?pre:(addr -> unit) -> ?post:(addr -> unit) ->
            (t -> addr -> addr list) -> t -> addr -> unit

  val to_list : t -> (addr * addr) list

  val fixpoint : ?steps:int ->
                 ?start:addr ->
                 ?rev:bool ->
                 ?step:(int -> addr -> 'a -> 'a -> 'a) ->
                 t ->
                 init:(addr, 'a) Solution.t ->
                 equal:('a -> 'a -> bool) ->
                 merge:('a -> 'a -> 'a) ->
                 f:(addr -> 'a -> 'a) -> (addr, 'a) Solution.t
    
  (** Print the graph to file for a given superset *)
  val print_dot : ?colorings:Addr.Hash_set.t String.Map.t -> t -> unit

  (** For all items in the address hash set, remove them from the
  superset. This is a raw removal, so it does not mark bad and
  traverse to perform maximal removals. *)
  val filter : t -> Addr.Hash_set.t -> t

  (** Prints the isg via a formatter in gml format. *)
  val format_isg : ?format:Format.formatter -> t -> unit

  (** Prints the isg to a string buffer in gml format. *)
  val isg_to_string : t -> string
end

module Core : sig

  (** Insert the memory and disassembled instruction into the superset *)
  val add : t -> mem -> Dis.full_insn option -> t
  (** Stops tracking an address. Can unbalance the internal
  structure, requiring further balanace and trim calls *)
  val remove : t -> addr -> t
  val empty : arch -> t

  (** This primary core function is the core of disassembly, and
  simply reads each byte consecutively in memory by address
  successor. *)
  val run_seq :
    ('a, 'b) Dis.t ->
    mem ->
    (mem * (Dis.asm, Dis.kinds) Dis.insn option) seq

  val seq_of_addr_range : addr -> int -> addr seq
    
  (** This primary core function is the core of disassembly, and simply 
   reads each byte consecutively in memory by address successor. It 
   is alike to run_seq, but it hides the sequence part, and accepts 
   a parameter lambda. *)
  val run :
    ('a, 'b) Dis.t ->
    accu:'c ->
    f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> 'c -> 'c) ->
    mem -> 'c

  (** This function is the fundamental superset disassembly, and
      disassembles at the addresses given by the supplied sequence. *)
  val disasm :
    ?backend:string -> addrs:addr seq -> accu:'a ->
    f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> 'a -> 'a) ->
    Arch.t -> mem -> 'a Or_error.t

  (** This function is the core of disassembly, and simply 
   reads each byte consecutively in memory by address successor. It 
   builds the disassembler and runs the superset behind the 
   scenes. One can accumulate with any arbitrary type. Later 
   accessories tuck the results into a custom superset 
   representation with graph specializations suited to the 
   invariants, heuristics and performance that are vital to good 
   operation. *)
  val disasm_all :
    ?backend:string -> accu:'a ->
    f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> 'a -> 'a) ->
    Arch.t -> mem -> 'a Or_error.t

  (** Lift a single disassembled memory and instruction pair *)
  val lift_insn :
    t -> (mem * Dis.full_insn option) -> (bil) option

  (** Given an address, lift a single instruction at that address *)
  val lift_at :
    t -> (addr) -> bil option
    
  (** The primary disassembler design interface. Implementing a
      disassembler from the ground up almost certainly uses this as
      it loads the memory images into the superset. *)
  val update_with_mem :
    ?backend:string -> ?addrs:addr seq ->
    ?f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> t -> t) ->
    t -> mem -> t

  (** Marking an address bad means that it is temporarily maintained
      until a later phase in which it is removed, together with as
      many other other instructions that might have accessed it as
      possible. *)
  val mark_bad : t -> addr -> unit

  (** Internally, performance is important, and it may be the case
      that after many bad instructions are marked and removed, that
      there is some mismatch between the internal tracking that is
      done. So, this library imposes that after a trim that this be
      called. *)
  val rebalance : t -> t

  (** This removes bad entries from the tracker without pruning them
      from the superset. If this is called before trimming the
      superset, then the bad instructions that were marked are no
      longer distinguishable as previously. *)
  val clear_bad : t -> addr -> unit

  val clear_each : t -> Addr.Hash_set.t -> unit
    
  (** Removes all addresses from being tracked as bad, without
      removing them from the superset. *)
  val clear_all_bad : t -> unit

  (** Returns a copy of the set of addresses that have been marked
      bad *)
  val copy_bad : t -> Addr.Hash_set.t

  val lookup : t -> addr -> (mem * Dis.full_insn option) option

  (** Accumulate over each current disassembled instruction in the
      current superset. *)
  val fold : t -> init:'a -> f:(key:addr -> data:elem -> 'a -> 'a) -> 'a
  val mem : t -> addr -> bool
end

module Inspection : sig
  (** Returns if the addr is still in the container representing the 
   superset. Note, an addr may be marked as bad for removal, but isn't 
   removed until the trim module traverses for cleaning. Marking for 
   removal can be reversed by calling clear_bad. *)
  val contains_addr : t -> addr -> bool

  val get_endianness : t -> endian option

  val get_arch : t -> arch

  (** Mark and track the argument address for removal upon trimming. *)
  val num_bad : t -> int

  (** Current number of disassembled instructions in the superset *)
  val count : t -> int

  (** Returns information reporting unbalanced the superset has
      become, which ideally resides at zero. *)
  val count_unbalanced : t -> int

  (** Returns a tuple representing differences of addresses not in
      each of either the disassembly set and the graph. *)
  val unbalanced_diff : t -> (Addr.Set.t * Addr.Set.t)

  (** Returns true if the address is currently marked for removal *)
  val is_bad_at : t -> addr -> bool

  (** Returns the length of the instruction at addr *)
  val len_at : t -> addr -> int

  (** Returns the total overall size of memory in bytes that has been
      processed by the superset *)
  val total_bytes : t -> int

  (** A carefully written function that visits the address in the body 
   of any instruction that is longer than one byte. So, addr + 1, 
   addr + 2, ... addr + n. *)
  val static_successors : t -> mem -> Dis.full_insn option ->
    Brancher.dests

  val get_memmap : t -> value memmap

  (** Returns the entry of the image that was originally loaded into
      this superset, if any was used or if it was discovered when
      loaded. *)
  val get_main_entry : t -> addr option

  (** Returns the filename, if any was used, to compute this superset. *)
  val filename : t -> string option
end

module Cache : sig
  open Bap_knowledge
  open Bap_core_theory
  open Theory

  val package : string
  val sym_label : program Knowledge.obj KB.t
  val superset_graph_t :
    (addr * addr) list option Knowledge.domain
  val superset_graph_persistent :
    (addr * addr) list option Knowledge.persistent
  val superset_graph :
    (program, (addr * addr) list option) Knowledge.slot

end
     
module Occlusion : sig
  (** For each address within the set, search within the body of the
      corresponding disassembled instruction, looking for conflicts
      (shingles). Add all such addresses, including the original
      instruction if any occurred in it's body. Only exclude an
      address from the output set if it didn't occur in the body of
      another, and if no instruction conflicted. *)
  val conflicts_within_insns : t -> Addr.Set.t -> Addr.Set.t

  (** For a given address, produce a set of all addresses
      within the body of the disassembly that possess a conflict. *)
  val conflicts_within_insn_at :
    t -> ?mem:(addr -> bool) -> ?conflicts:Addr.Set.t -> addr -> Addr.Set.t

  (** For a given superset, look at every single instruction. Defers
      to conflicts_within_insns in implementation. *)
  val find_all_conflicts : ?mem:(addr -> bool) -> t -> Addr.Set.t

  (** A sequence view of conflicts within a given disassembled
      instruction at a given address for a given length. *)
  val range_seq_of_conflicts : mem:(addr -> bool) -> addr -> int -> addr seq

  (** At a given address, return all addresses within its body for
      which there exists another conflicting instruction. *)
  val conflict_seq_at : t -> addr -> addr seq

  (** Compute at a given address given with those addresses reside
      within the body of the disassembly. *)
  val with_data_of_insn :
    t -> addr -> f:(addr -> unit) -> unit
end

(** The instruction immediately after argument addr. *)
val fall_through_of : t -> addr -> addr
(** A helper function meant to tuck away the representation 
 underneath that tracks bad addresses. *)
val is_fall_through :
  t -> addr -> addr -> bool

(** Entry is a connotation that denotes no other instruction leads
    into this one. *)
val is_entry : t -> addr -> bool

(** Return all addresses in a set for which is_entry returns true *)
val entries_of_isg : t -> Addr.Hash_set.t

(** A frond point is a point that is distant most of a terminating
 ** instruction, meaning it may be the first instruction of a
 ** function. However, these could actually occur either within or
 ** beyond the body of instruction sequence intended.  *)
val is_frond_point : t -> addr -> bool

(** Return all addresses in a set for which is_front_point is true *)
val frond_of_isg : t -> Addr.Hash_set.t
  
(** Return the set of addreses for which more than one other
    instruction targets it. *)
val mergers : t -> Addr.Set.t

(** Checks for the existence of successors other than fall through. *)
val is_branch : t -> addr -> bool

(** Returns a set of the addresses for which is_branch is true *)
val get_branches : t -> Addr.Hash_set.t

(** DFS traverses the set of addresses that are currently marked as
    bad, applying functions pre and post, from the bad to all
    ancestors. *)
val with_bad :
  t -> ?visited:Addr.Hash_set.t -> pre:('b -> addr -> 'c) ->
  post:('c -> addr -> 'b) -> 'b -> 'b

(** Take the raw superset of a given file name of a compiled object of
    any kind that Image can parse. *)
val superset_disasm_of_file :
  ?backend:string -> 
  ?f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> t -> t) ->
  ?addrs:addr seq -> string -> t

