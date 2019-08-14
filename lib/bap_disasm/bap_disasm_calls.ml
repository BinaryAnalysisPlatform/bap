open Bap_core_theory

module OCamlGraph = Graph

open Core_kernel
open Graphlib.Std

open Bap_types.Std
open Bap_image_std

open KB.Syntax

module Driver = Bap_disasm_driver
module Insn = Bap_disasm_insn


module Parent = struct
  let none = Word.b0
  let unknown = Word.b1
  let equal = Word.equal
  let is_root p = equal p none
  let is_known p = not (equal p unknown)
  let merge x y =
    if equal x unknown then y else
    if equal y unknown then x else
    if equal x y then x else none

  let transfer self parent =
    if equal parent none then self else parent
end

module Parents = struct
  type t = (word,word) Solution.t
  include Binable.Of_binable(struct
      type t = (word * word) Seq.t [@@deriving bin_io]
    end)(struct
      type t = (word,word) Solution.t
      let to_binable = Solution.enum
      let of_binable xs =
        let init = ok_exn @@
          Map.of_increasing_sequence
            (module Word) xs in
        Solution.create init Parent.unknown
    end)
end

type input = Driver.state
type output = {
  parents : Parents.t;
  entries : Addr.Set.t;
} [@@deriving bin_io]

type t = output [@@deriving bin_io]

module Callgraph = struct
  let entry = Word.b0
  let exit = Word.b1
  let is_entry = Word.equal entry
  include Graphlib.Make(Addr)(Unit)
  let mark_as_root n g =
    if Word.equal n entry then g
    else
      let e = Edge.create entry n () in
      Edge.insert e g
end

let string_of_node n =
  sprintf "%S" @@ if Callgraph.is_entry n
  then "entry"
  else Addr.string_of_value n

let pp_callgraph ppf graph =
  Graphlib.to_dot (module Callgraph) graph
    ~formatter:ppf
    ~string_of_node


let pp_roots ppf graph =
  Graphlib.to_dot (module Callgraph) graph
    ~formatter:ppf
    ~string_of_node:(fun s ->
        sprintf "%S" (Addr.string_of_value s))

let of_disasm disasm =
  Driver.explore disasm ~init:Callgraph.empty
    ~block:(fun mem _ -> KB.return (Memory.min_addr mem))
    ~node:(fun n g ->
        let g = Callgraph.Node.insert n g in
        Theory.Label.for_addr (Word.to_bitvec n) >>= fun code ->
        KB.collect Theory.Label.is_subroutine code >>| function
        | Some true -> Callgraph.mark_as_root n g
        | _ -> g)
    ~edge:(fun src dst g ->
        let e = Callgraph.Edge.create src dst () in
        KB.return (Callgraph.Edge.insert e g))


let empty =
  let root =
    Map.singleton (module Addr) Callgraph.entry Parent.none in {
    parents = Solution.create root Parent.unknown;
    entries = Set.empty (module Addr);
  }

let connect_inputs g =
  Callgraph.nodes g |>
  Seq.fold ~init:g ~f:(fun g n ->
      if Callgraph.Node.degree ~dir:`In n g = 0
      then Callgraph.mark_as_root n g
      else g)

let connect_unreachable_scc g =
  Graphlib.depth_first_search (module Callgraph) g
    ~start:Callgraph.entry
    ~init:g
    ~start_tree:Callgraph.mark_as_root

let callgraph disasm =
  of_disasm disasm >>|
  connect_inputs >>|
  connect_unreachable_scc

let parent parents addr =
  let parent = Solution.get parents addr in
  if Parent.equal parent Parent.none then addr else parent

let entries graph parents =
  let init = Set.empty (module Addr) in
  Callgraph.nodes graph |> Seq.fold ~init ~f:(fun entries n ->
      if not (Parent.is_root n) && Parent.equal (parent parents n) n
      then Set.add entries n
      else entries)


let pp_calls ppf (parents,graph) =
  Graphlib.to_dot (module Callgraph) graph
    ~formatter:ppf
    ~string_of_node
    ~node_attrs:(fun n ->
        if parent parents n = n
        then [`Shape `Diamond; `Style `Filled]
        else [])

let update {parents} disasm =
  callgraph disasm >>| fun graph ->
  Graphlib.fixpoint (module Callgraph) graph
    ~init:parents
    ~start:Callgraph.entry
    ~equal:Parent.equal
    ~merge:Parent.merge
    ~f:Parent.transfer
  |> fun parents ->
  {
    parents;
    entries = entries graph parents;
  }

let entry {parents} addr = parent parents addr

let entries {entries} = entries

let equal s1 s2 =
  Set.equal s1.entries s2.entries &&
  Solution.equal ~equal:Word.equal s1.parents s2.parents


let domain = KB.Domain.flat ~empty ~equal "callgraph"
