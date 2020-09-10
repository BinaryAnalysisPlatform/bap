open Bap_core_theory

open Core_kernel
open Graphlib.Std

open Bap_types.Std
open Bap_image_std

open KB.Syntax

module Driver = Bap_disasm_driver
module Insn = Bap_disasm_insn

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

  let init = Node.insert entry empty
end

(** Set of node parents.


    A node [n] is a parent of node [m] iff [n <> m /\ n <> Entry] and
    if every path from the [entry] node to [m] contains [n]. In other
    words, if it [n] strictly dominates [m] and is not the entry node.

    We compute parents using descending fixed point computation, with
    lattice of subsets. In our representation we use [Top] to denote the
    set of all nodes, and [Set xs] to denote a set of known nodes.

    The merge (meet) operator is the set intersection. The transfer
    function is defined as
    {v
      transfer Entry ps = ps
      transfer node Top = Top
      transfer node ps = (node) U ps
    v}

    Usually, dominators are computed using a transfer function that is
    defined as [transfer node ps = (node) U ps], which our third clause,
    however we specialized our tranfer function for two reasons

    1) efficiency - the first clause removes an explicit entry node,
    which by dominates all nodes. So we can save some space, as well
    as implement the [is_root] as little bit more efficient.

    2) robustness - usually, the dominators are computed for graphs
    where all nodes are dominated by the entry node. However, our case
    is not general, and we have components which are not reachable
    from the entry node. We do not want such componenets to change the
    parent propery of any node, since there is no path from the [entry]
    node to any other node, that will contain a node from an
    unconnected component.
*)
module Parent = struct
  type t = Top | Set of Word.Set.t [@@deriving compare, bin_io]
  let none = Set Word.Set.empty
  let unknown = Top
  let equal x y = match x,y with
    | Top,Top -> true
    | Set x, Set y -> Set.equal x y
    | _,_ -> false

  let is_root = function
    | Top -> false
    | Set x -> Set.is_empty x

  let is_known p = Option.is_some p

  let merge x y = match x,y with
    | Top,x | x,Top -> x
    | Set x, Set y -> Set (Set.inter x y)

  let transfer self parents =
    if Word.equal self Callgraph.entry
    then parents
    else match parents with
      | Top -> Top
      | Set parents -> Set (Set.add parents self)

  let pp ppf parents = match parents with
    | Top -> Format.fprintf ppf "Top"
    | Set parents ->
      Format.fprintf ppf "%a" Addr.pp_seq (Set.to_sequence parents)
end

module Parents = struct
  type t = (word,Parent.t) Solution.t
  include Binable.Of_binable(struct
      type t = (word * Parent.t) Seq.t [@@deriving bin_io]
    end)(struct
      type t = (word,Parent.t) Solution.t
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

let callgraph_of_disasm disasm =
  Driver.subroutines disasm |>
  Set.to_sequence |>
  KB.Seq.fold ~init:Callgraph.init ~f:(fun g entry ->
      Driver.explore disasm ~init:g
        ~entry
        ~block:(fun mem _ -> KB.return (Memory.min_addr mem))
        ~node:(fun n g ->
            let g = Callgraph.Node.insert n g in
            Theory.Label.for_addr (Word.to_bitvec n) >>= fun code ->
            KB.collect Theory.Label.is_subroutine code >>| function
            | Some true -> Callgraph.mark_as_root n g
            | _ -> g)
        ~edge:(fun src dst g ->
            let e = Callgraph.Edge.create src dst () in
            KB.return (Callgraph.Edge.insert e g)))

let empty =
  let root =
    Map.singleton (module Addr) Callgraph.entry Parent.none in {
    parents = Solution.create root Parent.unknown;
    entries = Set.empty (module Addr);
  }

let belongs {parents} ~entry:parent addr =
  Addr.equal parent addr || match Solution.get parents addr with
  | Top -> false
  | Set parents -> Set.mem parents parent

let siblings {parents} x y =
  Addr.equal x y ||
  match Solution.get parents x, Solution.get parents y with
  | Top,_|_,Top -> false
  | Set p1, Set p2 ->
    if Set.is_empty p1 then Set.mem p2 x else
    if Set.is_empty p2 then Set.mem p1 x else
      not @@ Set.is_empty (Set.inter p1 p2)

let entries graph parents =
  let init = Set.empty (module Addr) in
  Callgraph.nodes graph |> Seq.fold ~init ~f:(fun entries n ->
      if Word.equal n Callgraph.entry then entries
      else match Solution.get parents n with
        | Parent.Top -> entries
        | Set parents ->
          if Set.is_empty parents
          then Set.add entries n
          else entries)

let update {parents} disasm =
  callgraph_of_disasm disasm >>| fun graph ->
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

let entries {entries} = entries

let equal s1 s2 =
  Set.equal s1.entries s2.entries &&
  Solution.equal ~equal:Parent.equal s1.parents s2.parents

let domain = KB.Domain.flat ~empty ~equal "callgraph"
