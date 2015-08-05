open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_graph_intf
open Bap_graph_regular_intf
open Format

module Seq = Sequence

module Make(Node : Opaque)(Label : T) = struct

  type edge_label = Label.t

  type edge = {
    src : Node.t;
    dst : Node.t;
    data : edge_label;
  } with fields

  let compare_edge x y = match Node.compare x.src y.src with
    | 0 -> Node.compare x.dst y.dst
    | n -> n

  type arrows = edge_label Node.Map.t

  let compare_arrows x y =
    Map.compare_direct (fun _ _ -> 0) x y

  type node_info = {
    inc : arrows;
    out : arrows;
  } with compare, fields

  type graph = node_info Node.Map.t
  with compare

  let empty_node = {inc = Node.Map.empty; out = Node.Map.empty}

  module Node = struct
    type nonrec edge = edge
    type label = Node.t
    type nonrec graph = graph

    let create = ident
    let label = ident
    let mem n g = Map.mem g n
    let adj dir n g  = Map.find g n |> function
      | None -> Seq.empty
      | Some ns -> Map.to_sequence (dir ns) |> Seq.map ~f:fst

    let succs n = adj out n
    let preds n = adj inc n

    let edges dir reorder n g = Map.find g n |> function
      | None -> Seq.empty
      | Some ns -> Map.to_sequence (dir ns) |>
                   Seq.map ~f:(fun (m,data) ->
                       let src,dst = reorder n m in
                       {src;dst;data})

    let inputs  n = edges inc (fun dst src -> src,dst) n
    let outputs n = edges out (fun src dst -> src,dst) n
    let insert n g = Map.change g n (function
        | None -> Some empty_node
        | other -> other)

    let update n l g : graph = Map.find g n |> function
      | None -> g
      | Some data -> Map.add g ~key:(create l) ~data

    let remove n g = Map.remove g n

    let edge src (dst : Node.t) g = match Map.find g src with
      | None -> None
      | Some a -> match Map.find a.out dst with
        | None -> None
        | Some data -> Some {src; dst; data}

    let has_edge src dst g = edge src dst g <> None


    let degree select n g = match Map.find g n with
      | None -> 0
      | Some s -> Map.length (select s)

    let degree ?dir n g = match dir with
      | None -> degree inc n g + degree out n g
      | Some `In -> degree inc n g
      | Some `Out -> degree out n g

    include Node
  end

  module Edge = struct
    type t = edge
    type node = Node.t
    type label = edge_label
    type nonrec graph = graph

    let create src dst data = {src; dst; data}
    let label e = e.data
    let src e = e.src
    let dst e = e.dst
    let mem e g = match Map.find g e.src with
      | None -> false
      | Some a -> Map.mem a.out e.dst

    let upsert_arrow src dst field e g =
      Map.change g (src e) (function
          | None ->
            Some (Fieldslib.Field.fset field empty_node
                    (Node.Map.singleton (dst e) e.data))
          | Some ns ->
            let map = Fieldslib.Field.get field ns in
            Some (Fieldslib.Field.fset field ns
                    (Map.add map ~key:(dst e) ~data:e.data)))


    let remove_arrow field arr src g = Map.change g src (function
        | None -> None
        | Some ns ->
          let set = Fieldslib.Field.get field ns in
          Some (Fieldslib.Field.fset field ns
                  (Map.remove set arr)))

    let upsert e g =
      upsert_arrow src dst Fields_of_node_info.out e g |>
      upsert_arrow dst src Fields_of_node_info.inc e

    let insert e g = if mem e g then g else upsert e g
    let update e l g = if mem e g then upsert {e with data=l} g else g

    let remove e g : graph =
      remove_arrow Fields_of_node_info.out e.dst e.src g |>
      remove_arrow Fields_of_node_info.inc e.src e.dst

    include Opaque.Make(struct
        type t = edge with compare
        let hash e = Node.hash e.src lxor Node.hash e.dst
      end)
  end

  type t = graph with compare
  type node = Node.t

  let is_directed = true

  let empty = Node.Map.empty

  let nodes g = Map.keys g |> Seq.of_list

  let edges g = nodes g |>
                Seq.concat_map ~f:(fun src -> Node.outputs src g)

  let number_of_nodes g = Map.length g

  let number_of_edges g =
    Map.fold g ~init:0 ~f:(fun ~key:_ ~data:{out} sum ->
        sum + Map.length out)

  include Opaque.Make(struct
      open Format
      type t = graph with compare
      let hash g =
        nodes g |> Seq.fold ~init:0 ~f:(fun hash n ->
            Node.hash n lxor hash)
    end)

  include Printable(struct
      type nonrec t = t
      let module_name = None
      let pp ppf graph =
        let open Bap_graph_pp in
        let string_of_node =
          by_natural_order symbols Node.compare (nodes graph) in
        Dot.pp_graph
          ~string_of_node
          ~nodes_of_edge:(fun e -> Edge.(src e, dst e))
          ~nodes:(nodes graph)
          ~edges:(edges graph)  ppf

    end)
end

module type Printable_graph = sig
  module G : Graph
  val pp_node : formatter -> G.node -> unit
  val pp_edge : formatter -> G.Edge.label -> unit
  val module_name : string option
end

module Printable_graph(Graph_pp : Printable_graph) = struct
  open Graph_pp
  include G
  include Printable(struct
      type t = G.t

      let pp_label ppf lab = match asprintf "%a" pp_edge lab with
        | "" -> ()
        | l -> fprintf ppf "[label=%S]" l

      let pp_edge ppf e =
        Format.fprintf ppf "\"%a\" -> \"%a\"%a"
          pp_node (Edge.src e) pp_node (Edge.dst e)
          pp_label (Edge.label e)

      let pp ppf g =
        let open Format in
        fprintf ppf "@.@[<v2>digraph {";
        Seq.iter (nodes g) ~f:(fun n -> fprintf ppf "@;\"%a\"" pp_node n);
        Seq.iter (edges g) ~f:(fun e -> fprintf ppf "@;%a" pp_edge e);
        fprintf ppf "@]@.}"

      let module_name = module_name
    end)
end



module Labeled(Node : Opaque)(NL : T)(EL : T) = struct
  module Labeled_node = struct
    type t = (Node.t, NL.t) labeled
    include Opaque.Make(struct
        type nonrec t = t
        let compare x y = Node.compare x.node y.node
        let hash x = Node.hash x.node
      end)
  end
  include Make(Labeled_node)(EL)
end

module type Pp = sig
  type t
  val pp : formatter -> t -> unit
  val module_name : string
end

module type Product = sig
  type t
  include Opaque with type t := t
  include Pp with type t := t
end

module Aux(P : Pp) = struct
  open Bap_graph
  let make_name name = Some (P.module_name^"."^name)

  module Tree = struct
    type t = P.t tree
    include Printable(struct
        type nonrec t = t
        let pp = Tree.pp P.pp
        let module_name = make_name "Tree"
      end)
  end

  module Frontier = struct
    type t = P.t frontier
    include Printable(struct
        type nonrec t = t
        let pp = Frontier.pp P.pp
        let module_name = make_name "Frontier"
      end)
  end

  module Group = struct
    type t = P.t group
    include Printable(struct
        type nonrec t = t
        let pp = Group.pp P.pp
        let module_name = make_name "Group"
      end)
  end

  module Partition = struct
    type t = P.t partition
    include Printable(struct
        type nonrec t = t
        let pp = Partition.pp P.pp
        let module_name = make_name "Partition"
      end)
  end

  module Path = struct
    type t = P.t path
    include Printable(struct
        type nonrec t = t
        let pp = Path.pp P.pp
        let module_name = make_name "Path"
      end)
  end
end

module Make_factory(P : Product) = struct
  open Bap_graph
  type node = P.t

  let make_name name = Some (P.module_name^"."^name)

  include Aux(P)

  module Bool = Printable_graph(struct
      module G = Make(P)(Bool)
      let pp_node = P.pp
      let pp_edge ppf x = fprintf ppf "%c" (if x then 't' else 'f')
      let module_name = make_name "Bool"
    end)


  module Unit = Printable_graph(struct
      module G = Make(P)(Unit)
      let pp_node = P.pp
      let pp_edge _ _ = ()
      let module_name = make_name "Unit"
    end)

  module Value = Printable_graph(struct
      module G = Make(P)(Bap_value)
      let pp_node = P.pp
      let pp_edge = Bap_value.pp
      let module_name = make_name "Value"
    end)

  module Word = Printable_graph(struct
      module G = Make(P)(Bitvector)
      let pp_node = P.pp
      let pp_edge = Bitvector.pp
      let module_name = make_name "Word"
    end)

  module Int = Printable_graph(struct
      module G = Make(P)(Int)
      let pp_node = P.pp
      let pp_edge = Int.pp
      let module_name = make_name "Int"
    end)

  module String = Printable_graph(struct
      module G = Make(P)(String)
      let pp_node = P.pp
      let pp_edge = String.pp
      let module_name = make_name "String"
    end)

  module Var = Printable_graph(struct
      module G = Make(P)(Bap_var)
      let pp_node = P.pp
      let pp_edge = Bap_var.pp
      let module_name = make_name "Var"
    end)

  module RExp = struct
    module G = Make(P)(struct type t = exp include Bap_exp end)
    let pp_node = P.pp
    let pp_edge = Bap_exp.pp
    let module_name = make_name "Exp"
  end
  module Exp = Printable_graph(RExp)

  module RStmt = struct
    module G = Make(P)(struct type t = stmt include Bap_stmt end)
    let pp_node = P.pp
    let pp_edge = Bap_stmt.pp
    let module_name = make_name "Stmt"
  end

  module Stmt = Printable_graph(RStmt)

  module RType = struct
    module G = Make(P)(struct type t = typ include Bap_type end)
    let pp_node = P.pp
    let pp_edge = Bap_type.pp
    let module_name = make_name "Type"
  end

  module Type = Printable_graph(RType)

  module Tid = Printable_graph(struct
      module G = Make(P)(Bap_ir.Tid)
      let pp_node = P.pp
      let pp_edge = Bap_ir.Tid.pp
      let module_name = make_name "Tid"
    end)
end

module Named(T : sig
    type t
    val name : string
    val pp : Format.formatter -> t -> unit
    include Opaque with type t := t
  end) = struct
  let module_name = "Bap.Std.Graphlib."^T.name
  include T
end

module PInt = Named(struct include Int let name = "Int" end)
module PValue = Named(struct include Bap_value let name = "Value" end)
module PString = Named(struct include String let name = "String" end)
module PWord = Named(struct include Bitvector let name = "Word" end)
module PVar = Named(struct include Bap_var let name = "Var" end)
module PExp = Named(struct type t = exp include Bap_exp let name = "Exp" end)
module PStmt = Named(struct type t = stmt include Bap_stmt let name = "Stmt" end)
module PTid = Named(struct include Bap_ir.Tid let name = "Tid" end)
module PType = Named(struct type t = typ include Bap_type let name = "Type" end)

module Int = Make_factory(PInt)
module Value = Make_factory(PValue)
module Word = Make_factory(PWord)
module String = Make_factory(PString)
module Var = Make_factory(PVar)
module Exp = Make_factory(PExp)
module Stmt = Make_factory(PStmt)
module Tid  = Make_factory(PTid)
