open Core_kernel.Std
open Regular.Std
open Graphlib_intf
open Graphlib_regular_intf
open Format

module Make(Node : Opaque)(Label : T) = struct

  type edge_label = Label.t

  type edge = {
    src : Node.t;
    dst : Node.t;
    data : edge_label;
  } [@@deriving fields]

  let compare_edge x y = match Node.compare x.src y.src with
    | 0 -> Node.compare x.dst y.dst
    | n -> n

  type arrows = edge_label Node.Map.t

  let compare_arrows x y =
    Map.compare_direct (fun _ _ -> 0) x y

  type node_info = {
    inc : arrows;
    out : arrows;
  } [@@deriving compare, fields]

  type graph = node_info Node.Map.t
    [@@deriving compare]

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

    let insert_arrow field info arrs n = match Map.find arrs n with
      | None -> None
      | Some l ->
        let arrs = Map.add arrs ~key:n ~data:l in
        Some (Field.fset field info arrs)

    let remove_arrow field info arrs n =
      let arrs = Map.remove arrs n in
      Some (Field.fset field info arrs)

    let update_arrows update field neibs n g : graph =
      Map.fold neibs ~init:g ~f:(fun ~key:m ~data:_ g ->
          Map.change g m ~f:(function
              | None -> None
              | Some info ->
                update field info (Field.get field info) n))

    let insert_arrows = update_arrows insert_arrow
    let remove_arrows = update_arrows remove_arrow

    let update n l g : graph = Map.find g n |> function
      | None -> g
      | Some {inc;out} ->
        let n = (create l) in
        Map.add g ~key:n ~data:{inc;out}  |>
        insert_arrows Fields_of_node_info.out inc n |>
        insert_arrows Fields_of_node_info.inc out n

    let remove n g = match Map.find g n with
      | None -> g
      | Some {inc;out} ->
        Map.remove g n |>
        remove_arrows Fields_of_node_info.out inc n |>
        remove_arrows Fields_of_node_info.inc out n

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
            Some (Field.fset field empty_node
                    (Node.Map.singleton (dst e) e.data))
          | Some ns ->
            let map = Field.get field ns in
            Some (Field.fset field ns
                    (Map.add map ~key:(dst e) ~data:e.data)))

    let remove_arrow field arr src g = Map.change g src (function
        | None -> None
        | Some ns ->
          let set = Field.get field ns in
          Some (Field.fset field ns (Map.remove set arr)))

    let upsert e g =
      upsert_arrow src dst Fields_of_node_info.out e g |>
      upsert_arrow dst src Fields_of_node_info.inc e

    let insert e g = if mem e g then g else upsert e g
    let update e l g = if mem e g then upsert {e with data=l} g else g

    let remove e g : graph =
      remove_arrow Fields_of_node_info.out e.dst e.src g |>
      remove_arrow Fields_of_node_info.inc e.src e.dst

    include Opaque.Make(struct
        type t = edge [@@deriving compare]
        let hash e = Node.hash e.src lxor Node.hash e.dst
      end)
  end

  type t = graph [@@deriving compare]
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
      type t = graph [@@deriving compare]
      let hash g =
        nodes g |> Seq.fold ~init:0 ~f:(fun hash n ->
            Node.hash n lxor hash)
    end)

  include Printable(struct
      type nonrec t = t
      let module_name = None

      let pp ppf graph =
        let open Graphlib_pp in
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
  open Graphlib_graph
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
  open Graphlib_graph
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

end

module Named(T : sig
    type t [@@deriving bin_io, compare, sexp]
    val name : string
    val hash : t -> int
    val pp : Format.formatter -> t -> unit
  end) = struct
  include T
  let module_name = "Graphlib."^T.name
  include Opaque.Make(T)
end
