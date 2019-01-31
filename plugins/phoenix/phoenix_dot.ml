open Core_kernel
open Bap.Std
open Graphlib.Std
open Phoenix_options

module Cfg = Graphs.Cfg
module Make(Env : sig
    val project : project
    val options : Phoenix_options.t
    module Target : Target
  end) = struct
  module Printing = Phoenix_printing.Make(Env)
  module Helpers = Phoenix_helpers.Make(Env)
  open Printing
  open Helpers
  open Env

  let symbols = Project.symbols project
  let cfg = Project.disasm project |> Disasm.cfg

  (** Creates a graph bounded by a memory region *)
  module Dottable = struct
    type t = Symtab.fn

    type arity = [`Mono | `Many]
    type dest_kind = [`Jump | `Cond | `Fall]

    module V = struct
      type t = block * string
    end

    module E = struct
      type t = V.t * dest_kind * V.t * arity
      let src (src,_,_,_) = src
      let dst (_,_,dst,_) = dst
    end

    let block_name blk =
      Format.asprintf "%a" pp_blk_name blk

    let vertex t blk = blk, block_name blk

    let iter_vertex f fn =
      Seq.iter (Cfg.nodes cfg) ~f:(fun blk -> f (vertex fn blk))

    let iter_edges_e f mem =
      iter_vertex (fun (src,_) ->
          let dests = Cfg.Node.outputs src cfg in
          let arity =
            if Seq.length_is_bounded_by ~min:1 ~max:1 dests
            then `Mono else `Many in
          Seq.iter dests ~f:(fun e ->
              let dst = Cfg.Edge.dst e in
              let kind = Cfg.Edge.label e in
              f (vertex mem src,kind, vertex mem dst,arity))) mem

    let graph_attributes _ = [
      `Fontsize 14;
    ]
    let default_vertex_attributes gr = [
      `Shape `Box;
      `Fontsize 14;
      `Fontcolor 0x666699;
      `Fontname "Monospace";
      `Width 1.0
    ]
    let vertex_name (_,name) = sprintf "%S" name

    let id_only_vertex (blk,name) = [`Label name]


    let label_order x y = match x,y with
      | `with_name,`with_name -> 0
      | `with_name, _ -> -1
      | _ -> Polymorphic_compare.compare x y


    let escape_label str =
      String.to_list_rev str |>
      List.rev_map ~f:(function
          | '\n'|'\r' -> "\\l"
          | ',' | ';' -> " "
          | '\t' -> "<tabulation>"
          | c -> String.of_char c) |>
      String.concat

    let label_of_vertex (blk,name) =
      let open Format in
      let pp_bil = pp_list Stmt.pp in
      let pp_blk_bil = pp_blk bil_of_block pp_bil in
      let pp_blk_asm = pp_blk Block.insns pp_insns in
      let pr = asprintf in
      List.sort ~compare:label_order options.cfg_format |>
      List.map ~f:(function
          | `with_name -> pr "«%s»\\n" name
          | `with_asm  -> pr "@[<v>%a@]@?" pp_blk_asm blk |> escape_label
          | `with_bil  -> pr "@[<v>%a@]@?" pp_blk_bil blk |> escape_label) |>
      String.concat


    let vertex_attributes v =
      [`Label (label_of_vertex v)]

    let get_subgraph _ = None
    let default_edge_attributes _ = [
      `Penwidth 1.0
    ]

    let edge_attributes (_,kind,_,arity) =
      let color,weight = match kind,arity with
        | `Fall,`Many -> 0x660000, 4
        | `Fall,`Mono -> 0x000066, 8
        | `Cond,_ -> 0x006600, 2
        | `Jump,_ -> 0x000066, 2 in [
        `Color color;
        `Arrowsize 0.5;
        `Headport `N;
        `Tailport `S;
        `Labelfloat true;
        `Weight weight;
      ]
  end
  module Dot = Graph.Graphviz.Dot(Dottable)
  include Dot
end
