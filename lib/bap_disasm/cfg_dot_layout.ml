open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Graphlib.Std

type colored_superset = Superset_risg.t * Addr.Hash_set.t String.Map.t
         * Superset.elem Addr.Map.t

module Make(T : sig val instance : colored_superset end) = struct
  open T
  module Dottable = struct
    type t = colored_superset

    module V = struct
      type t = Superset_risg.G.V.t
    end

    module E = struct
      type t = Superset_risg.G.E.t
      let src (s,_) = s
      let dst (_,d) = d
    end

    let iter_vertex f (g, _, _) =
      Superset_risg.G.iter_vertex f g

    let iter_edges_e f (g, _, _) =
      Superset_risg.G.iter_edges_e f g

    let graph_attributes _ = [
        `Fontsize 14;
      ]
    let default_vertex_attributes gr = [
        `Shape `Box; 
        (*`Height 1.0*.Memory.(length mem);*)
        `Fontsize 14;
        `Fontcolor 0x666699;
        `Fontname "Monospace";
        `Width 1.0
      ]

    let red = 0xff0000
    let green = 0x009900
    let yellow = 0xffff00
    let blue = 0x0000ff
    let orange = 0xff6600
    let purple = 0x660066
    let brown = 0x663300
    let cyan = 0x0099cc

    let vertex_name name =
      let fmt = Format.str_formatter in
      Addr.(pp_generic ~prefix:`none ~suffix:`none ~format:`dec
              fmt name);
      Format.flush_str_formatter ()

    let vertex_attributes v =
      let default_attrs =
        [
          `Label ((vertex_name v));
        ] in
      let g, colors, insn_map = instance in
      let contains name =
        match Map.find colors name with
        | Some(s) ->
           Hash_set.mem s v
        | None -> false in
      let find_update default_attrs name color =
        if contains name then
          `Color color :: default_attrs
        else default_attrs in
      let default_attrs =
        find_update default_attrs "False Negatives" red in
      let default_attrs =
        find_update default_attrs "True Positives" green in
      let default_attrs =
        find_update default_attrs "False Positives" yellow in
      let default_attrs =
        match List.hd default_attrs with
        | Some (`Color _) -> 
           default_attrs
        | _ -> `Color 0X660000 :: default_attrs  in
      match Map.find insn_map v with
      | Some(mem,insn) ->
         let len = float_of_int Memory.(length mem) in
         `Height (1.0 *. len) ::
           default_attrs
      | None -> default_attrs


    let get_subgraph _ = None
    let default_edge_attributes _ = [
        `Penwidth 1.0;
        `Arrowsize 0.5;
        `Headport `N;
        `Tailport `S;
        `Labelfloat true;
      ]

    let edge_attributes (src,dst) =
      (*let color,weight = match kind,arity with
        | `Fall,`Many -> 0x660000, 4
        | `Fall,`Mono -> 0x000066, 8
        | `Cond,_ -> 0x006600, 2
        | `Jump,_ -> 0x000066, 2 in*)
      [
        (*`Color color;*)
        (*`Weight weight;*)
      ]
  end
  module Dot = Graph.Graphviz.Dot(Dottable)
             
  include Dot
end
