open Core_kernel.Std
open Bap_common
open Format

module Seq = Sequence

type scheme = string * (string -> string)

type 'n symbolizer = 'n -> string


(** [next_char c] returns next a lowercase character following the
    [c], if it exists   *)
let next_char char : char option =
  match (Char.to_int char + 1) |> Char.of_int with
  | Some c when Char.(is_lowercase c && is_alpha c) -> Some c
  | _ -> None

(** [next_sym s] generates new fresh symbol, assuming that [s] is the
    highest generated symbol.  The function will at first use all
    lowercase symbols from [[a-z]], and then fall back to a simple
    scheme [node_n], where n goes from zero to max_int.*)
let next_sym sym : string =
  match String.split sym ~on:'_' with
  | [v] when String.length v = 1 ->
    (match next_char v.[0] with
     | Some c -> String.of_char c
     | None -> "node_1")
  | [v;n] -> let m = Int.of_string n + 1 in
    sprintf "%s_%d" v m
  | _ -> assert false

let next_num num : string =
  Int.to_string (Int.of_string num + 1)

let symbols = ("a",next_sym)
let numbers = ("0",next_num)
let nothing = ("",ident)

let create_scheme ~next init = init,next



let by_given_order (type a) (init,next) compare nodes =
  let module T = Comparator.Make(struct
      type t = a
      let compare = compare
      let sexp_of_t = sexp_of_opaque
    end) in
  let comparator = T.comparator in
  Seq.fold nodes ~init:(Map.empty ~comparator,init) ~f:(fun (names,name) node ->
      Map.add names ~key:node ~data:name, next name) |> fst |>
  Map.find_exn

let by_natural_order (type a) variant compare nodes =
  let module T = Comparator.Make(struct
      type t = a
      let compare = compare
      let sexp_of_t = sexp_of_opaque
    end) in
  let comparator = T.comparator in
  Seq.fold nodes ~init:(Set.empty ~comparator) ~f:Set.add |>
  Set.to_sequence |> by_given_order variant compare

module Dot = struct
  let pp_label ppf lab = match lab with
    | "" -> ()
    | lab -> Format.fprintf ppf "[label=\"%s\"]" lab

  let pp_edge ppf (src,dst,lab) =
    fprintf ppf "\"%s\" -> \"%s\"%a" src dst pp_label lab

  let pp_node ppf (name,label) =
    match label with
    | "" -> fprintf ppf "\"%s\"" name
    | _ ->  fprintf ppf "\"%s\"[label=\"%s\"]" name label

  let noname _ = ""

  let pp_graph
      ?(attrs=[])
      ?string_of_node:(node=noname)
      ?(node_label=noname)
      ?(edge_label=noname)
      ~nodes_of_edge ~nodes ~edges ppf =
    let nodes = Seq.map nodes ~f:(fun n -> node n,node_label n) in
    let edges = Seq.map edges ~f:(fun e ->
        let src,dst = nodes_of_edge e in
        node src, node dst, edge_label e) in
    let attrs = if attrs = [] then "" else
        sprintf "@;%s" (String.concat ~sep:"@;" attrs) in
    fprintf ppf "@.@[<v2>digraph {%s" attrs;
    Seq.iter nodes ~f:(fprintf ppf "@;%a" pp_node);
    Seq.iter edges ~f:(fprintf ppf "@;%a" pp_edge);
    fprintf ppf "@]@.}"
end
