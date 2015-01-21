open Core_kernel.Std
open Bap.Std
open Options
open Format


module Make(Env : Printing.Env) = struct
  module Dot = Dot.Make(Env)
  module Printing = Printing.Make(Env)
  module Helpers = Helpers.Make(Env)
  open Env
  open Printing
  open Phoenix_root
  open Helpers

  let root = Phoenix_root.create
      ?root:options.output_phoenix
      options.filename
  let filename = Filename.basename options.filename

  let block_name blk = asprintf "%a" pp_blk_name blk

  let store_cfg name mem =
    with_cfg_file root name ~f:(fun out ->
        Dot.fprint_graph out mem)

  let store_index () =
    let func ?(children=[]) ?parent is_func id = Ezjsonm.(dict [
        "text", string id;
        "id", string (Option.value_map parent ~default:id
                        ~f:(fun p -> p ^ "*" ^ id));
        "expanded", bool false;
        "function", bool is_func;
        if children = []
        then "leaf", bool true
        else "children", `A children]) in
    let funcs = Table.foldi syms ~init:[] ~f:(fun lim parent acc ->
        let children = Seq.(Table.intersections cfg lim >>| snd >>|
                            block_name >>|
                            func ~parent false |> to_list) in
        func ~children true parent :: acc) in
    let children : Ezjsonm.value  = `O Ezjsonm.([
        "text", string filename;
        "id", string (path root); "expanded", bool true;
        "root", bool true; "children", `A funcs
      ]) in
    let document = `O Ezjsonm.(["text", string ".";
                                "children", `A [children]]) in
    with_index_file root ~f:(fun out ->
        Ezjsonm.to_string ~minify:false document |>
        pp_print_string out)

  let store_functions () =
    let address sel mem =
      Ezjsonm.string (Addr.string_of_value (sel mem)) in
    let digest mem =
      let data = Memory.to_buffer mem in
      Digest.(string (Bigsubstring.to_string data) |> to_hex)  in
    let func m name = Ezjsonm.(dict [
        "name", string name;
        "startAddress", address Memory.min_addr m;
        "endAddress", address Memory.max_addr m;
        "hash", string (digest m)
      ]) in
    let functions =
      Table.foldi syms ~init:[] ~f:(fun m n a -> func m n :: a) in
    let document = Ezjsonm.(`O [
        "binary", `O [
          "name", string filename;
          "functionNumber", int (Table.length syms);
          "functions", `A functions;
        ]
      ]) in
    with_funcs_file root ~f:(fun out ->
        Ezjsonm.to_string ~minify:false document |>
        pp_print_string out)

  let store_asm name mem =
    let pp_blk_bil = pp_blk bil_of_block pp_bil in
    with_bil_file root name ~f:(fun out ->
        pp_code (pp_sym pp_blk_bil) out (mem,name))

  let store_dump () =
    let pp_blk_asm = pp_blk Block.insns pp_insns in
    let pr fmt cfg =
      fprintf fmt "@{<(pre (class %S))>%a@}"
        "prettyprint linenums" (pp_syms pp_blk_asm) syms in
    with_dump_file root ~f:(fun out -> pp_code pr out cfg)


  let store () =
    Table.iteri syms ~f:(fun mem name ->
        store_cfg name mem;
        store_asm name mem);
    store_index ();
    store_functions ();
    store_dump ();
    path root
end
