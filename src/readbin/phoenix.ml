open Core_kernel.Std
open Bap.Std
open Options
open Format


module Make(Env : sig
    val project : project
    val options : Options.t
    module Target : Target
  end) = struct
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
  let symbols = Project.symbols project
  let cfg = Project.disasm project |> Disasm.blocks

  let block_name blk = asprintf "%a" pp_blk_name blk

  let store_cfg fn =
    with_cfg_file root (Symtab.name_of_fn fn) ~f:(fun out ->
        Dot.fprint_graph out fn)

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
    let funcs =
      Symtab.to_sequence symbols |>
      Seq.fold  ~init:[] ~f:(fun acc fn  ->
          let parent = Symtab.name_of_fn fn in
          let bound = unstage (Symtab.create_bound symbols fn) in
          let bound blk = bound (Block.addr blk) in
          let children =
            Seq.(Table.elements cfg |>
                 Seq.filter ~f:bound >>|
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
    let func min max name = Ezjsonm.(dict [
        "name", string name;
        "startAddress", address Memory.min_addr min;
        "endAddress", address Memory.max_addr max;
        "hash", string "0000000000000000000"
      ]) in
    let functions =
      Symtab.to_sequence symbols |>
      Seq.fold ~init:[] ~f:(fun a fn ->
          let entry = Symtab.entry_of_fn fn in
          let entry_mem = Block.memory entry in
          let mem = Symtab.memory_of_fn symbols fn in
          let min_mem = Memmap.min_binding mem |> Option.map ~f:fst in
          let max_mem = Memmap.max_binding mem |> Option.map ~f:fst in
          let min = Option.value ~default:entry_mem min_mem in
          let max = Option.value ~default:entry_mem max_mem in
          func min max (Symtab.name_of_fn fn) :: a) in
    let amount = Seq.length (Symtab.to_sequence symbols) in
    let document = Ezjsonm.(`O [
        "binary", `O [
          "name", string filename;
          "functionNumber", int amount;
          "functions", `A functions;
        ]
      ]) in
    with_funcs_file root ~f:(fun out ->
        Ezjsonm.to_string ~minify:false document |>
        pp_print_string out)

  let store_asm fn =
    let pp_blk_bil = pp_blk bil_of_block pp_bil in
    with_bil_file root (Symtab.name_of_fn fn) ~f:(fun out ->
        pp_code (pp_sym pp_blk_bil) out fn)

  let store_dump () =
    let pp_blk_asm = pp_blk Block.insns pp_insns in
    let pr fmt cfg =
      fprintf fmt "@{<(pre (class %S))>%a@}"
        "prettyprint linenums" (pp_syms pp_blk_asm) symbols in
    with_dump_file root ~f:(fun out -> pp_code pr out cfg)


  let store () =
    Seq.iter (Symtab.to_sequence symbols) ~f:(fun fn ->
        store_cfg fn;
        store_asm fn);
    store_index ();
    store_functions ();
    store_dump ();
    path root
end
