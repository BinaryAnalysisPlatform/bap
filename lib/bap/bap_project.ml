open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std
open Format

type t = {
  arch    : arch;
  disasm  : disasm;
  memory  : value memmap;
  storage : dict;
  program : program term;
  symbols : string table;
  base    : mem;
}


type bound = [`min | `max] with sexp
type spec = [`name | bound] with sexp

type subst = [
  | `region of spec
  | `symbol of spec
  | `memory of bound
  | `block of bound
  | `asm
  | `bil
] with sexp


let plugins : (string array -> t -> t) list ref = ref []
let register_plugin_with_args p =
  plugins := p :: !plugins
let register_plugin_with_args' v =
  register_plugin_with_args (fun a p -> v a p; p)
let register_plugin v = register_plugin_with_args (fun _arg p -> v p)
let register_plugin' v = register_plugin (fun p -> v p; p)
let plugins () = List.rev !plugins

let subst_of_string = function
  | "region" | "region_name" -> Some (`region `name)
  | "region_addr" | "region_min_addr" -> Some (`region `min)
  | "region_max_addr" -> Some (`region `max)
  | "symbol" | "symbol_name" -> Some (`symbol `name)
  | "symbol_addr" | "symbol_min_addr" -> Some (`symbol `min)
  | "symbol_max_addr" -> Some (`symbol `max)
  | "bil" -> Some (`bil)
  | "asm" -> Some (`asm)
  | "block_name" ->  Some (`block `name)
  | "block_addr" | "block_min_addr" -> Some (`block `min)
  | "block_max_addr" -> Some (`block `max)
  | "min_addr" | "addr" -> Some (`memory `min)
  | "max_addr" -> Some (`memory `max)
  | _ -> None

let addr which mem =
  let take = match which with
    | `min -> Memory.min_addr
    | `max -> Memory.max_addr in
  sprintf "0x%s" @@ Addr.string_of_value (take mem)

let substitute ?(tags=[comment; python; shell]) project =
  let find_tag tag mem =
    Memmap.dominators project.memory mem |>
    Seq.find_map ~f:(fun (mem,v) -> match Value.get tag v with
        | Some reg -> Some (mem,reg)
        | None -> None) in
  let find_region = find_tag Image.region in
  let find_symbol mem =
    Table.find_addr project.symbols (Memory.min_addr mem) in
  let find_block mem =
    Table.find_addr (Disasm.blocks project.disasm)
      (Memory.min_addr mem) in

  let subst_region (mem,name) = function
    | #bound as b -> addr b mem
    | `name -> name in
  let subst_block (mem,block) = function
    | #bound as b -> addr b mem
    | `name -> "blk_"^addr `min mem in
  let asm insn = Insn.asm insn in
  let bil insn = asprintf "%a" Bil.pp (Insn.bil insn) in
  let subst_disasm mem out =
    let inj = match out with `asm -> asm | `bil -> bil in
    disassemble project.arch mem |> Disasm.insns |>
    Seq.map ~f:(fun (_,insn) -> inj insn) |> Seq.to_list |>
    String.concat ~sep:"\n" in

  let apply_subst find mem subst spec value =
    match find mem with
    | Some thing -> subst thing spec
    | None -> value in
  let sub mem x =
    let buf = Buffer.create (String.length x) in
    Buffer.add_substitute buf (fun x -> match subst_of_string x with
        | Some (`region spec) ->
          apply_subst find_region mem subst_region spec x
        | Some (`symbol spec) ->
          apply_subst find_symbol mem subst_region spec x
        | Some (`memory bound) -> addr bound mem
        | Some (`block spec) ->
          apply_subst find_block mem subst_block spec x
        | Some (`bil | `asm as out) -> subst_disasm mem out
        | None -> x) x;
    Buffer.contents buf in
  let memory = Memmap.mapi project.memory ~f:(fun mem value ->
      let tagval =
        List.find_map tags
          ~f:(fun tag -> match Value.get tag value with
              | Some value -> Some (tag,value)
              | None -> None) in
      match tagval with
      | Some (tag,value) -> Value.create tag (sub mem value)
      | None -> value) in
  {project with memory}
