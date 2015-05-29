open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std
open Or_error
open Format

type t = {
  arch    : arch;
  disasm  : disasm;
  memory  : value memmap;
  storage : dict;
  program : program term;
  symbols : Symtab.t;
} with fields

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


let from_mem ?name ?roots arch mem : t Or_error.t =
  let roots = Option.value ~default:[Memory.min_addr mem] roots in
  let disasm = disassemble ~roots arch mem in
  let cfg = Disasm.blocks disasm in
  let symbols = Symtab.reconstruct ?name ~roots cfg in
  let program = Program.lift symbols in
  let memory =
    Memmap.add Memmap.empty mem (Value.create Image.region "bap.user") in
  let storage = Dict.empty in
  Ok ({arch; disasm; memory; storage; program; symbols})

let null arch : addr =
  Addr.of_int 0 ~width:(Arch.addr_size arch |> Size.to_bits)

let from_bigstring ?base ?name ?roots arch big : t Or_error.t =
  let base = Option.value base ~default:(null arch) in
  Memory.create (Arch.endian arch) base big >>=
  from_mem ?name ?roots arch

let ok _ = Ok ()

let from_string ?base ?name ?roots arch s : t Or_error.t =
  from_bigstring ?base ?name ?roots arch (Bigstring.of_string s)

let from_image ?(name=fun _ -> None) ?(roots=[]) img =
  let image_roots = Image.symbols img |>
                    Table.regions |> Seq.map ~f:Memory.min_addr |>
                    Seq.to_list in
  let name addr = match name addr with
    | Some name -> Some name
    | None -> Table.find_addr (Image.symbols img) addr |>
              Option.map ~f:(fun (_,sym) -> Image.Sym.name sym) in
  let roots = roots @ image_roots in
  let disasm = disassemble_image ~roots img in
  let cfg = Disasm.blocks disasm in
  let symbols = Symtab.reconstruct ~name ~roots cfg in
  let program = Program.lift symbols in
  let memory = Image.memory img in
  let storage = Option.value_map (Image.filename img)
      ~default:Dict.empty
      ~f:(fun name -> Dict.set Dict.empty filename name) in
  let arch = Image.arch img in
  return {arch; disasm; memory; storage; program; symbols}


let from_file ?(on_warning=ok) ?backend ?name ?roots filename =
  Image.create ?backend filename >>= fun (img,warns) ->
  List.map warns ~f:on_warning |> Or_error.combine_errors_unit
  >>= fun () -> from_image ?name ?roots img


let with_memory = Field.fset Fields.memory
let with_symbols = Field.fset Fields.symbols

let tag_memory t tag mem x =
  with_memory t @@
  Memmap.add t.memory mem (Value.create tag x)

let with_storage = Field.fset Fields.storage

let set t tag x =
  with_storage t @@
  Dict.set t.storage tag x

let get t = Dict.find t.storage
let has t = Dict.mem t.storage

let subst_of_string = function
  | "region" | "region_name" -> Some (`region `name)
  | "region_addr" | "region_min_addr" -> Some (`region `min)
  | "region_max_addr" -> Some (`region `max)
  | "symbol" | "symbol_name" -> Some (`symbol `name)
  | "symbol_addr" | "symbol_min_addr" -> Some (`symbol `min)
  | "symbol_max_addr" -> Some (`symbol `max)
  | "bil" -> Some (`bil)
  | "asm" -> Some (`asm)
  | "block" | "block_name" ->  Some (`block `name)
  | "block_addr" | "block_min_addr" -> Some (`block `min)
  | "block_max_addr" -> Some (`block `max)
  | "min_addr" | "addr" -> Some (`memory `min)
  | "max_addr" -> Some (`memory `max)
  | _ -> None


let addr which mem =
  let take = match which with
    |  `min -> Memory.min_addr
    | `max -> Memory.max_addr in
  sprintf "0x%s" @@ Addr.string_of_value (take mem)


let tag_memory project mem tag x =
  {project with
   memory = Memmap.add project.memory mem (Value.create tag x) }

let substitute project mem tag value : t =
  let find_tag tag mem =
    Memmap.dominators project.memory mem |>
    Seq.find_map ~f:(fun (mem,v) -> match Value.get tag v with
        | Some reg -> Some (mem,reg)
        | None -> None) in
  let find_region = find_tag Image.region in
  let find_symbol mem =
    Symtab.fns_of_addr project.symbols (Memory.min_addr mem) |>
    List.hd |>
    Option.map ~f:(fun fn ->
        Block.memory (Symtab.entry_of_fn fn), Symtab.name_of_fn fn) in
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
  tag_memory project mem tag (sub mem value)

let passes : (string * (string array -> t -> t)) list ref = ref []
let register_pass_with_args n p =
  passes := (n,p) :: !passes
let register_pass_with_args' n v =
  register_pass_with_args n (fun a p -> v a p; p)
let register_pass n v = register_pass_with_args n (fun _arg p -> v p)
let register_pass' n v = register_pass n (fun p -> v p; p)

let prepare_args argv name =
  let prefix = "--" ^ name ^ "-" in
  Array.filter_map argv ~f:(fun arg ->
      if arg = argv.(0) then Some name
      else match String.chop_prefix arg ~prefix with
        | None -> None
        | Some arg -> Some ("--" ^ arg))

let run_it ?(argv=Sys.argv) (name,f) p = f (prepare_args argv name) p

let find_pass name =
  List.find (List.rev !passes) ~f:(fun (pass,f) -> name = pass)

let run_pass ?argv name p =
  let open Option.Monad_infix in
  find_pass name >>| fun pass -> run_it ?argv pass p

let run_passes ?argv p =
  List.fold_right !passes ~init:p ~f:(run_it ?argv)


let has_pass name = find_pass name <> None
