open Core_kernel.Std
open Bap_plugins.Std
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
  | `section of spec
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
    Memmap.add Memmap.empty mem (Value.create Image.section "bap.user") in
  let storage = Dict.empty in
  Ok ({arch; disasm; memory; storage; program; symbols})

let null arch : addr =
  Addr.of_int 0 ~width:(Arch.addr_size arch |> Size.in_bits)

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
              Option.map ~f:(fun (_,sym) -> Image.Symbol.name sym) in
  let roots = match roots @ image_roots with
    | [] -> [Image.entry_point img]
    | xs -> xs in
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
let with_program = Field.fset Fields.program

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
  | "section" | "section_name" -> Some (`section `name)
  | "section_addr" | "section_min_addr" -> Some (`section `min)
  | "section_max_addr" -> Some (`section `max)
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
  let find_section = find_tag Image.section in
  let find_symbol mem =
    Symtab.fns_of_addr project.symbols (Memory.min_addr mem) |>
    List.hd |>
    Option.map ~f:(fun fn ->
        Block.memory (Symtab.entry_of_fn fn), Symtab.name_of_fn fn) in
  let find_block mem =
    Table.find_addr (Disasm.blocks project.disasm)
      (Memory.min_addr mem) in
  let subst_section (mem,name) = function
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
        | Some (`section spec) ->
          apply_subst find_section mem subst_section spec x
        | Some (`symbol spec) ->
          apply_subst find_symbol mem subst_section spec x
        | Some (`memory bound) -> addr bound mem
        | Some (`block spec) ->
          apply_subst find_block mem subst_block spec x
        | Some (`bil | `asm as out) -> subst_disasm mem out
        | None -> x) x;
    Buffer.contents buf in
  tag_memory project mem tag (sub mem value)

module DList = Doubly_linked

type pass = {
  name : string;
  main : string array -> t -> t;
  deps : string list;
}

type 'a register = ?deps:string list -> string -> 'a -> unit

let passes : pass DList.t = DList.create ()
let errors : Error.t String.Table.t = String.Table.create ()

let forget : pass DList.Elt.t -> unit = fun _ -> ()

let find name : pass option =
  DList.find passes ~f:(fun p -> p.name = name)

let register_pass_with_args ?(deps=[]) name main : unit =
  DList.insert_last passes {name; main; deps} |> forget

let register_pass_with_args' ?deps n v : unit =
  register_pass_with_args ?deps n (fun a p -> v a p; p)

let register_pass ?deps n v : unit =
  register_pass_with_args ?deps n (fun _arg p -> v p)

let register_pass' ?deps n v : unit =
  register_pass ?deps n (fun p -> v p; p)


let prepare_args argv name =
  let prefix = "--" ^ name ^ "-" in
  let is_key = String.is_prefix ~prefix:"-" in
  Array.fold argv ~init:([],`drop) ~f:(fun (args,act) arg ->
      let take arg = ("--" ^ arg) :: args in
      if arg = argv.(0) then (name::args,`drop)
      else match String.chop_prefix arg ~prefix, act with
        | None,`take when is_key arg -> args,`drop
        | None,`take -> arg::args,`drop
        | None,`drop -> args,`drop
        | Some arg,_ when String.mem arg '=' -> take arg,`drop
        | Some arg,_ -> take arg,`take) |>
  fst |> List.rev |> Array.of_list

type error =
  | Not_loaded of string
  | Is_duplicate of string
  | Not_found of string
  | Doesn't_register of string
  | Load_failed of string * Error.t
  | Runtime_error of string * exn
with variants, sexp_of

exception Pass_failed of error with sexp
let plugin_failure error = raise (Pass_failed error)
let fail name error = plugin_failure (error name)

let load ?library name : unit =
  match find name with
  | Some _ -> ()
  | None -> match Plugin.find_plugin ?library name with
    | None -> fail name not_found
    | Some p -> match Plugin.load p with
      | Error err -> plugin_failure (load_failed name err)
      | Ok () ->
        match find name with
        | Some _ -> ()
        | None -> fail name doesn't_register

(* load dependencies until a fixpoint or error condition are reached  *)
let load_deps ?library () : unit =
  let step () =
    DList.to_list passes |>
    List.iter ~f:(fun pass -> List.iter pass.deps ~f:(load ?library)) in
  let rec loop () =
    let m = DList.length passes in
    step ();
    if m <> DList.length passes then loop () in
  loop ()


let rec run ?library ?(argv=Sys.argv) (passed,proj) name =
  if List.mem passed name then (passed,proj)
  else
    let pass = match find name with
      | Some pass -> pass
      | None -> fail name not_loaded in
    let (passed,proj) = List.fold pass.deps ~init:(passed,proj)
        ~f:(run ?library ~argv) in
    let argv = prepare_args argv pass.name in
    let proj = try pass.main argv proj with
        exn -> plugin_failure (runtime_error name exn) in
    name :: passed, proj

let prepare_passes ?library ?argv () =
  let passes = DList.to_list passes |> List.map ~f:(fun p -> p.name) in
  match List.find_a_dup passes with
  | Some name -> fail name is_duplicate
  | None -> load_deps ?library ()

let run_pass_exn ?library ?argv proj pass =
  prepare_passes ?library ?argv ();
  run ?library ?argv ([],proj) pass |> snd

let make_error = function
  | Not_loaded name ->
    errorf "plugin %s wasn't not loaded in the system" name
  | Is_duplicate name ->
    errorf "plugin with name %s was registered more than once" name
  | Not_found name ->
    errorf "can't find library for plugin %s" name
  | Doesn't_register name ->
    errorf "plugin %s didn't register any passes" name
  | Load_failed (name,err) ->
    errorf "the following error has occured when loading %s:%s"
      name (Error.to_string_hum err)
  | Runtime_error (name,exn) ->
    errorf "plugin %s failed in runtime with exception %s"
      name (Exn.to_string exn)

let run_pass ?library ?argv proj name : t Or_error.t =
  try Ok (run_pass_exn ?library ?argv proj name) with
  | Pass_failed error -> make_error error
  | exn -> errorf "unexpected error when running plugins: %s"
             (Exn.to_string exn)

let passes_exn ?library () =
  load_deps ?library ();
  DList.to_list passes |> List.map ~f:(fun p -> p.name)

let passes ?library () =
  try Ok (passes_exn ?library ()) with
  | Pass_failed err -> make_error err
  | exn -> errorf "unexpecting error, when loading dependencies: %s"
             (Exn.to_string exn)
