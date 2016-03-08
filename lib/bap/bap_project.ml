open Core_kernel.Std
open Regular.Std
open Bap_plugins.Std
open Bap_bundle.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std
open Or_error
open Format

type state = {
  tids : Tid.Tid_generator.t;
  name : Tid.Name_resolver.t;
  vars : Var.Id.t;
}

type t = {
  arch    : arch;
  disasm  : disasm;
  memory  : value memmap;
  storage : dict;
  program : program term;
  symbols : Symtab.t;
  state : state;
} [@@deriving fields]

type project = t


type bound = [`min | `max] [@@deriving sexp]
type spec = [`name | bound] [@@deriving sexp]

type subst = [
  | `section of spec
  | `symbol of spec
  | `memory of bound
  | `block of bound
  | `asm
  | `bil
] [@@deriving sexp]


let roots rooter = match rooter with
  | None -> []
  | Some r -> Rooter.roots r |> Seq.to_list

let fresh_state () = {
  tids = Tid.Tid_generator.fresh ();
  name = Tid.Name_resolver.fresh ();
  vars = Var.Id.fresh ();
}

let from_mem
    ?disassembler:backend
    ?brancher
    ?(symbolizer=Symbolizer.empty)
    ?rooter
    ?reconstructor
    arch mem  =
  let state = fresh_state () in
  Disasm.of_mem ?backend ?brancher ?rooter arch mem >>= fun disasm ->
  let cfg = Disasm.cfg disasm in
  let symbols = match reconstructor with
    | Some r -> Reconstructor.run r cfg
    | None ->
      let name = Symbolizer.resolve symbolizer in
      Reconstructor.(run (default name (roots rooter)) cfg) in
  let program = Program.lift symbols in
  let memory =
    Memmap.add Memmap.empty mem (Value.create Image.section "bap.user") in
  let storage = Dict.empty in
  Ok ({arch; disasm; memory; storage; program; symbols; state})

let null arch : addr =
  Addr.of_int 0 ~width:(Arch.addr_size arch |> Size.in_bits)

let from_bigstring
    ?base ?disassembler ?brancher
    ?symbolizer ?rooter ?reconstructor arch big : t Or_error.t =
  let base = Option.value base ~default:(null arch) in
  Memory.create (Arch.endian arch) base big >>=
  from_mem ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor arch

let from_string
    ?base ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor arch s : t Or_error.t =
  from_bigstring ?disassembler ?base ?brancher ?symbolizer ?rooter ?reconstructor
    arch (Bigstring.of_string s)

let from_image ?disassembler:backend ?brancher ?symbolizer ?rooter ?
    reconstructor img =
  let state = fresh_state () in
  let symbolizer = Option.value symbolizer
      ~default:(Symbolizer.of_image img) in
  let rooter = Option.value rooter ~default:(Rooter.of_image img) in
  Disasm.of_image ?backend ?brancher ~rooter img >>= fun disasm ->
  let cfg = Disasm.cfg disasm in
  let symbols = match reconstructor with
    | Some r -> Reconstructor.run r cfg
    | None ->
      let name = Symbolizer.resolve symbolizer in
      let roots = Rooter.roots rooter |> Seq.to_list in
      Reconstructor.(run (default name roots) cfg) in
  let program = Program.lift symbols in
  let memory = Image.memory img in
  let storage = Option.value_map (Image.filename img)
      ~default:Dict.empty
      ~f:(fun name -> Dict.set Dict.empty filename name) in
  let arch = Image.arch img in
  return {arch; disasm; memory; storage; program; symbols; state}

let ok _ = Ok ()

let from_file ?(on_warning=ok)
    ?loader ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor filename =
  Image.create ?backend:loader filename >>= fun (img,warns) ->
  List.map warns ~f:on_warning |> Or_error.combine_errors_unit
  >>= fun () -> from_image ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor img

let restore_state t =
  Tid.Tid_generator.store t.state.tids;
  Tid.Name_resolver.store t.state.name

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
    Symtab.owners project.symbols (Memory.min_addr mem) |>
    List.hd |>
    Option.map ~f:(fun (name,entry,_) ->
        Block.memory entry, name) in
  let find_block mem =
    Symtab.dominators project.symbols mem |>
    List.find_map ~f:(fun (_,_,cfg) ->
        Seq.find_map (Cfg.nodes cfg) ~f:(fun block ->
            if Addr.(Block.addr block = Memory.min_addr mem)
            then Some (Block.memory block, block)
            else None)) in
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
    match Disasm.of_mem project.arch mem with
    | Error er -> "<failed to disassemble memory region>"
    | Ok dis ->
      Disasm.insns dis |>
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
  main : (t -> t) sexp_opaque;
  deps : string sexp_list;
  auto : sexp_bool;
} [@@deriving sexp_of]

let passes : pass DList.t = DList.create ()
let errors : Error.t String.Table.t = String.Table.create ()

let forget : pass DList.Elt.t -> unit = fun _ -> ()


let name_of_bundle () =
  let module Self = Bap_self.Create() in
  Self.name

let register_pass ?(autorun=false) ?(deps=[]) ?name main : unit =
  let pref = name_of_bundle () in
  let name = match name with
    | None -> pref
    | Some name -> pref ^ "-" ^ name in
  DList.insert_last passes {name; main; deps; auto = autorun} |> forget

let register_pass' ?autorun ?deps ?name v : unit =
  register_pass ?autorun ?deps ?name (fun p -> v p; p)

module Pass = struct
  type t = pass [@@deriving sexp_of]
  type error =
    | Unsat_dep of pass * string
    | Runtime_error of pass * exn
    [@@deriving variants, sexp_of]

  let find name : pass option =
    DList.find passes ~f:(fun p -> p.name = name)

  exception Failed of error [@@deriving sexp]
  let fail error = raise (Failed error)

  let rec exec proj pass =
    let deps = List.map pass.deps ~f:(fun name -> match find name with
        | None -> fail @@ unsat_dep pass name
        | Some dep -> dep) in
    let proj = List.fold deps ~init:proj ~f:exec in
    try pass.main proj with
      exn -> fail @@ runtime_error pass exn

  let run_exn pass proj = exec proj pass

  let run pass proj : (project,error) Result.t =
    try Ok (exec proj pass) with
    | Failed error -> Error error

  let name p = p.name
  let autorun p  = p.auto
end

let passes () = DList.to_list passes
let find_pass = Pass.find

module Creator = struct
  type nonrec t =
    ?disassembler:string ->
    ?brancher:brancher ->
    ?symbolizer:symbolizer ->
    ?rooter:rooter ->
    ?reconstructor:reconstructor -> unit -> t Or_error.t
end
module Factory = struct
  include Source.Factory(Creator)
  include Creator
end


include Data.Make(struct
    type nonrec t = t
    let version = "0.1"
  end)


let () =
  let make f src =
    Some (fun ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor () ->
        f ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor src) in
  let from_memory ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor (mem,arch) =
    from_mem ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor arch mem in
  let from_binary ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor img =
    from_image ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor img in
  let from_file ?disassembler ?brancher ?symbolizer ?rooter ?reconstructor file =
    let fmt,ver = match Bap_fileutils.parse_name file with
      | None -> None,None
      | Some (fmt,ver) -> Some fmt, ver in
    let load = Io.load ?fmt ?ver in
    try_with (fun () -> In_channel.with_file file ~binary:true ~f:load) in
  Factory.register Source.Binary "builtin" (make from_binary);
  Factory.register Source.Memory "builtin" (make from_memory);
  Factory.register Source.File   "builtin" (make from_file)
