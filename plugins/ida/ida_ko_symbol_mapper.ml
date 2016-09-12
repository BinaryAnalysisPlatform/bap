open Core_kernel.Std
open Bap.Std
include Self ()
open Graphlib.Std

let map_pc_to_dest w relocs =
  let open Option in
  List.find_map relocs ~f:(fun (pc,dest) -> some_if (w = pc) (dest))
  >>= fun dest -> return (Bil.int dest)

class bil_ko_symbol_relocator relocs = object(self)
  inherit Stmt.mapper as super

  (** If there's a BIL jmp with an address to one of our assembly addresses, in
      relocs, it currently jumps to itself. We need to update it to jump to the
      extern functions in the lookup table provided by IDA brancher. We
      substitute it with [w], our address in the table *)
  method map_jmp e =
    let open Option in
    match e with
    | Bil.Int w ->
      map_pc_to_dest w relocs
      |> Option.value ~default:(Bil.int w)
      |> super#map_jmp
    | _ -> super#map_jmp e
end

module Cfg = struct
  include Graphs.Cfg

  (** No edge update method? Rolling my own... *)
  let update_edge cfg e e' = cfg |> Edge.remove e |> Edge.insert e'

  (** use the same label (edge which is `Jump, `Cond, etc) *)
  let update_dst b cfg e =
    let src = Edge.src e in
    let lbl = Edge.label e in
    let e' = Edge.create src b lbl in
    update_edge cfg e e'

  let update_src b cfg e =
    let dst = Edge.dst e in
    let lbl = Edge.label e in
    let e' = Edge.create b dst lbl in
    update_edge cfg e e'
end

let full_insn_of_mem mem arch =
  Disasm_expert.Basic.with_disasm ~backend:"llvm" arch
    ~f:(fun dis ->
        let open Disasm_expert.Basic in
        dis |> store_kinds |> store_asm |> fun dis' ->
        insn_of_mem dis' mem)

let remap_block b memory arch relocs =
  let bil_remapper = new bil_ko_symbol_relocator relocs in
  let arch = Arch.to_string arch in
  (* We need a full_insn type to reconstruct an Insn with an updated
      bil. So re-disassemble from mem to get the full_insn type *)
  Block.insns b |> List.map ~f:(fun (mem,insn) ->
      let bil = Insn.bil insn in
      let bil' = bil_remapper#run bil in
      match full_insn_of_mem mem arch with
      | Ok (_,Some x,_) ->
        (mem,Insn.of_basic ~bil:bil' x)
      | _ -> (mem,insn))
  |> Block.create memory

(** Destructs a disassembly block to instructions, transforms the bil with
    relocation information in remap_block, and reconstructs / updates edges. *)
class remap_cfg entry relocs arch = object(self)
  inherit [block, Graphs.Cfg.edge, block * cfg] Graphlib.dfs_identity_visitor
  method! enter_node _ b (entry,cfg) =
    let memory = Block.memory b in
    let b' = remap_block b memory arch relocs in
    let ins = Graphs.Cfg.Node.inputs b cfg in
    let outs = Graphs.Cfg.Node.outputs b cfg in
    cfg |> Graphs.Cfg.Node.remove b |> Graphs.Cfg.Node.insert b' |> fun cfg' ->
    Seq.fold ~init:cfg' ins ~f:(Cfg.update_dst b') |> fun cfg' ->
    Seq.fold ~init:cfg' outs ~f:(Cfg.update_src b') |> fun res ->
    if Block.addr b' = Block.addr entry then (b',res)
    else (entry,res)
end

let remap entry cfg relocs arch =
  let cfg' = cfg in
  let visitor = new remap_cfg entry relocs arch in
  Graphlib.depth_first_visit (module Graphs.Cfg) cfg ~init:(entry,cfg')
    visitor

(** Brancher introduces synthetic sections (IDA's extern section). The recursive
    descent disassembler visits this extern section, and under normal operation
    disassembles bogus hex values. The job of this function is to clean up those
    extern functions (stub them out) so that they don't contain bogus
    disassembled instructions. Without this fix-up, the interpreter will get
    stuck on these instructions which do not return or preserve control flow. *)
let clean_extern proj entry cfg arch =

  let make_stub mem' arch =
    match full_insn_of_mem mem' arch with
    | Ok (_,Some x,_) ->
      let bil = [Bil.(Jmp (Unknown ("kernel <3", Imm 0)))] in
      let nop_insn = Insn.of_basic ~bil x in
      let nop_block = Block.create mem' [mem',nop_insn] in
      let nop_cfg = Graphs.Cfg.Node.insert nop_block Graphs.Cfg.empty in
      (nop_block,nop_cfg)
    | _ ->
      warning "Failed to disassemble mem for extern function in Ida_ko_mapper.";
      entry,cfg
  in

  let memmap = Project.memory proj in
  Block.memory entry |>
  Memmap.dominators memmap |>
  Seq.find_map ~f:(fun (mem,tag) ->
      match Value.get Image.section tag with
      | Some "extern" -> Some mem
      | _ -> None) |> function
  | Some mem ->
    (* We need any instruction in a piece of memory which lifts successfully *)
    let s = Bigstring.of_string "\x00\x00\x00\x00" in
    let mem' = match Memory.create LittleEndian (Memory.min_addr mem) s with
      | Ok x -> x
      (* Note sure when this would fail, but if it does, fail hard. *)
      | Error e ->
        let msg = sprintf "Could not create memory in Ida_ko_mapper: %s"
          @@ Error.to_string_hum e in
        failwith msg in
    make_stub mem' (Arch.to_string arch)
  | None -> entry,cfg

let main proj relocs =
  let open Option in
  let open Ida_info in
  let symtab = Project.symbols proj in
  (* build a new symtab with remapped ko symbols *)
  let symtab' =
    Symtab.to_sequence symtab |>
    Seq.fold ~init:(Symtab.empty) ~f:(fun acc (fn_name,fn_start_block,fn_cfg) ->
        (* first, perform relocation *)
        let fn_start',cfg' =
          remap fn_start_block fn_cfg relocs (Project.arch proj) in
        (* now make the function clean if it is in .extern *)
        let _,cfg' = clean_extern proj fn_start' cfg' (Project.arch proj) in
        Symtab.add_symbol acc (fn_name,fn_start',cfg')) in
  Program.lift symtab'
  |> Project.with_program proj
