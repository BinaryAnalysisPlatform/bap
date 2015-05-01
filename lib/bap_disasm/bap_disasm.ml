open Core_kernel.Std
open Bap_types.Std

open Or_error

open Bap_disasm_types
open Image_internal_std

module Rec = Bap_disasm_rec
module Dis = Bap_disasm_basic
module Block = Bap_disasm_block
module Insn = Bap_disasm_insn
module Image = Bap_image

type block = Block.t with compare, sexp_of
type insn = Insn.t with bin_io, compare, sexp
type op = Op.t with bin_io, compare, sexp
type image = Image.t

type error = [
  | `Failed of Error.t
  | `Errors of error * error
  | Rec.error
] with sexp_of

type mem_state =
  | Failed of error                (** failed to decode anything    *)
  | Decoded of insn * error option (** decoded with optional errors *)
with sexp_of

type disasm = {
  blocks : block table;
  memmap : mem_state table;
  insns : insn table Lazy.t;
  mems_of_insn : (insn -> mem seq) Lazy.t;
}

let insns_of_blocks bs =
  Seq.(Table.elements bs >>| Block.insns >>| of_list |> join)

let insns_table blocks =
  insns_of_blocks blocks |> Seq.fold ~init:Table.empty
    ~f:(fun tab (mem,insn) -> Table.add tab mem insn |> ok_exn)

let reverse_insns_table tab =
  Table.(rev_map ~one_to:many Insn.hashable tab) |> ok_exn

let create_insn_mapping blocks =
  let insns = lazy (insns_table blocks) in
  let mems_of_insn = Lazy.(insns >>| reverse_insns_table) in
  insns, mems_of_insn


let empty = {
  blocks = Table.empty;
  memmap = Table.empty;
  insns = lazy Table.empty;
  mems_of_insn = (lazy (fun _ -> Seq.empty));
}

let fail error mem = {
  empty with
  memmap = Table.singleton mem (Failed error);
}

let mem_of_rec_error = function
  | `Failed_to_disasm mem -> mem
  | `Failed_to_lift (mem,insn,err) -> mem

let errors e1 e2 = `Errors (e1,e2)

let add_error map mem err : mem_state table =
  Table.change map mem ~f:(fun ss ->
      if Seq.is_empty ss
      then `rebind (mem, Failed err)
      else `update (fun (_,s) -> match s with
          | Failed e  -> Failed (errors err e)
          | Decoded (insn,None) -> Decoded (insn, Some err)
          | Decoded (insn,Some e) ->
            Decoded (insn, Some (errors err e))))

let add_rec_error memmap (err : Rec.error) : mem_state table =
  let mem = mem_of_rec_error err in
  add_error memmap mem (err :> error)

let add_decoded map mem insn =
  Table.change map mem ~f:(fun maps ->
      if Seq.is_empty maps
      then `rebind (mem, Decoded (insn,None))
      else `update (fun (mem',s) -> match s with
          | Failed err -> Decoded (insn,Some err)
          | Decoded (insn',_) -> assert false))

let of_rec r =
  let memmap =
    Table.foldi (Rec.blocks r) ~init:Table.empty
      ~f:(fun mem blk map ->
          List.fold (Rec.Block.insns blk) ~init:map
            ~f:(fun map dec -> match dec with
                | mem, (None,_) ->
                  add_rec_error map (`Failed_to_disasm mem)
                | mem, (Some insn, bil) ->
                  add_decoded map mem (Insn.of_basic ?bil insn))) in
  let memmap =
    List.fold (Rec.errors r) ~init:memmap ~f:add_rec_error in
  let blocks = Rec.blocks r |> Table.map ~f:Block.of_rec_block in
  let insns, mems_of_insn = create_insn_mapping blocks in
  {blocks; memmap; insns; mems_of_insn}

let lifter_of_arch = function
  | #Arch.arm -> Some Bap_disasm_arm_lifter.lift
  | `x86    -> Some (Bap_disasm_x86_lifter.IA32.lift)
  | `x86_64 -> Some (Bap_disasm_x86_lifter.AMD64.lift)
  | _ -> None

let linear_sweep arch mem : (mem * insn option) list Or_error.t =
  Dis.create ~backend:"llvm" (Arch.to_string arch) >>| fun dis ->
  let dis = Dis.store_asm dis in
  let dis = Dis.store_kinds dis in
  Dis.run dis mem
    ~init:[] ~return:ident ~stopped:(fun s _ ->
        Dis.stop s (Dis.insns s)) |>
  List.map ~f:(function
      | mem, None -> mem,None
      | mem, Some insn -> match lifter_of_arch arch with
        | None -> mem, Some (Insn.of_basic insn)
        | Some lift -> match lift mem insn with
          | Ok bil -> mem, Some (Insn.of_basic ~bil insn)
          | _ -> mem, Some (Insn.of_basic insn))

let linear_sweep_exn arch mem = ok_exn (linear_sweep arch mem)


let disassemble ?roots arch mem =
  let lifter = lifter_of_arch arch in
  match Rec.run ?lifter ?roots arch mem with
  | Error err -> fail (`Failed err) mem
  | Ok r -> of_rec r

(* merges the results of disassembling of different sections,
   table entries mustn't overlap. *)
let merge_different_sections d1 d2 =
  let add mem x tab = Table.add tab mem x |> ok_exn in
  let merge t1 t2 = Table.foldi t1 ~init:t2 ~f:add in
  let memmap = merge d1.memmap d2.memmap in
  let blocks = merge d1.blocks d2.blocks in
  let insns, mems_of_insn = create_insn_mapping blocks in
  {memmap; blocks; insns; mems_of_insn}

let disassemble_image ?roots image =
  let arch = Image.arch image in
  let roots = match roots with
    | Some roots -> roots
    | None -> Image.symbols image |> Table.regions |>
              Seq.map ~f:Memory.min_addr |> Seq.to_list in
  Table.foldi ~init:empty (Image.sections image) ~f:(fun mem sec dis ->
      if Image.Sec.is_executable sec then
        merge_different_sections dis (disassemble ~roots arch mem)
      else dis)

let disassemble_file ?roots filename =
  Image.create filename >>= fun (img,errs) ->
  let dis = disassemble_image ?roots img in
  let memmap =
    List.fold ~init:dis.memmap errs ~f:(fun memmap e ->
        let e = `Failed e in
        Table.map memmap ~f:(function
            | Failed e' -> Failed (`Errors (e,e'))
            | Decoded (insn,None) -> Decoded (insn,Some e)
            | Decoded (insn,Some e') ->
              Decoded (insn, Some (`Errors(e,e'))))) in
  return {dis with memmap}

let disassemble_file_exn ?roots filename =
  disassemble_file ?roots filename |> ok_exn

module Disasm = struct
  type t = disasm
  let insns t = insns_of_blocks t.blocks
  let blocks t = t.blocks
  let insn_at_mem {memmap} m =
    Option.(Table.find memmap m >>= function
      | Decoded (insn,_) -> Some insn
      | _ -> None)

  let insns_at_mem {memmap} m =
    Table.intersections memmap m |> Seq.filter_map ~f:(function
        | _, Failed _ -> None
        | mem, Decoded (insn,_) -> Some (mem,insn))

  let insn_at_addr d addr = match Table.find_addr d.memmap addr with
    | Some (mem, Decoded (insn,_)) -> Some (mem,insn)
    | _ -> None

  let mems_of_insn {mems_of_insn = lazy f} = f

  let span d =
    let s = Table.regions d.memmap in
    match Seq.next s with
    | None -> Seq.empty
    | Some (x,s) -> Seq.unfold_with s ~init:x ~f:(fun prev next ->
        match Memory.merge prev next with
        | Ok merged -> Seq.Step.Skip merged
        | Error _ -> Seq.Step.Yield (prev,next))


  (* flattened tree of errors, for external use *)
  type error = [
    | `Failed of Error.t
    | `Failed_to_disasm of mem
    | `Failed_to_lift of mem * insn * Error.t
  ] with sexp_of

  let insn  = Tag.register "insn" sexp_of_insn
  let block = Tag.register "block" sexp_of_block
  let error = Tag.register "error" sexp_of_error

  module Error = Printable(struct
      open Format
      type t = error

      let module_name = "Bap.Std.Disasm.Error"

      let pp fmt t : unit =
        match t with
        | `Failed e ->
          fprintf fmt "Failed: %a@\n" Error.pp e
        | `Failed_to_disasm m ->
          fprintf fmt "Failed to disassemble: %a@\n" Memory.pp m
        | `Failed_to_lift (m, i, e) ->
          fprintf fmt "Failed to lift: %a%a%a@\n"
            Memory.pp m Insn.pp i Error.pp e
    end)

  let errors d : (mem * error) seq =
    let open Seq.Generator in
    let error_of_state (mem,s) = match s with
      | Decoded (_, Some err) | Failed err -> Some (mem,err)
      | _ -> None in
    let rec yield_error = function
      | mem, `Failed_to_lift (m,insn,err) ->
        yield (mem, `Failed_to_lift (m, Insn.of_basic insn, err))
      | mem, `Failed_to_disasm m -> yield (mem,`Failed_to_disasm m)
      | mem, `Failed err -> yield (mem, `Failed err)
      | mem, `Errors (e1,e2) ->
        yield_error (mem,e1) >>= fun () ->
        yield_error (mem,e2) in
    let rec traverse s = match Seq.next s with
      | None -> return ()
      | Some (x,s) -> match error_of_state x with
        | None -> traverse s
        | Some err -> yield_error err >>= fun () -> traverse s in
    Table.to_sequence d.memmap |> traverse |> run
end
