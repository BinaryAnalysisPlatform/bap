open Core_kernel
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std
open Monads.Std

include Bap_self.Create()

module Fact = Ogre.Make(Monad.Ident)

module Ext = struct
  open Image.Scheme
  open Fact.Syntax

  let addr_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let calls insns =
    let insns =
      Seq.filter insns ~f:(fun (mem, insn) ->
          Bap_disasm_insn.(may affect_control_flow insn)) in
    addr_width >>= fun width ->
    Fact.collect
      Ogre.Query.(select (from external_reference)) >>= fun syms ->
    let tab = Seq.fold ~init:Bap_table.empty ~f:(fun tab (mem,_) ->
        match Bap_table.add tab mem () with
        | Ok tab -> tab
        | Error er ->
          warning "%a" Error.pp er;
          tab) insns in
    Fact.return @@
    Seq.fold syms ~init:Addr.Map.empty ~f:(fun m (addr, data) ->
        let addr = Addr.of_int64 ~width addr in
        match Bap_table.find_addr tab addr with
        | None -> m
        | Some (mem, _) ->
          Map.set m ~key:(Memory.min_addr mem) ~data)

end

class sub_symbolizer names = object
  inherit Term.mapper

  method! map_sub s =
    if Term.(has_attr s synthetic) then
      let name =
        List.find ~f:(fun (tid, name) -> Term.tid s = tid) names in
      match name with
      | None -> s
      | Some (tid,name) ->
        Tid.set_name tid name;
        Ir_sub.with_name s name
    else s
end

class jmp_finder = object
  inherit [jmp term list] Term.visitor
  method! enter_jmp jmp jmps = jmp :: jmps
end

let direct_call_of_jmp jmp = match Ir_jmp.kind jmp with
  | Call call ->
    begin
      match Call.target call with
      | Direct tid -> Some (call, tid)
      | _ -> None
    end
  | _ -> None

let target_tid jmp = match direct_call_of_jmp jmp with
  | None -> None
  | Some (_,tid) -> Some tid

let find_destinations calls jmp =
  let open Option in
  Term.get_attr jmp address >>= fun addr ->
  Map.find calls addr >>= fun name ->
  target_tid jmp >>= fun tid ->
  Some (tid,name)

(** [find_externals_names program insn externals] - returns a list of
    (tid, name) where every [tid] denotes a sythetic subroutine term
    that must be renamed to [name]. *)
let find_externals_names prg insns calls =
  let jumps = List.rev @@ (new jmp_finder)#run prg [] in
  List.filter_map ~f:(find_destinations calls) jumps

let name prg exts = (new sub_symbolizer exts)#run prg

let is_synthetic_sub prg tid =
  match Term.find sub_t prg tid with
  | Some sub -> Term.(has_attr sub synthetic)
  | None -> false

class relinker src_prg names = object
  inherit Term.mapper

  method! map_jmp jmp = match direct_call_of_jmp jmp with
    | None -> jmp
    | Some (call, tid) ->
      if is_synthetic_sub src_prg tid then
        match String.Map.find names (Tid.name tid) with
        | Some unq_tid when not (Tid.equal unq_tid tid) ->
          let call = Call.with_target call (Direct unq_tid) in
          Ir_jmp.with_kind jmp (Call call)
        | _ -> jmp
      else jmp
end

(** [reduce prg] removes duplicated synthetic subroutines,
    that appeared during program lifting and external calls
    resolving. *)
let reduce prg =
  let jumps = List.rev @@ (new jmp_finder)#run prg [] in
  let targets = List.filter_map ~f:target_tid jumps in
  let unique, duplicates =
    List.fold targets ~init:(String.Map.empty, [])
      ~f:(fun (unq, rem) tid ->
          let name = Tid.name tid in
          if String.Map.mem unq name then unq, tid :: rem
          else String.Map.set unq name tid, rem) in
  let prg = (new relinker prg unique)#run prg in
  List.fold duplicates ~init:prg
    ~f:(fun prg tid ->
        if is_synthetic_sub prg tid then
          Term.remove sub_t prg tid
        else prg)


let resolve doc insns prg =
  match Fact.eval (Ext.calls insns) doc with
  | Ok exts ->
    find_externals_names prg insns exts |>
    name prg |>
    reduce
  | Error er ->
    error "%a" Error.pp er;
    prg
