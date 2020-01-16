open Bap_core_theory
open Core_kernel
open Bap_types.Std
open Bap_image_std
open Monads.Std

open KB.Syntax

module Source = Bap_disasm_source
module Targets = Bap_disasm_target_factory
module Dis = Bap_disasm_basic
module Insn = Bap_disasm_insn

type edge = Bap_disasm_block.edge [@@deriving sexp, compare]
type dest = addr option * edge [@@deriving sexp]
type dests = dest list [@@deriving sexp]
type full_insn = Bap_disasm_basic.full_insn

type t = Brancher of (mem -> full_insn -> dests)
type brancher = t

module Rel_info = struct

  type t = {
    rels : addr Addr.Map.t;
    exts : Addr.Set.t;
  }

  module Fact = Ogre.Make(Monad.Ident)

  module Request = struct
    open Image.Scheme
    open Fact.Syntax

    let of_aseq s =
      Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
          Map.set m ~key ~data)

    let arch_width =
      Fact.require arch >>= fun a ->
      match Arch.of_string a with
      | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
      | None -> Fact.failf "unknown/unsupported architecture" ()

    let relocations =
      arch_width >>= fun width ->
      Fact.collect Ogre.Query.(select (from relocation)) >>= fun s ->
      Fact.return
        (of_aseq @@ Seq.map s ~f:(fun (addr, data) ->
             Addr.of_int64 ~width addr, Addr.of_int64 ~width data))

    let external_symbols  =
      arch_width >>= fun width ->
      Fact.collect Ogre.Query.(
          select (from external_reference)) >>= fun s ->
      Fact.return
        (Seq.fold s ~init:Addr.Set.empty ~f:(fun addrs (addr, _) ->
             Set.add addrs (Addr.of_int64 ~width addr)))
  end

  let relocations = Fact.eval Request.relocations
  let external_symbols = Fact.eval Request.external_symbols
  let empty = {rels = Addr.Map.empty; exts = Addr.Set.empty}

  let of_spec spec =
    match relocations spec, external_symbols spec with
    | Ok rels, Ok exts -> {rels; exts}
    | Error e, _  | _, Error e -> Error.raise e

  let span mem =
    let start = Memory.min_addr mem in
    let len = Memory.length mem in
    Seq.init len ~f:(Addr.nsucc start)

  let is_external {exts} mem =
    Seq.exists ~f:(Set.mem exts) (span mem)

  let find_internal {rels} mem =
    Seq.find_map ~f:(Map.find rels) (span mem)

end

let fixup rel_info mem target =
  match Rel_info.find_internal rel_info mem with
  | Some _ as a -> a
  | None ->
    if Rel_info.is_external rel_info mem then None
    else Some target

let resolve_jumps rel_info mem dests =
  List.map ~f:(function
      | Some addr, `Jump -> fixup rel_info mem addr, `Jump
      | x -> x) dests

let create f = Brancher f
let resolve (Brancher f) = f

let empty = Brancher (fun _ _ -> [])

let kind_of_dests = function
  | xs when List.for_all xs ~f:(fun (_,x) -> [%compare.equal : edge] x `Fall) -> `Fall
  | xs -> if List.exists  xs ~f:(fun (_,x) -> [%compare.equal : edge] x `Jump)
    then `Jump
    else `Cond

let kind_of_branches t f =
  match kind_of_dests t, kind_of_dests f with
  | `Jump,`Jump -> `Jump
  | `Fall,`Fall -> `Fall
  | _           -> `Cond

let has_jumps =
  Bil.exists
    (object
      inherit [unit] Stmt.finder
      method! enter_jmp _ r = r.return (Some ())
    end)

let rec dests_of_bil bil : dests =
  List.concat_map ~f:dests_of_stmt bil
and dests_of_stmt = function
  | Bil.Jmp (Bil.Int addr) -> [Some addr,`Jump]
  | Bil.Jmp (_) -> [None, `Jump]
  | Bil.If (_,yes,no) -> merge_branches yes no
  | Bil.While (_,ss) -> dests_of_bil ss
  | _ -> []
and merge_branches yes no =
  let x = dests_of_bil yes and y = dests_of_bil no in
  let kind = kind_of_branches x y in
  List.(rev_append x y >>| fun (a,_) -> a,kind)

let dests_of_bil ?(rel_info=Rel_info.empty) arch =
  let module Target = (val Targets.target_of_arch arch) in
  fun mem insn ->
    let next = Addr.succ (Memory.max_addr mem) in
    let dests = match Target.lift mem insn with
      | Error _ -> []
      | Ok bil ->
        if has_jumps bil then
          resolve_jumps rel_info mem (dests_of_bil bil)
        else [] in
    let is = Dis.Insn.is insn in
    let fall = Some next, `Fall in
    match kind_of_dests dests with
    | `Fall when is `Return -> []
    | `Jump when is `Call -> fall :: dests
    | `Cond | `Fall -> fall :: dests
    | _ -> dests

let of_bil arch = create (dests_of_bil arch)

let of_image img =
  let rel_info = Rel_info.of_spec (Image.spec img) in
  create (dests_of_bil ~rel_info (Image.arch img))

module Factory = Source.Factory.Make(struct type nonrec t = t end)

let (>>=?) x f = x >>= function
  | None -> KB.return Insn.empty
  | Some x -> f x

let provide =
  KB.Rule.(declare ~package:"bap.std" "reflect-brancher" |>
           dynamic ["brancher"] |>
           require Memory.slot |>
           require Dis.Insn.slot |>
           provide Insn.Slot.dests |>
           comment "[Brancher.provide b] provides [b] to KB");
  fun brancher ->
    let init = Set.empty (module Theory.Label) in
    KB.promise Theory.Program.Semantics.slot @@ fun label ->
    KB.collect Memory.slot label >>=? fun mem ->
    KB.collect Dis.Insn.slot label >>=? fun insn ->
    resolve brancher mem insn |>
    KB.List.fold ~init ~f:(fun dsts dst ->
        match dst with
        | Some addr,_ ->
          Theory.Label.for_addr (Word.to_bitvec addr) >>| fun dst ->
          Set.add dsts dst
        | None,_ -> KB.return dsts) >>| fun dests ->
    KB.Value.put Insn.Slot.dests Insn.empty (Some dests)
