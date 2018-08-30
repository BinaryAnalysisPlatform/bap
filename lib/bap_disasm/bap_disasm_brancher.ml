open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Monads.Std

module Source = Bap_disasm_source
module Targets = Bap_disasm_target_factory
module Dis = Bap_disasm_basic
module Insn = Bap_disasm_insn

type edge = Bap_disasm_block.edge [@@deriving sexp]
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
          Map.add m ~key ~data)

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
    | Ok rels, Error _ -> {rels; exts = Addr.Set.empty}
    | Error _, Ok exts -> {rels = Addr.Map.empty; exts}
    | _ -> empty

  let find mem get =
    let max_addr = Memory.max_addr mem in
    let rec run addr =
      if Addr.(addr > max_addr) then None
      else
        match get addr with
        | None -> run (Addr.succ addr)
        | Some value -> Some value in
    run (Memory.min_addr mem)

  let exists_external t mem =
    Option.is_some @@
    find mem (fun x -> if Set.mem t.exts x then Some x else None)

  let find_internal t mem = find mem (Map.find t.rels)

end

let fixup rel_info mem target =
  match Rel_info.find_internal rel_info mem with
  | Some _ as a -> a
  | None ->
    if Rel_info.exists_external rel_info mem then None
    else Some target

let resolve_jumps rel_info mem dests =
  List.map ~f:(function
      | Some addr, `Jump -> fixup rel_info mem addr, `Jump
      | x -> x) dests

let create f = Brancher f
let resolve (Brancher f) = f

let empty = Brancher (fun _ _ -> [])

let kind_of_dests = function
  | xs when List.for_all xs ~f:(fun (_,x) -> x = `Fall) -> `Fall
  | xs -> if List.exists  xs ~f:(fun (_,x) -> x = `Jump)
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
  Bil.fold_consts bil |> List.concat_map ~f:dests_of_stmt
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
