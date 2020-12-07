open Core_kernel
open Bap.Std
open Bap_future.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std

open Knowledge.Syntax

open Theory.Parser
include Self()

let package = "bap"

module Optimizer = Theory.Parser.Make(Bil_semantics.Core)
[@@inlined]

let provide_bir () =
  KB.Rule.(declare ~package "reify-ir" |>
           require Theory.Semantics.slot |>
           require Bil_ir.slot |>
           provide Theory.Semantics.slot |>
           comment "reifies IR");
  KB.promise Theory.Semantics.slot @@ fun obj ->
  KB.collect Theory.Semantics.slot obj >>| fun sema ->
  let bir = Bil_ir.reify @@  KB.Value.get Bil_ir.slot sema in
  KB.Value.put Term.slot sema bir


module Relocations = struct

  type t = {
    rels : Addr.t Addr.Map.t;
    exts : string Addr.Map.t;
  } [@@deriving equal]

  module Fact = Ogre.Make(Monad.Ident)

  module Request = struct
    open Image.Scheme
    open Fact.Syntax

    let of_aseq s =
      Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
          Map.set m ~key ~data)

    let arch =
      Fact.collect Ogre.Query.(select (from arch)) >>= fun s ->
      Fact.Seq.reduce ~f:(fun a1 a2 ->
          if Arch.equal a1 a2 then Fact.return a1
          else Fact.failf "arch is ambiguous: %a <> %a"
              Arch.pp a1 Arch.pp a2 ())
        (Seq.filter_map ~f:Arch.of_string s) >>= fun a ->
      match a with
      | Some a -> Fact.return a
      | None -> Fact.return `unknown

    let arch_width =
      arch >>| fun arch -> Arch.addr_size arch |> Size.in_bits

    let relocations =
      arch_width >>= fun width ->
      Fact.collect Ogre.Query.(select (from relocation)) >>= fun s ->
      Fact.return
        (of_aseq @@ Seq.map s ~f:(fun (addr, data) ->
             Addr.of_int64 ~width addr, Addr.of_int64 ~width data))

    let external_symbols  =
      arch_width >>= fun width ->
      Fact.collect Ogre.Query.(select (from external_reference)) >>| fun s ->
      Seq.fold s ~init:Addr.Map.empty ~f:(fun addrs (addr, name) ->
          Map.set addrs
            ~key:(Addr.of_int64 ~width addr)
            ~data:name)
  end

  let relocations = Fact.eval Request.relocations
  let external_symbols = Fact.eval Request.external_symbols
  let empty = {rels = Addr.Map.empty; exts = Addr.Map.empty}


  let of_spec spec =
    match relocations spec, external_symbols spec with
    | Ok rels, Ok exts -> {rels; exts}
    | Error e, _  | _, Error e -> Error.raise e


  let reference_analyzer = object
    inherit [addr Var.Map.t * Addr.Set.t] Stmt.visitor
    method! enter_move var exp (vars,refs) =
      match exp with
      | Bil.Int const -> Map.set vars var const,refs
      | _ -> vars,refs
    method! enter_load ~mem:_ ~addr _ _ (vars,refs) =
      let const = match addr with
        | Bil.Int const -> Some const
        | Bil.Var var -> Map.find vars var
        | _ -> None in
      match const with
      | Some const -> vars, Set.add refs const
      | None -> vars,refs
  end

  let references bil = snd @@
    reference_analyzer#run bil
      (Var.Map.empty, Addr.Set.empty)

  let addresses bil mem =
    let start = Memory.min_addr mem in
    let len = Memory.length mem in
    Seq.append
      (Set.to_sequence (references bil))
      (Seq.init len ~f:(Addr.nsucc start))

  let find_external {exts} bil mem =
    Seq.find_map ~f:(Map.find exts) (addresses bil mem)

  let find_internal {rels} bil mem =
    Seq.find_map ~f:(Map.find rels) (addresses bil mem)

  let override_internal dst =
    Stmt.map (object inherit Stmt.mapper
      method! map_jmp _ = [Bil.Jmp (Int dst)]
    end)

  let override_external is_stub name =
    let name = if is_stub then name^":external" else name in
    Stmt.map (object inherit Stmt.mapper
      method! map_jmp _ = [Bil.(encode call name)]
    end)

  let relocations_slot =
    KB.Class.property Theory.Unit.cls "bil-relocations"
      ~package:"bap" @@
    KB.Domain.flat ~empty ~equal "bil-relocations"

  let fixup obj mem bil =
    KB.collect Theory.Label.unit obj >>= function
    | None -> !!bil
    | Some unit ->
      KB.collect relocations_slot unit >>= fun info ->
      KB.collect (Value.Tag.slot Sub.stub) obj >>|
      Option.is_some >>| fun is_stub ->
      match find_internal info bil mem with
      | Some dst ->
        override_internal dst bil
      | None ->
        match find_external info bil mem with
        | Some name ->
          override_external is_stub name bil
        | None -> bil

  let prepare () =
    KB.promise relocations_slot @@ fun unit ->
    KB.collect Image.Spec.slot unit >>| of_spec
end

module Brancher = struct
  include Theory.Empty

  let pack kind dsts =
    KB.Value.put Insn.Slot.dests (Theory.Effect.empty kind) dsts

  let get x = KB.Value.get Insn.Slot.dests x

  let union k e1 e2 =
    pack k @@ match get e1, get e2 with
    | None,s|s,None -> s
    | Some e1, Some e2 -> Some (Set.union e1 e2)

  let ret kind dst =
    let dsts = Set.singleton (module Theory.Label) dst in
    KB.return @@ pack kind (Some dsts)

  let goto dst = ret Theory.Effect.Sort.jump dst

  let jmp _ =
    KB.Object.create Theory.Program.cls >>= fun dst ->
    ret Theory.Effect.Sort.jump dst

  let seq x y =
    x >>= fun x ->
    y >>= fun y ->
    let k = Theory.Effect.sort x in
    KB.return (union k x y)

  let blk _ data ctrl =
    data >>= fun data ->
    ctrl >>= fun ctrl ->
    let k = Theory.Effect.Sort.join
        [Theory.Effect.sort data]
        [Theory.Effect.sort ctrl] in
    KB.return (union k data ctrl)

  let branch _cnd yes nay =
    yes >>= fun yes ->
    nay >>= fun nay ->
    let k = Theory.Effect.sort yes in
    KB.return (union k yes nay)
end

let base_context = [
  "bil-lifter";
]

let create_intrinsic arch mem insn =
  let module Insn = Disasm_expert.Basic.Insn in
  let width = Size.in_bits (Arch.addr_size arch) in
  let module Target = (val target_of_arch arch) in
  let pre = Insn.ops insn |> Array.to_list |> List.mapi ~f:(fun p op ->
      let name = sprintf "insn:op%d" (p+1) in
      let v = Var.create name (Imm width) in
      Bil.move v @@ match op with
      | Op.Imm x -> Bil.int (Option.value_exn (Imm.to_word ~width x))
      | Op.Fmm x -> Bil.int @@ Word.of_int64 @@
        Int64.bits_of_float (Fmm.to_float x)
      | Op.Reg r -> Bil.int (Word.of_int ~width (Reg.code r))) in
  pre @ Bil.[
      Var.create ("insn:next_address") (Imm width) :=
        int (Addr.succ (Memory.max_addr mem));
      Bil.(encode intrinsic) @@ sprintf "%s:%s"
        (Insn.encoding insn)
        (Insn.name insn)
    ]

type predicate = [
  | `tag of string
  | `asm of string
  | `insn of string * string
]

type ispec = [
  | `any | `unk
  | `special
  | predicate
]

let matches ~pat str =
  String.is_prefix ~prefix:pat @@
  String.(uppercase@@strip str)

let string_of_kind k =
  Format.asprintf "%a" Sexp.pp (sexp_of_kind k)

let matches_spec specs insn =
  let module Insn = Disasm_expert.Basic.Insn in
  List.exists specs ~f:(function
      | `tag t ->
        List.exists (Insn.kinds insn) ~f:(fun k ->
            matches ~pat:t (string_of_kind k))
      | `asm t -> matches ~pat:t (Insn.asm insn)
      | `insn (ns,opcode) ->
        matches ~pat:ns (Insn.encoding insn) &&
        matches ~pat:opcode (Insn.name insn))

let with_unknown = List.exists ~f:(function `unk -> true | _ -> false)


type enable_intrinsic = {
  for_all : bool;
  for_unk : bool;
  for_special : bool;
  predicates : predicate list;
}

let split_specs =
  List.fold
    ~init:{for_all=false; for_unk=false; for_special=false; predicates=[]}
    ~f:(fun spec -> function
        | `any -> {spec with for_all = true}
        | `unk -> {spec with for_unk = true}
        | `special -> {spec with for_special=true}
        | #predicate as p ->
          {spec with predicates = p :: spec.predicates})

let rec is_special = function
  | Bil.Special _ -> true
  | Move _ | CpuExn _ | Jmp _ -> false
  | Bil.While (_, xs) -> has_special xs
  | Bil.If (_, xs, ys) -> has_special xs || has_special ys
and has_special = List.exists ~f:is_special

let lift ~enable_intrinsics:{for_all; for_unk; for_special; predicates}
    arch mem insn =
  if for_all || matches_spec predicates insn
  then Ok (create_intrinsic arch mem insn)
  else
    let module Target = (val target_of_arch arch) in
    match Target.lift mem insn with
    | Error _ as err ->
      if for_unk
      then Ok (create_intrinsic arch mem insn)
      else err
    | Ok bil ->
      if for_special && has_special bil
      then Ok (create_intrinsic arch mem insn)
      else Ok bil

let provide_lifter ~enable_intrinsics ~with_fp () =
  info "providing a lifter for all BIL lifters";
  let unknown = Theory.Semantics.empty in
  let context arch =
    sprintf "arch-%a" Arch.str arch ::
    if with_fp
    then "floating-point" :: base_context
    else base_context in
  let (>>?) x f = x >>= function
    | None -> KB.return unknown
    | Some x -> f x in
  let enable_intrinsics = split_specs enable_intrinsics in
  let lifter obj =
    Knowledge.collect Arch.slot obj >>= fun arch ->
    Theory.instance ~context:(context arch) () >>=
    Theory.require >>= fun (module Core) ->
    Knowledge.collect Memory.slot obj >>? fun mem ->
    Knowledge.collect Disasm_expert.Basic.Insn.slot obj >>? fun insn ->
    match lift ~enable_intrinsics arch mem insn with
    | Error err ->
      info "BIL: the BIL lifter failed with %a" Error.pp err;
      !!Insn.empty
    | Ok bil ->
      let module Lifter = Theory.Parser.Make(Core) in
      Optimizer.run Bil.Theory.parser bil >>= fun sema ->
      let bil = Insn.bil sema in
      Relocations.fixup obj mem bil >>= fun bil ->
      Lifter.run Bil.Theory.parser bil >>| fun sema ->
      let bil = Insn.bil sema in
      KB.Value.merge ~on_conflict:`drop_left
        sema (Insn.of_basic ~bil insn) in
  KB.Rule.(declare ~package "bil-semantics" |>
           require Memory.slot |>
           require Disasm_expert.Basic.Insn.slot |>
           provide Theory.Semantics.slot |>
           comment "denotates BIL in the Core Theory terms");
  Knowledge.promise Theory.Semantics.slot lifter

let init ~enable_intrinsics ~with_fp () =
  provide_lifter ~enable_intrinsics ~with_fp ();
  provide_bir ();
  Relocations.prepare ();
  Theory.declare !!(module Brancher : Theory.Core)
    ~package ~name:"jump-dests"
    ~desc:"an approximation of jump destinations"
    ~provides:[
      "brancher";
      "branch-destinations"
    ]
