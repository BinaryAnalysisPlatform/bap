open Core_kernel
open Bap_core_theory
open Regular.Std
open Bap_types.Std
open Bap_disasm_types

module Insn = Bap_disasm_basic.Insn
let package = "bap"

type must = Must
type may = May
type 'a property = Z.t * string

let known_properties = ref []

let new_property _ name : 'a property =
  let name = sprintf ":%s" name in
  let bit = List.length !known_properties in
  let property = Z.shift_left Z.one bit, name in
  known_properties := !known_properties @ [property];
  property

let prop = new_property ()
(* must be the first one *)
let invalid             = prop "invalid"

let jump                = prop "jump"
let conditional         = prop "cond"
let indirect            = prop "indirect"
let call                = prop "call"
let return              = prop "return"
let barrier             = prop "barrier"
let affect_control_flow = prop "affect-control-flow"
let load                = prop "load"
let store               = prop "store"

module Props = struct
  type t = Z.t [@@deriving compare]
  module Bits = struct
    type t = Z.t
    let to_string = Z.to_bits
    let of_string = Z.of_bits
  end
  let empty = Z.zero
  let (+) flags (flag,_) = Z.logor flags flag
  let (-) flags (flag,_) = Z.logand flags (Z.lognot flag)
  let has flags (flag,_) =
    [%compare.equal : t] (Z.logand flags flag) flag
  let set_if cond flag =
    if cond then fun flags -> flags + flag else ident

  module T = struct
    type t = Z.t
    include Sexpable.Of_stringable(Bits)
    include Binable.Of_stringable(Bits)
  end

  let name = snd

  let assoc_of_props props =
    List.map !known_properties ~f:(fun p ->
        name p, has props p)

  let domain = KB.Domain.flat "props"
      ~empty:Z.one ~equal:Z.equal
      ~inspect:(fun props ->
          [%sexp_of: (string * bool) list]
            (assoc_of_props props))

  let persistent = KB.Persistent.of_binable (module T)

  let slot = KB.Class.property ~package:"bap"
      Theory.Semantics.cls "insn-properties" domain
      ~persistent
      ~public:true
      ~desc:"semantic properties of an instruction"
end


type t = Theory.Semantics.t
type op = Op.t [@@deriving bin_io, compare, sexp]


module Slot = struct
  type 'a t = (Theory.Effect.cls, 'a) KB.slot
  let empty = "#undefined"
  let text = KB.Domain.flat "text"
      ~inspect:sexp_of_string ~empty
      ~equal:String.equal

  let delay_t = KB.Domain.optional "delay_t"
      ~inspect:sexp_of_int
      ~equal:Int.equal


  let name = KB.Class.property ~package:"bap"
      Theory.Semantics.cls "insn-opcode" text
      ~persistent:KB.Persistent.string
      ~public:true
      ~desc:"instruction opcode"

  let asm = KB.Class.property ~package:"bap"
      Theory.Semantics.cls "insn-asm" text
      ~persistent:KB.Persistent.string
      ~public:true
      ~desc:"an assembly string"

  let sexp_of_op = function
    | Op.Reg r -> Sexp.Atom (Reg.name r)
    | Op.Imm w -> sexp_of_int64 (Imm.to_int64 w)
    | Op.Fmm w -> sexp_of_float (Fmm.to_float w)


  let ops_domain = KB.Domain.optional "insn-ops"
      ~equal:[%compare.equal: Op.t array]
      ~inspect:[%sexp_of: op array]

  let ops_persistent = KB.Persistent.of_binable (module struct
      type t = Op.t array option [@@deriving bin_io]
    end)

  let ops = KB.Class.property ~package:"bap"
      Theory.Semantics.cls "insn-ops" ops_domain
      ~persistent:ops_persistent
      ~public:true
      ~desc:"an array of instruction operands"

  let delay = KB.Class.property ~package:"bap"
      Theory.Semantics.cls "insn-delay" delay_t
      ~persistent:(KB.Persistent.of_binable (module struct
                     type t = int option [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"the length of the delay slot"

  type KB.conflict += Jump_vs_Move

  let dests =
    let empty = Some (Set.empty (module Theory.Label)) in
    let order x y : KB.Order.partial = match x,y with
      | Some x,_ when Set.is_empty x -> LT
      | _,Some x when Set.is_empty x -> GT
      | None,None -> EQ
      | None,_ | _,None -> NC
      | Some x, Some y ->
        if Set.equal x y then EQ else
        if Set.is_subset x y then LT else
        if Set.is_subset y x then GT else NC in
    let join x y = match x,y with
      | None,None -> Ok None
      | None,Some x |Some x,None ->
        if Set.is_empty x then Ok None
        else Error Jump_vs_Move
      | Some x, Some y -> Ok (Some (Set.union x y)) in
    let module IO = struct
      module Set = Set.Make_binable_using_comparator(Theory.Label)
      type t = Set.t option [@@deriving bin_io, sexp_of]
    end in
    let inspect = IO.sexp_of_t in
    let data = KB.Domain.define ~empty ~order ~join ~inspect "dest-set" in
    let persistent = KB.Persistent.of_binable (module IO) in
    KB.Class.property ~package:"bap" Theory.Semantics.cls
      "insn-dests" data
      ~persistent
      ~public:true
      ~desc:"a set of destinations of a control-flow instruction"
end

let normalize_asm asm =
  String.substr_replace_all asm ~pattern:"\t"
    ~with_:" " |> String.strip

type vis = {
  jump : bool;
  cond : bool;
  indirect : bool;
}

let lookup_jumps bil =
  let jump ?(cond=false) v = { v with jump = true; cond } in
  let conditional v = jump ~cond:true v in
  let indirect f v = f { v with indirect=true } in
  let cons check x xs = if check then x :: xs else xs in
  (object
    inherit [vis] Stmt.visitor
    method! enter_jmp ex vis = match ex with
      | Bil.Int _ when under_condition -> conditional vis
      | Bil.Int _ -> jump vis
      | _ when under_condition -> indirect conditional vis
      | _ -> indirect jump vis
  end)#run bil {jump=false;cond=false;indirect=false} |> fun v ->
  if not v.jump then []
  else
    cons (not v.cond) `Unconditional_branch [] |>
    cons v.cond `Conditional_branch |>
    cons v.indirect `Indirect_branch

let lookup_side_effects bil = (object
  inherit [kind list] Stmt.visitor
  method! enter_store ~mem:_ ~addr:_ ~exp:_ _ _ acc =
    `May_store :: acc
  method! enter_load ~mem:_ ~addr:_ _ _ acc =
    `May_load :: acc
end)#run bil []

let (<--) slot value insn = KB.Value.put slot insn value

let write init ops =
  List.fold ~init ops ~f:(fun init f -> f init)

let derive_props ?bil insn =
  let bil_kinds = match bil with
    | Some bil -> lookup_jumps bil @ lookup_side_effects bil
    | None -> [] in
  let is = Insn.is insn in
  let is_bil kind =
    if Option.is_some bil
    then List.mem ~equal:[%compare.equal : kind] bil_kinds kind
    else is kind in
  (* those two are the only which we can't get from the BIL semantics *)
  let is_return = is `Return in
  let is_call = is `Call in

  let is_conditional_jump = is_bil `Conditional_branch in
  let is_jump = is_conditional_jump || is_bil `Unconditional_branch in
  let is_indirect_jump = is_bil `Indirect_branch in
  let may_affect_control_flow =
    is_jump ||
    is `May_affect_control_flow in
  let is_barrier = is_jump &&  not is_call && not is_conditional_jump in
  let may_load = is_bil `May_load in
  let may_store = is_bil `May_store in
  Props.empty                                              |>
  Props.set_if is_jump jump                                |>
  Props.set_if is_conditional_jump conditional             |>
  Props.set_if is_indirect_jump indirect                   |>
  Props.set_if is_call call                                |>
  Props.set_if is_return return                            |>
  Props.set_if is_barrier barrier                          |>
  Props.set_if may_affect_control_flow affect_control_flow |>
  Props.set_if may_load load                               |>
  Props.set_if may_store store

let set_basic effect insn : t =
  write effect Slot.[
      name <-- Insn.name insn;
      asm <-- normalize_asm (Insn.asm insn);
      ops <-- Some (Insn.ops insn);
    ]

let of_basic ?bil insn : t =
  let effect =
    KB.Value.put Bil.slot
      (KB.Value.empty Theory.Semantics.cls)
      (Option.value bil ~default:[]) in
  write (set_basic effect insn) Slot.[
      Props.slot <-- derive_props ?bil insn;
    ]

let with_basic effect insn : t =
  let bil = KB.Value.get Bil.slot effect in
  write (set_basic effect insn) Slot.[
      Props.slot <-- derive_props ~bil insn
    ]


let get = KB.Value.get Props.slot
let put = KB.Value.put Props.slot
let is flag t = Props.has (get t) flag
let may = is
let must flag insn =  put insn Props.(get insn + flag)
let mustn't flag insn = put insn Props.(get insn - flag)
let should = must
let shouldn't = mustn't

let name = KB.Value.get Slot.name
let asm = KB.Value.get Slot.asm
let bil insn = KB.Value.get Bil.slot insn
let ops s = match KB.Value.get Slot.ops s with
  | None -> [||]
  | Some ops -> ops

let empty = KB.Value.empty Theory.Semantics.cls

module Adt = struct
  let pr fmt = Format.fprintf fmt

  let rec pp_ops ch = function
    | [] -> ()
    | [x] -> pr ch "%a" Op.pp_adt x
    | x :: xs -> pr ch "%a, %a" Op.pp_adt x pp_ops xs

  let props insn =
    List.filter !known_properties ~f:(fun p -> is p insn) |>
    List.map ~f:snd |>
    String.concat ~sep:", "

  let pp ppf insn =
    let name = name insn in
    if String.equal name Slot.empty
    then pr ppf "Undefined()"
    else pr ppf "%s(%a, Props(%s))"
        (String.capitalize name)
        pp_ops (Array.to_list (ops insn))
        (props insn)
end

let pp_adt = Adt.pp

module Trie = struct
  module Key = struct
    type token = string * Op.t array [@@deriving bin_io, compare, sexp]
    type t = token array

    let length = Array.length
    let nth_token = Array.get
    let token_hash = Hashtbl.hash
  end

  module Normalized = Trie.Make(struct
      include Key
      let compare_token (x,xs) (y,ys) =
        let r = compare_string x y in
        if r = 0 then Op.Normalized.compare_ops xs ys else r
      let hash_ops = Array.fold ~init:0
          ~f:(fun h x -> h lxor Op.Normalized.hash x)
      let hash (x,xs) =
        x lxor hash_ops xs
    end)

  let token_of_insn insn = name insn, ops insn
  let key_of_insns = Array.of_list_map ~f:token_of_insn

  include Trie.Make(Key)
end

include Regular.Make(struct
    type t = Theory.Semantics.t [@@deriving sexp, bin_io, compare]
    let hash t = Hashtbl.hash t
    let module_name = Some "Bap.Std.Insn"
    let version = "2.0.0"

    let string_of_ops ops =
      Array.map ops ~f:Op.to_string |> Array.to_list |>
      String.concat ~sep:","

    let pp fmt insn =
      let name = name insn in
      if String.equal name Slot.empty
      then Format.fprintf fmt "%s" name
      else Format.fprintf fmt "%s(%s)" name (string_of_ops (ops insn))
  end)

let pp_asm ppf insn =
  Format.fprintf ppf "%s" (normalize_asm (asm insn))

let () =
  Data.Write.create ~pp:Adt.pp () |>
  add_writer ~desc:"Abstract Data Type pretty printing format"
    ~ver:version "adt";
  Data.Write.create ~pp:pp_asm () |>
  add_writer ~desc:"Target assembly language" ~ver:"1.0" "asm";
  set_default_printer "asm"
