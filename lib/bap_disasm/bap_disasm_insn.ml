open Core_kernel
open Bap_core_theory
open Regular.Std
open Bap_types.Std
open Bap_disasm_types

module Insn = Bap_disasm_basic.Insn
let package = "bap.std"

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

let jump                = prop "jump"
let conditional         = prop "cond"
let indirect            = prop "indirect"
let call                = prop "call"
let return              = prop "return"
let affect_control_flow = prop "affect_control_flow"
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
    Z.logand flags flag = flag
  let set_if cond flag =
    if cond then fun flags -> flags + flag else ident

  module T = struct
    type t = Z.t
    include Sexpable.Of_stringable(Bits)
    include Binable.Of_stringable(Bits)
  end

  let domain = KB.Domain.flat "props"
      ~empty:Z.zero ~is_empty:Z.(equal zero)

  let persistent = KB.Persistent.of_binable (module T)

  let slot = KB.Class.property ~package:"bap.std"
      ~persistent
      Theory.Program.cls "insn-properties" domain
end


type t = Theory.Program.t
type op = Op.t [@@deriving bin_io, compare, sexp]


module Slot = struct
  let empty = "#undefined"
  let text = KB.Domain.flat "text"
      ~inspect:sexp_of_string ~empty
      ~is_empty:(String.equal empty)

  let name = KB.Class.property ~package:"bap.std"
      ~persistent:KB.Persistent.string
      Theory.Program.cls "insn-opcode" text

  let asm = KB.Class.property ~package:"bap.std"
      ~persistent:KB.Persistent.string
      Theory.Program.cls "insn-asm" text

  let ops_domain = KB.Domain.optional "insn-ops"
      ~inspect:[%sexp_of: Op.t array]

  let ops_persistent = KB.Persistent.of_binable (module struct
      type t = Op.t array option [@@deriving bin_io]
    end)

  let ops = KB.Class.property ~package:"bap.std"
      ~persistent:ops_persistent
      Theory.Program.cls "insn-ops" ops_domain
end


let normalize_asm asm =
  String.substr_replace_all asm ~pattern:"\t"
    ~with_:" " |> String.strip

let lookup_jumps bil = (object
  inherit [kind list] Stmt.visitor
  method! enter_jmp ex _ =
    match ex with
    | Bil.Int _ when under_condition -> [`Conditional_branch]
    | Bil.Int _ -> [`Unconditional_branch]
    | _ when under_condition -> [`Conditional_branch; `Indirect_branch]
    | _ -> [`Indirect_branch]
end)#run bil []

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

let of_basic ?bil insn : t =
  let bil_kinds = match bil with
    | Some bil -> lookup_jumps bil @ lookup_side_effects bil
    | None -> [] in
  let is = Insn.is insn in
  let is_bil kind =
    if bil <> None
    then List.mem ~equal:[%compare.equal : kind] bil_kinds kind
    else is kind in
  let is_conditional_jump = is_bil `Conditional_branch in
  let is_jump = is_conditional_jump || is_bil `Unconditional_branch in
  let is_indirect_jump = is_bil `Indirect_branch in
  let is_return = is `Return in
  let is_call = is `Call in
  let may_affect_control_flow = is `May_affect_control_flow in
  let may_load = is_bil `May_load in
  let may_store = is_bil `May_store in
  let effect =
    KB.Value.put Bil.slot
      (KB.Value.empty Theory.Program.Semantics.cls)
      (Option.value bil ~default:[]) in
  let props =
    Props.empty                                              |>
    Props.set_if is_jump jump                                |>
    Props.set_if is_conditional_jump conditional             |>
    Props.set_if is_indirect_jump indirect                   |>
    Props.set_if is_call call                                |>
    Props.set_if is_return return                            |>
    Props.set_if may_affect_control_flow affect_control_flow |>
    Props.set_if may_load load                               |>
    Props.set_if may_store store in
  write (KB.Value.empty Theory.Program.cls) Slot.[
      Props.slot <-- props;
      name <-- Insn.name insn;
      asm <-- normalize_asm (Insn.asm insn);
      Theory.Program.Semantics.slot <-- effect;
      ops <-- Some (Insn.ops insn);
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
let bil insn = KB.Value.get Bil.slot (KB.Value.get Theory.Program.Semantics.slot insn)
let ops s = match KB.Value.get Slot.ops s with
  | None -> [||]
  | Some ops -> ops

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
    if name = Slot.empty
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
    type t = Theory.Program.t [@@deriving sexp, bin_io, compare]
    let hash t = Hashtbl.hash t
    let module_name = Some "Bap.Std.Insn"
    let version = "2.0.0"

    let string_of_ops ops =
      Array.map ops ~f:Op.to_string |> Array.to_list |>
      String.concat ~sep:","

    let pp fmt insn =
      let name = name insn in
      if name = Slot.empty
      then Format.fprintf fmt "%s" name
      else Format.fprintf fmt "%s(%s)" name (string_of_ops (ops insn))
  end)

let pp_asm ppf insn =
  Format.fprintf ppf "%s" (asm insn)

let () =
  Data.Write.create ~pp:Adt.pp () |>
  add_writer ~desc:"Abstract Data Type pretty printing format"
    ~ver:version "adt";
  Data.Write.create ~pp:pp_asm () |>
  add_writer ~desc:"Target assembly language" ~ver:"1.0" "asm";
  set_default_printer "asm"
