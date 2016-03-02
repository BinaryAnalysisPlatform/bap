open Core_kernel.Std
open Regular.Std
open Bap_types.Std
open Bap_disasm_types

module Insn = Bap_disasm_basic.Insn

type must = Must
type may = May
type 'a property = Z.t * string

let known_properties = ref []

let new_property _ name : 'a property =
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
  include Sexpable.Of_stringable(Bits)
  include Binable.Of_stringable(Bits)
end

type t = {
  code : int;
  name : string;
  asm  : string;
  bil  : bil;
  ops  : Op.t array;
  props : Props.t;
} [@@deriving bin_io, fields, compare, sexp]

type op = Op.t [@@deriving bin_io, compare, sexp]

let normalize_asm asm =
  String.substr_replace_all asm ~pattern:"\t"
    ~with_:" " |> String.strip

let lookup_jumps bil = (object
  inherit [kind list] Bil.visitor
  method! enter_jmp ex _ =
    match ex with
    | Bil.Int _ when under_condition -> [`Conditional_branch]
    | Bil.Int _ -> [`Unconditional_branch]
    | _ when under_condition -> [`Conditional_branch; `Indirect_branch]
    | _ -> [`Indirect_branch]
end)#run bil []

let lookup_side_effects bil = (object
  inherit [kind list] Bil.visitor
  method! enter_store ~mem:_ ~addr:_ ~exp:_ _ _ acc =
    `May_store :: acc
  method! enter_load ~mem:_ ~addr:_ _ _ acc =
    `May_load :: acc
end)#run bil []

let of_basic ?bil insn =
  let bil_kinds = match bil with
    | Some bil -> lookup_jumps bil @ lookup_side_effects bil
    | None -> [] in
  let is = Insn.is insn in
  let is_bil kind =
    if bil <> None then List.mem bil_kinds kind else is kind in
  let is_conditional_jump = is_bil `Conditional_branch in
  let is_jump = is_conditional_jump || is_bil `Unconditional_branch in
  let is_indirect_jump = is_bil `Indirect_branch in
  let is_return = is `Return in
  let is_call = is `Call in
  let may_affect_control_flow = is `May_affect_control_flow in
  let may_load = is_bil `May_load in
  let may_store = is_bil `May_store in
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
  {
    code = Insn.code insn;
    name = Insn.name insn;
    asm  = normalize_asm (Insn.asm insn);
    bil  = Option.value bil ~default:[Bil.special "Unknown Semantics"];
    ops  = Insn.ops insn;
    props;
  }

let is flag t = Props.has t.props flag
let may = is
let must flag insn = {insn with props = Props.(insn.props + flag) }
let mustn't flag insn = {insn with props = Props.(insn.props - flag)}
let should = must
let shouldn't = mustn't

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

  let pp ch insn = pr ch "%s(%a, Props(%s))"
      (String.capitalize insn.name)
      pp_ops (Array.to_list insn.ops)
      (props insn)
end

let pp_adt = Adt.pp

module Trie = struct
  module Key = struct
    type token = int * Op.t array [@@deriving bin_io, compare, sexp]
    type t = token array

    let length = Array.length
    let nth_token = Array.get
    let token_hash = Hashtbl.hash
  end

  module Normalized = Trie.Make(struct
      include Key
      let compare_token (x,xs) (y,ys) =
        let r = compare_int x y in
        if r = 0 then Op.Normalized.compare_ops xs ys else r
      let hash_ops = Array.fold ~init:0
          ~f:(fun h x -> h lxor Op.Normalized.hash x)
      let hash (x,xs) =
        x lxor hash_ops xs
    end)

  let token_of_insn insn = insn.code, insn.ops
  let key_of_insns = Array.of_list_map ~f:token_of_insn

  include Trie.Make(Key)
end

include Regular.Make(struct
    type nonrec t = t [@@deriving sexp, bin_io, compare]
    let hash t = t.code
    let module_name = Some "Bap.Std.Insn"
    let version = "0.1"

    let string_of_ops ops =
      Array.map ops ~f:Op.to_string |> Array.to_list |>
      String.concat ~sep:","

    let pp fmt insn =
      Format.fprintf fmt "%s(%s)" insn.name (string_of_ops insn.ops)
  end)

let pp_asm ppf insn =
  Format.fprintf ppf "%s" insn.asm

let () =
  Data.Write.create ~pp:Adt.pp () |>
  add_writer ~desc:"Abstract Data Type pretty printing format"
    ~ver:version "adt";
  Data.Write.create ~pp:pp_asm () |>
  add_writer ~desc:"Target assembly language" ~ver:"1.0" "asm"
