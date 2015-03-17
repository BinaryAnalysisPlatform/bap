open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_types

module Insn = Bap_disasm_basic.Insn


type t = {
  code : int;
  name : string;
  asm  : string;
  bil  : bil;
  ops  : Op.t array;
  is_jump : bool;
  is_conditional_jump : bool;
  is_indirect_jump : bool;
  is_call : bool;
  is_return : bool;
  may_affect_control_flow : bool;
  may_load : bool;
  may_store : bool;
} with bin_io, fields, sexp

type op = Op.t with bin_io, compare, sexp

let compare t1 t2 =
  let r = Int.compare t1.code t2.code in
  if r = 0 then String.compare t1.asm t2.asm else r

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
  method! enter_store ~dst:_ ~addr:_ ~src:_ _ _ acc =
    `May_store :: acc
  method! enter_load ~src:_ ~addr:_ _ _ acc =
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
  {
    code = Insn.code insn;
    name = Insn.name insn;
    asm  = normalize_asm (Insn.asm insn);
    bil  = Option.value bil ~default:[];
    ops  = Insn.ops insn;
    is_jump;
    is_conditional_jump;
    is_indirect_jump;
    is_call;
    is_return;
    may_affect_control_flow;
    may_load;
    may_store;
  }

let has_side_effect insn = may_store insn || may_load insn
let is_unconditional_jump insn =
  is_jump insn || not (is_conditional_jump insn)

module Trie = struct
  module Key = struct
    type token = int * Op.t array with bin_io, compare, sexp
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
    type nonrec t = t with sexp, bin_io, compare
    let hash = code
    let module_name = "Bap_disasm_insn"

    let string_of_ops ops =
      Array.map ops ~f:Op.to_string |> Array.to_list |>
      String.concat ~sep:","

    let pp fmt insn =
      let open Format in
      pp_print_string fmt insn.asm;
      pp_print_tbreak fmt 0 10;
      Format.fprintf fmt "; %s(%s)" insn.name (string_of_ops insn.ops)
  end)
