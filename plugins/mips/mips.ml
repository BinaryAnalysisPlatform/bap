open Core_kernel
open Bap.Std
open Bap_core_theory


(* This CPU model and instruction set is based on the
 * "MIPS Architecture For Programmers
 * Volume II-A: The MIPS64 Instruction Set Reference Manual"
 * Document Number: MD00087  Revision 6.04
 * November 13, 2015 *)

module Model = Mips_model

module Std = struct
  open Mips_rtl
  include Mips_utils
  include Mips_types
  include Mips_cpu
  include Mips_dsl

  module Array = Mips_rtl.Op_array

  module RTL = struct
    include Mips_rtl
    include Infix
    let foreach = foreach ~inverse:false
  end

  type rtl = RTL.rtl [@@deriving bin_io, compare, sexp]
  type exp = RTL.exp [@@deriving bin_io, compare, sexp]
  type lift = cpu -> op array -> rtl list

  let bil_of_rtl = RTL.bil_of_t

  let concat f g = fun cpu ops -> f cpu ops @ g cpu ops

  let (^) = concat

  let lifters = String.Table.create ()


  let delayed_opcodes = Hashtbl.create (module String)

  let register ?delay name lifter =
    Option.iter delay ~f:(fun d -> Hashtbl.add_exn delayed_opcodes name d);
    Hashtbl.add_exn lifters name lifter

  let (>>) = register

  let lift addr_size endian mem insn =
    let insn = Insn.of_basic insn in
    let insn_name = Insn.name insn in
    let cpu = make_cpu addr_size endian mem in
    let lift lifter =
      try
        lifter cpu (Insn.ops insn) |>
        bil_of_rtl |>
        Result.return
      with
      | Failure str -> Error (Error.of_string str)
      | Array.Invalid_operand_index n ->
        let str =
          sprintf "instruction %s doesn't have an operand with index %d"
            insn_name n in
        Error (Error.of_string str) in
    match String.Table.find lifters (Insn.name insn) with
    | None -> Or_error.errorf "unknown instruction %s" insn_name
    | Some lifter -> lift lifter

  module M32BE = struct
    module CPU = Model.MIPS_32_cpu
    let lift = lift `r32 BigEndian
  end

  module M32LE = struct
    module CPU = Model.MIPS_32_cpu
    let lift = lift `r32 LittleEndian
  end

  module M64BE = struct
    module CPU = Model.MIPS_64_cpu
    let lift = lift `r64 BigEndian
  end

  module M64LE = struct
    module CPU = Model.MIPS_64_cpu
    let lift = lift `r64 LittleEndian
  end

  include Model
end

let () =
  let provide_delay obj =
    let open KB.Syntax in
    Theory.Label.target obj >>= fun target ->
    if Theory.Target.belongs Bap_mips_target.parent target
    then
      KB.collect Theory.Semantics.slot obj >>| fun insn ->
      let name = KB.Value.get Insn.Slot.name insn in
      Hashtbl.find_and_call Std.delayed_opcodes name
        ~if_found:(fun delay ->
            KB.Value.put Insn.Slot.delay insn (Some delay))
        ~if_not_found:(fun _ -> insn)
    else !!Insn.empty in
  Bap_main.Extension.declare @@ fun _ctxt ->
  KB.Rule.(declare ~package:"mips" "delay-slot" |>
           require Insn.Slot.name |>
           provide Insn.Slot.delay |>
           comment "provides the delay slot length for branches");
  Ok (KB.promise Theory.Semantics.slot provide_delay)
