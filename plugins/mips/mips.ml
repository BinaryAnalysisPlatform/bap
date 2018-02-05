open Core_kernel.Std
open Bap.Std

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
    let foreach = foreach ~inverse:true
  end

  type rtl = RTL.rtl [@@deriving bin_io, compare, sexp]
  type exp = RTL.exp [@@deriving bin_io, compare, sexp]
  type lift = cpu -> op array -> rtl list

  let bil_of_rtl = RTL.bil_of_t

  let concat f g = fun cpu ops -> f cpu ops @ g cpu ops

  let (^) = concat

  let lifters = String.Table.create ()

  let register name lifter =
    String.Table.change lifters name ~f:(fun _ -> Some lifter)

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
