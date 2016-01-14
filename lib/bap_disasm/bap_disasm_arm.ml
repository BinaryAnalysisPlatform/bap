open Core_kernel.Std
open Or_error

open Bap_types.Std
open Bap_disasm_arm_types


let sexpable_of_string t_of_sexp name =
  try Some (t_of_sexp @@ Sexp.of_string name)
  with Sexp.Of_sexp_error _ -> None


module Cond = struct
  include Cond
  include Regular.Make(struct
      type t = cond with bin_io, compare, sexp
      let hash (cond : t) = Hashtbl.hash cond
      let module_name = Some "Bap.Std.ARM.Cond"
      let version = "0.1"
      let pp fmt cond =
        Format.fprintf fmt "%a" Sexp.pp (sexp_of_t cond)
    end)
  let of_int_exn = function
    | 0 ->  `EQ
    | 1 ->  `NE
    | 2 ->  `CS
    | 3 ->  `CC
    | 4 ->  `MI
    | 5 ->  `PL
    | 6 ->  `VS
    | 7 ->  `VC
    | 8 ->  `HI
    | 9 ->  `LS
    | 10 -> `GE
    | 11 -> `LT
    | 12 -> `GT
    | 13 -> `LE
    | 14 -> `AL
    | n -> invalid_argf "not a condition: %d" n ()

  let create w =
    Word.to_int w >>= fun w ->
    try_with (fun () -> of_int_exn w)

end


module Reg = struct
  let create reg =
    sexpable_of_string reg_of_sexp (Basic.Reg.name reg)

  include Regular.Make(struct
      type t = reg with bin_io, compare, sexp
      let hash (reg : t) = Hashtbl.hash reg
      let module_name = Some "Bap.Std.ARM.Reg"
      let version = "0.1"
      let pp fmt reg =
        Format.fprintf fmt "%a" Sexp.pp (sexp_of_t reg)
    end)
  include Reg

end

module Op = struct
  include Regular.Make(struct
      type t = op with bin_io, compare, sexp
      let module_name = Some "Bap.Std.ARM.Op"
      let version = "0.1"
      let pp fmt op =
        Format.fprintf fmt "%a" Sexp.pp (sexp_of_t op)
      let hash (op : op) = Hashtbl.hash op
    end)
  include Op

  let create : Basic.op -> op option =
    let open Option.Monad_infix in
    function
    | Basic.Op.Fmm fmm -> None
    | Basic.Op.Reg reg -> Reg.create reg >>| fun reg -> Reg reg
    | Basic.Op.Imm imm ->
      Basic.Imm.to_word ~width:32 imm >>| fun imm -> Imm imm
end

module Insn = struct
  let create insn =
    sexpable_of_string insn_of_sexp (Basic.Insn.name insn)

  include Regular.Make(struct
      type t = insn with bin_io, compare, sexp
      let module_name = Some "Bap.Std.ARM.Insn"
      let version = "0.1"
      let pp fmt insn =
        Format.fprintf fmt "%a" Sexp.pp (sexp_of_t insn)
      let hash (insn : t) = Hashtbl.hash insn
    end)
  include Insn

end
