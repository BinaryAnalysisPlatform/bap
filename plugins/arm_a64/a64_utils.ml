open A64_defs


exception Unbound_Reg

let is_64bit_register (r: registers_t) = 
	match r with
	| `X0 | `X1 | `X2 | `X3 | `X4 | `X5 | `X6 | `X7 | `X8 | `X9
	| `X10 | `X11 | `X12 | `X13 | `X14 | `X15 | `X16 | `X17 | `X18 | `X19
	| `X20 | `X21 | `X22 | `X23 | `X24 | `X25 | `X26 | `X27 | `X28 | `X29 
	| `X30 |`XZR | `SP | `PC | `ELR | `SPSR -> true
	| _ -> false


let is_64bit_register_op (operand: operand_t) = 
	match operand with
	| `Reg r -> is_64bit_register r
	| `Imm i -> raise (Failure "todo")


  