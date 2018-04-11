open Core_kernel
open Bap_common
open Bap_bil

class exp_state : object
  val exps_stack  : exp  list
  val under_condition : bool
end

class stmt_state : object
  val preds : bil
  val succs : bil
  val stmts_stack : bil
  val in_jmp : bool
  val in_move : bool
  val in_loop : bool
end

class ['a] exp_visitor : object
  inherit exp_state
  method enter_exp : exp -> 'a -> 'a
  method visit_exp : exp -> 'a -> 'a
  method leave_exp : exp -> 'a -> 'a

  method enter_load : mem:exp -> addr:exp -> endian -> size -> 'a -> 'a
  method visit_load : mem:exp -> addr:exp -> endian -> size -> 'a -> 'a
  method leave_load : mem:exp -> addr:exp -> endian -> size -> 'a -> 'a

  method enter_store : mem:exp -> addr:exp -> exp:exp -> endian -> size -> 'a -> 'a
  method visit_store : mem:exp -> addr:exp -> exp:exp -> endian -> size -> 'a -> 'a
  method leave_store : mem:exp -> addr:exp -> exp:exp -> endian -> size -> 'a -> 'a

  method enter_binop : binop -> exp -> exp -> 'a -> 'a
  method visit_binop : binop -> exp -> exp -> 'a -> 'a
  method leave_binop : binop -> exp -> exp -> 'a -> 'a

  method enter_unop : unop -> exp -> 'a -> 'a
  method visit_unop : unop -> exp -> 'a -> 'a
  method leave_unop : unop -> exp -> 'a -> 'a

  method enter_cast : cast -> nat1 -> exp -> 'a -> 'a
  method visit_cast : cast -> nat1 -> exp -> 'a -> 'a
  method leave_cast : cast -> nat1 -> exp -> 'a -> 'a

  method enter_let : var -> exp:exp -> body:exp -> 'a -> 'a
  method visit_let : var -> exp:exp -> body:exp -> 'a -> 'a
  method leave_let : var -> exp:exp -> body:exp -> 'a -> 'a

  method enter_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a
  method visit_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a
  method leave_ite : cond:exp -> yes:exp -> no:exp -> 'a -> 'a

  method enter_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a
  method visit_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a
  method leave_extract : hi:nat1 -> lo:nat1 -> exp -> 'a -> 'a

  method enter_concat : exp -> exp -> 'a -> 'a
  method visit_concat : exp -> exp -> 'a -> 'a
  method leave_concat : exp -> exp -> 'a -> 'a

  method enter_int : word -> 'a -> 'a
  method visit_int : word -> 'a -> 'a
  method leave_int : word -> 'a -> 'a

  method enter_var : var -> 'a -> 'a
  method visit_var : var -> 'a -> 'a
  method leave_var : var -> 'a -> 'a

  method enter_unknown : string -> typ -> 'a -> 'a
  method visit_unknown : string -> typ -> 'a -> 'a
  method leave_unknown : string -> typ -> 'a -> 'a
end


class ['a] bil_visitor : object
  inherit ['a] exp_visitor
  inherit stmt_state

  method run : bil -> 'a -> 'a
  method enter_stmt : stmt -> 'a -> 'a
  method visit_stmt : stmt -> 'a -> 'a
  method leave_stmt : stmt -> 'a -> 'a
  method enter_move : var -> exp -> 'a -> 'a
  method visit_move : var -> exp -> 'a -> 'a
  method leave_move : var -> exp -> 'a -> 'a

  method enter_jmp : exp -> 'a -> 'a
  method visit_jmp : exp -> 'a -> 'a
  method leave_jmp : exp -> 'a -> 'a

  method enter_while : cond:exp -> bil -> 'a -> 'a
  method visit_while : cond:exp -> bil -> 'a -> 'a
  method leave_while : cond:exp -> bil -> 'a -> 'a

  method enter_if : cond:exp -> yes:bil -> no:bil -> 'a -> 'a
  method visit_if : cond:exp -> yes:bil -> no:bil -> 'a -> 'a
  method leave_if : cond:exp -> yes:bil -> no:bil -> 'a -> 'a

  method enter_cpuexn : int -> 'a -> 'a
  method visit_cpuexn : int -> 'a -> 'a
  method leave_cpuexn : int -> 'a -> 'a

  method enter_special : string -> 'a -> 'a
  method visit_special : string -> 'a -> 'a
  method leave_special : string -> 'a -> 'a
end

class ['a] bil_finder : object
  inherit ['a option return] bil_visitor
  method find : bil -> 'a option
end

class ['a] exp_finder : object
  inherit ['a option return] exp_visitor
  method find : exp -> 'a option
end

class exp_mapper : object
  inherit exp_state
  method map_exp : exp -> exp
  method map_load : mem:exp -> addr:exp -> endian -> size -> exp
  method map_store : mem:exp -> addr:exp -> exp:exp -> endian -> size -> exp
  method map_binop : binop -> exp -> exp -> exp
  method map_unop : unop -> exp -> exp
  method map_cast : cast -> nat1 -> exp -> exp
  method map_let : var -> exp:exp -> body:exp -> exp
  method map_ite : cond:exp -> yes:exp -> no:exp -> exp
  method map_extract : hi:nat1 -> lo:nat1 -> exp -> exp
  method map_concat : exp -> exp -> exp
  method map_int : word -> exp
  method map_var : var -> exp
  method map_sym : var -> var
  method map_unknown : string -> typ -> exp
end


class bil_mapper : object
  inherit exp_mapper
  inherit stmt_state
  method run : bil -> bil

  method map_stmt : stmt -> bil
  method map_move : var -> exp -> bil
  method map_jmp : exp -> bil
  method map_while : cond:exp -> bil -> bil
  method map_if : cond:exp -> yes:bil -> no:bil -> bil
  method map_cpuexn : int -> bil
  method map_special : string -> bil
end
