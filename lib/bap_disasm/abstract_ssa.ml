open Bap_types.Std
open Bap_image_std
open Core_kernel.Std

let stmt_def_vars =
  object(self)
    inherit [Exp.Set.t] Stmt.visitor
    method enter_move def use accu =
      if not Var.(is_virtual def) then
        Set.add accu Exp.(Bil.Var def)
      else accu
  end

let stmt_use_vars =
  object(self)
    inherit [Exp.Set.t] Stmt.visitor
    method enter_move def use accu =
      Set.add accu use
  end


let stmt_def_freevars =
  object(self)
    inherit [Var.Set.t] Stmt.visitor
    method enter_move def use accu =
      if not Var.(is_virtual def) then
        Set.add accu def
      else accu
  end

let stmt_use_freevars =
  object(self)
    inherit [Var.Set.t] Stmt.visitor
    method enter_move def use accu =
      let free_vars = 
        Set.filter ~f:(fun v -> not Var.(is_virtual v)) (Exp.free_vars use)
      in Set.union accu free_vars
  end

let def_ssa bil =
  stmt_def_vars#run bil Exp.Set.empty

let use_ssa bil =
  stmt_use_vars#run bil Exp.Set.empty 

let def_freevars bil =
  stmt_def_freevars#run bil Var.Set.empty

let use_freevars bil =
  stmt_use_freevars#run bil Var.Set.empty 
