open Bap.Std
open Core

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


(* Abstract SSA *)
(*type t = 
  {
    (* pointer expression *)
    (* bil *)
    (* ssa name *)
  }*)

(* currentDef is a mapping from each variable to it's defining *)
(* expression. When recording into this mapping, the right hand *)
(* expression of the IR is stored. *)

(* filled - when local numbering for a block has finished. *)
(* sealed - when no further predecessors will be added to the *)
(* block. *)

let absexp_of_bil bil = 
  object(self)
    inherit [exp option] Stmt.finder
    method! enter_jmp exp r = r
  end
(*
let writeVariable variable block value =
  currentDef[variable][block] <- value

let readVariable variable block =
  if Set.mem currentDef[variable] block then
    currentDef[variable][block]
  else readVariableRecursive varaible block

let tryRemoveTrivialPhi phi = 
  let same = 
    List.fold phi.operands ~init:None ~f:(fun op curr -> 
        if op = curr or op = phi then
          curr (* Unique value or self reference *)
        else 
          Some(op)
      ) in
  if Option.is_some same then
    (* The phi merges at least two values: not trivial *)
    phi
  else 
    (* If same is none (unreachable or in the start block, *)
    (* then create an undefined value *)
    let same = Option.value same ~default:(Operand.create ()) in
    (* Remember all users except the phi itself *)
    let users = phi.users.remove phi in
    (* Reroute all uses of phi to same and remove phi *)
    phi.replaceBy same;
    (* Try to recursively remove all phi users, which might *)
    (* have become trivial *)
    Set.iter users ~f:(fun use -> 
        match use with
        | Phi(use) -> 
          tryRemoveTrivialPhi use
        | _ -> ());
    same

let addPhiOperands variable phi =
  (* If there is more than one, collect the definitions from all *)
  (* the predecessors and construct a phi function joining them *)
  (* into a single value. *)
  List.iter phi.block.preds ~f:(fun pred -> 
      phi.appendOperand (readVariable variable pred));
  tryRemoveTrivialPhi phi

(* if a block currently contains no definition for a variable, we *)
(* recursively look for a definition in its predecessors. *)
let rec readVariableRecursive variable block =
  if not Set.(mem sealedBlocks block) then
    let value = Phi.create block in
    incompletePhis[block][variable] <- value;
    writeVariable variable block value;
    value
  else if List.(length block.preds) = 1 then
    (* If the block has a single predecessor (edge leading into it) *)
    (* then recursively query it for a definition. *)
    let value = readVariable(variable, List.(hd block.preds)) in
    writeVariable variable block value;
    value
  else
    (* Determine operands from predecessors *)
    let value = Phi.create block in
    writeVaraible variable block value;
    let value = addPhiOperands variable value in
    writeVariable variable block value;
    value


let readVariable variable block =
  if Map.mem currentDef[variable] block then
    currentDef[variable][block]
  else
    readVariableRecursive variable block

let sealBlock block =
  Set.iter incompletePhis[block] ~f:(fun variable ->
      addPhiOperands variable incompletePhis[block][variable]
    );
  Set.add sealedBlocks block
*)
