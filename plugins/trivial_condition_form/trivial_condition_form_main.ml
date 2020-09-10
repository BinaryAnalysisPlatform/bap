open Core_kernel
open Bap.Std
open Format

include Self()



(** Trivial Condition Form (TCF) transformation.

    In the TCF a condition expression must be either a variable or a
    constant. The transformations detect non-trivial condition
    expressions and bind them to variables whose definitions are
    pushed to the block definition list. *)
module TCF = struct
  let blk_without_jmps = Term.filter jmp_t ~f:(fun _ -> false)
  let new_var () = Var.create ~is_virtual:true ~fresh:true "c" bool_t

  (* Pre: number of jumps is greater than 1
     post: number of jumps is the same, each jump is in TCF.*)
  let blk blk =
    Term.enum jmp_t blk |>
    Seq.fold ~init:(blk_without_jmps blk) ~f:(fun blk jmp ->
        match Jmp.cond jmp with
        | Bil.Int _ | Bil.Var _ -> Term.append jmp_t blk jmp
        | cond ->
          let var = new_var () in
          let def = Def.create var cond in
          let blk = Term.append def_t blk def in
          let jmp = Jmp.with_cond jmp (Bil.var var) in
          Term.append jmp_t blk jmp)

  let sub = Term.map blk_t ~f:(fun b ->
      if Term.length jmp_t b > 0 then blk b else b)

  let prog = Term.map sub_t ~f:sub

  let proj = Project.map_program ~f:prog
end


let main proj =
  info "translating the program into the Trivial Condition Form (TCF)";
  TCF.proj proj

open Config;;

manpage [
  `S "DESCRIPTION";
  `P "Ensures that all branching conditions are either a variable
or a constant. We call such representation a Trivial Condition Form
(TCF). During the translation all complex condition expressions are
hoisted into the assignment section of a block.";
];;


let () = when_ready (fun _ ->
    Project.register_pass ~runonce:true main)
