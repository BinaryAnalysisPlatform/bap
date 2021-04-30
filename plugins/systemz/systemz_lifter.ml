open Core_kernel
open Bap_core_theory
open Bap.Std

open KB.Syntax
include Bap_main.Loggers()

module Target = Bap_systemz_target
module MC = Disasm_expert.Basic

let make_regs regs =
  let regs =
    List.mapi regs ~f:(fun i r -> (i,r)) |>
    Map.of_alist_exn (module Int) in
  Map.find_exn regs

let gpr = make_regs Target.gpr
let regnum s = Scanf.sscanf s "R%d" ident

(** [require_gpr insn n f] parses the machine instruction operands and
    requires that the [n]th register should be a GPR register and if
    it is, calls the action [f] with the variable that corresponds the register
*)
let require_gpr insn pos f =
  match (MC.Insn.ops insn).(pos) with
  | Op.Reg r -> f (gpr (regnum (Reg.name r)))
  | _ -> KB.return Insn.empty

(* The Theory of Systemz machines.

   For each target system we build a theory as a functor that takes
   the Core theory and extends it with operations specific to that
   target.
*)
module Systemz(CT : Theory.Core) = struct
  open Target

  (* We commonly start with a set of helpers, which also comprise the
     theory of our target. For example, [seq] extends the [seq x y] to
     [seq [x y ... z]], which makes it easier to write other
     functions...  *)
  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  (* ... for each sort of effects we will build a corresponding
     constructor. This one handles a sequence of data effects and
     packs them into a basic block that has the required type of [unit
     eff].  *)
  let data xs =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    CT.blk lbl (seq xs) (seq [])

  let (@) = CT.append r64


  (* finally, we can build the semantics of our LR instruction, we use
     our parser function [parse_gpr] to parse operands to GPR
     registers, and build the denotation as a data effect, which
     assigns the lower part of the destination register a value of the
     lower part of the source register.  This function relies on
     operations taken from the [CT] structure, namely [set] for
     assignment, [var] for reading the value of a variable, [high] and
     [low] for extracting parts of the register, and [@] that is
     defined above as an infix version of the [append] operation
     specialized for 64-bit registers.  *)
  let lr insn =
    require_gpr insn 0 @@ fun rd ->
    require_gpr insn 1 @@ fun rs ->
    data CT.[
        set rd (high r32 (var rd) @ low r32 (var rs))
      ]
end


(* The lifter provides the instruction semantics and has type
   [Theory.program KB.obj -> unit Theory.eff], or, the same
   type but using [Bap.Std] abbreviations,
   [tid -> insn knowledge].

   The only input parameter is a label that denotes a program
   location, aka a knowledge base object of type
   core:program. The label is like a generalized address - it
   uniquely identifies a program location, even across modules.

   To build the semantics the lifter can access various properties of
   the instruction. Accessing a property might trigger computations
   that associated with this property, e.g., the instruction will be
   disassembled if it wasn't yet. It is even possible to access the
   semantics property of the instruction (the property that the lifter
   is supposed to compute). Thus, the lifter could be a co-recursive
   function with other lifters (itself included, of course).

   The list of properties of that class  can be obtained using the
   following command: `bap list classes -f core:program`.

   The return type [unit Theory.eff] is the denotation of effects that
   the instruction performs. The ['a Theory.eff] type is an
   abbreviation for ['a Theory.effect], which, in turn, is an
   abbreviation to the knowledge base value of class
   [Theory.Effect.cls], therefore the fully de-abbreviated type of the
   return type is:

   [(Theory.Effect.cls, unit Theory.Effect.sort) KB.cls KB.value knowledge]

   The set of sorts that parameterize the class denote what kinds of
   effects the instruction can have. With the [Effect.top] effect,
   which is denoted with the [unit] type, being the sort of all sorts
   of effects that an instruction can possibly have. In other words,
   the return type of the lifter expects an object that can denote any
   effect.

   To build the denotations of effects the lifter function is using
   a theory object, which is created using [Theory.instance] and
   [Theory.require] functions. The theory is a structure that
   includes a lot of functions that comprise the [Theory.Core]
   signature. This functions can be seen as smart constructors that
   build denotations.

   In our example, we will build a denotation for only one instrucion,
   `LR` (for load register) that loads (moves) a value from one
   register to another.

   Our simple framework matches on the opcode name and calls corresponding
   function, which expects the decoded machine instruction as its
   input and produces the effect denotation.
*)
let lifter label : unit Theory.eff =
  let lift insn lifter =
    lifter insn >>| fun sema -> Insn.with_basic sema insn in
  Theory.Label.target label >>= fun t ->
  if Theory.Target.belongs Target.parent t then
    KB.collect MC.Insn.slot label >>= function
    | None -> KB.return Insn.empty
    | Some insn ->
      Theory.instance () >>= Theory.require >>= fun (module Core) ->
      let module Systemz = Systemz(Core) in
      let open Systemz in
      lift insn @@ match MC.Insn.name insn with
      | "LR" -> lr
      | code ->
        info "unsupported opcode: %s" code;
        fun _ -> KB.return Insn.empty
  else !!Insn.empty

(* a lifter is a promise to provide the instruction semantics.*)
let load () =
  KB.promise Theory.Semantics.slot lifter
