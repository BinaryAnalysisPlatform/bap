open Bap_core_theory
open KB.Syntax

(* TODO: Move into the separate plugin? *)
(* TODO: Enrich more with the standard types (and builtin methods/classes?) *)
module Java = struct
  type reg_name
  type regpair_name
  type value

  (* register pairs - it's wrong FIXME *)
  let regpair_name : regpair_name Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 4

  (* registers are indices in a stack frame *)
  let reg_name : reg_name Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 4

  (* we can define our own type hierarchy for Java,
     but let's start with just 32 bit integers for all
     primitive and reference types, it will hit us when
     we will start dealing with doubles and longs.
  *)
  let value : value Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 32

  (* Note the importance of the frames here! *)
  (* but frame is still a mapping from 4 bit offsets to 32 bit values.  *)
  let frame = Theory.Mem.define reg_name value

  let current_frame = Theory.Var.define frame "current_frame"

  (* Heap operations *)
  let heap_type = Theory.Mem.define value value
  let brk = Theory.Var.define value "brk"
  let heap = Theory.Var.define heap_type "mem"

end


