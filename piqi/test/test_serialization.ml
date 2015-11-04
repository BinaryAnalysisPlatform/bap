open OUnit2
open Core_kernel.Std
open Bap.Std
open Bir_piqi
open Bil.Types

let label_to_string c1 =
  match c1 with
  | Direct tid1 -> Tid.to_string tid1
  | Indirect e1 -> Exp.to_string e1

let opt_label_to_string oc1 =
  match oc1 with
  | Some c1 -> label_to_string c1
  | None -> " opt label none! "

let jmp_to_string jmp1 =
  match jmp1 with
  | Call c1 -> label_to_string (Call.target c1) ^ " " 
                        ^ opt_label_to_string (Call.return c1)
  | Goto l1 -> label_to_string l1
  | Ret r1 -> label_to_string r1
  | Int (i1, tid1) -> Int.to_string i1 ^ " " ^ Sexp.to_string @@ Tid.sexp_of_t tid1

let test_jmp_kind orig ctxt =
  let piqi_kind = piqi_of_jmp_kind orig in
  let bap_kind = jmp_kind_of_piqi piqi_kind in
  assert_equal ~ctxt bap_kind orig
    ~msg:("Original: " ^ (jmp_to_string orig) ^
         "\n\ttranslated: " ^ (jmp_to_string bap_kind))

let test_jmp_serialization orig ctxt =
  let piqi_jmp = piqi_of_jmp_term orig in
  let bap_jmp = jmp_of_piqi piqi_jmp in
  assert_equal ~cmp:Jmp.(=) ~ctxt bap_jmp orig ~msg:("Original: " ^ Jmp.to_string orig ^
                                        "\n\ttranslated: " ^ Jmp.to_string bap_jmp)

let test_intent_serialization orig ctxt =
  let piqi_intent = piqi_of_intent orig in
  let bap_intent = intent_of_piqi piqi_intent in
  assert_equal ~ctxt bap_intent orig

let test_def_term_serialization orig ctxt =
  let piqi_def_term = piqi_of_def_term orig in
  let bap_def_term = def_of_piqi piqi_def_term in
  assert_equal ~cmp:Def.(=) ~ctxt bap_def_term orig ~msg:(Def.to_string orig)

(* failed *)
let test_phi_term_serialization orig ctxt =
  let piqi_phi_term = piqi_of_phi_term orig in
  let bap_phi_term = phi_of_piqi piqi_phi_term in
  assert_equal ~cmp:Phi.(=) ~ctxt bap_phi_term orig ~msg:(Phi.to_string orig)

(* Failed *)
let test_blk_term_serialization blk_term ctxt =
  let piqi_blk_term = piqi_of_blk_term blk_term in
  let bap_blk_term = blk_of_piqi piqi_blk_term in
  assert_equal ~cmp:Blk.(=) ~ctxt bap_blk_term blk_term ~msg:(Blk.to_string blk_term)

let make_phi ?(tid=(Tid.create ())) ?var ?exp () =
  let var = Option.value var ~default:(Var.create "phi_var" (Type.Imm 0)) in
  let exp = Option.value exp ~default:(Int (Addr.of_int ~width:64 0)) in
  Phi.create ~tid var (Tid.create ()) exp

let make_def ?(tid=(Tid.create ())) ?var ?exp () =
  let var = Option.value var ~default:(Var.create ~tmp:true "def_var" (Type.Mem (`r32, `r64) )) in
  let exp = Option.value exp ~default:(Int (Addr.of_int ~width:32 0)) in
  Def.create ~tid var exp
    
let make_jmp ?(tid=(Tid.create ())) ?cond ?kind () =
  let cond = Option.value cond ~default:(Var (Var.create "test" (Type.Imm 0))) in
  let kind = Option.value kind ~default:(Goto (Direct (Tid.create ()))) in
  Jmp.create ~tid ~cond kind

let blk_term =
  let phi = make_phi () in
  let def = make_def () in
  let jmp = make_jmp () in
  let blk = Blk.Builder.create
      ~tid:(Tid.create ()) ~phis:1 ~defs:1 ~jmps:1 () in
  Blk.Builder.add_def blk def;
  Blk.Builder.add_jmp blk jmp;
  Blk.Builder.add_phi blk phi;
  Blk.Builder.result blk

let suite =
  let open Bil.Types in
  "serialization" >:::
  [
    (*| Goto of label             jump inside subroutine      *)
      (* label indirect *)
      "test direct label jmp_kind conversion" >:: test_jmp_kind @@ Goto (Direct (Tid.create ()));
      "test indirect label jmp_kind conversion" >:: test_jmp_kind (Goto (Indirect (Bap.Std.Bil.Int (Addr.of_int ~width:32 0) )));
      (* label direct *)
    (*| Call of call              call to subroutine          *)
      "test jmp_kind of call with return" >:: test_jmp_kind (Call (Call.create ~return:(Direct (Tid.create ())) ~target:(Direct (Tid.create ())) ()));
    (*| Ret  of label             return from call to label   *)
      "test jmp_kind with ret" >:: test_jmp_kind (Ret (Direct (Tid.create ())));
    (*| Int  of int * tid         interrupt and return to tid *)
      "test jmp_kind of int" >:: test_jmp_kind (Bap.Std.Int (0, (Tid.create ()))); (* TODO failed *)
    (* val jmp_term_to_piqi : Jmp.t -> Ir_piqi.jmp_term *)
      "jmp = exp * jmp_kind encode decode" >:: test_jmp_serialization
        (make_jmp ());
    (* intent = | In | Out | Both *)
      "intent serialization" >:: test_intent_serialization (Some In);
      "intent serialization" >:: test_intent_serialization (Some Out);
      "intent serialization" >:: test_intent_serialization (Some Both);
    (* type 'a term = { tid ; 'a ; dict } *)
    (* def term to piqi. type def = (var * exp) term *)
      "def term serialization" >:: test_def_term_serialization
        (make_def ());
    (* type phi = (var * exp Tid.Map.t) *)
      "phi term serialization" >:: test_phi_term_serialization
        (make_phi ());
    (* type blk =   phis : phi term array; defs : def term array; jmps : jmp term array; } *)
      "blk term serialization" >:: test_blk_term_serialization
        blk_term;
(*
val program_to_piqi : Program.t -> Ir_piqi.program
val sub_term_to_piqi : Sub.t -> Ir_piqi.sub
val arg_term_to_piqi : Arg.t -> Ir_piqi.arg



*)
  ]
