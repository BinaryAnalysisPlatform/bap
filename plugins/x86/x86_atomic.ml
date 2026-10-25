open Core_kernel[@@warning "-D"]
open Bap.Std
open X86_types
open X86_env

(* Captured, width-qualified intrinsic lanes; the call itself is the memory effect. *)
let lane role bits =
  Var.create ~is_virtual:false ~fresh:false
    (sprintf "intrinsic:x86:atomic:%s:%d" role bits) (Type.Imm bits)

let emit ~op ~address ~bits operands =
  let address_bits = match Type.infer address with
    | Ok (Type.Imm n) -> n | _ -> invalid_arg "atomic address" in
  let addr = lane "address" address_bits in
  let inputs = List.mapi operands ~f:(fun i e ->
      Bil.Move (lane (sprintf "input%d" i) bits, e)) in
  let old = lane "old" bits in
  (Bil.Move (addr, address) :: inputs @
   [Bil.(encode intrinsic (sprintf "x86:atomic_%s_%d" op bits))], Bil.Var old)

let rmw ~op ~address ~bits value = emit ~op ~address ~bits [value]

let lift ~read ~write ~add_flags ~sub_flags ~inc_flags ~dec_flags ~logic_flags op =
  let address t dst = match read t dst with
    | Bil.Load (_, a, _, _) -> a
    | _ -> invalid_arg "LOCK requires a memory destination" in
  let bits = function Type.Imm n -> n | _ -> invalid_arg "atomic width" in
  let source t = function
    | Oimm w -> Bil.Int (Word.extract_exn ~hi:(bits t - 1) ~lo:0
                         (Word.signed w))
    | o -> read t o in
  let arithmetic name bop flags t dst src =
    let w = bits t in
    let pre, old = rmw ~op:name ~address:(address t dst) ~bits:w (source t src) in
    let value = Bil.Var (lane "input0" w) in
    pre @ flags w old value (Bil.BinOp (bop, old, value)) in
  let unary name bop flags t dst value =
    let w = bits t in
    let pre, old = rmw ~op:name ~address:(address t dst) ~bits:w value in
    pre @ flags w old value (Bil.BinOp (bop, old, value)) in
  match op with
  | Add (t,d,s) -> arithmetic "add" Bil.PLUS add_flags t d s
  | Sub (t,d,s) -> arithmetic "sub" Bil.MINUS sub_flags t d s
  | And (t,d,s) -> arithmetic "and" Bil.AND logic_flags t d s
  | Or (t,d,s) -> arithmetic "or" Bil.OR logic_flags t d s
  | Xor (t,d,s) -> arithmetic "xor" Bil.XOR logic_flags t d s
  | Inc (t,d) -> unary "add" Bil.PLUS inc_flags t d (Bil.Int (Word.one (bits t)))
  | Dec (t,d) -> unary "sub" Bil.MINUS dec_flags t d (Bil.Int (Word.one (bits t)))
  | Not (t,d) -> unary "xor" Bil.XOR (fun _ _ _ _ -> []) t d (Bil.Int (Word.ones (bits t)))
  | Neg (t,d) ->
    let w = bits t in
    let pre, old = emit ~op:"neg" ~address:(address t d) ~bits:w [] in
    pre @ sub_flags w (Bil.Int (Word.zero w)) old (Bil.UnOp (Bil.NEG, old))
  | Xadd (t,d,s) ->
    let w = bits t in
    let pre, old = rmw ~op:"add" ~address:(address t d) ~bits:w (read t s) in
    let value = Bil.Var (lane "input0" w) in
    pre @ [write t s old] @ add_flags w old value Bil.(old + value)
  | Xchg (t,a,b) ->
    let d,s = match a with Oaddr _ | Oaddr_rip_relative _ -> a,b | _ -> b,a in
    let pre, old = rmw ~op:"xchg" ~address:(address t d) ~bits:(bits t) (read t s) in
    pre @ [write t s old]
  | Cmpxchg (t,s,d) ->
    let w = bits t in
    let pre, old = emit ~op:"cmpxchg" ~address:(address t d) ~bits:w
        [read t o_rax; read t s] in
    let expected = Bil.Var (lane "input0" w) in
    pre @ sub_flags w expected old Bil.(expected - old) @
    [Bil.If (Bil.Var (lane "success" 1), [], [write t o_rax old])]
  | Cmpxchg8b d ->
    let t = Type.Imm 64 in
    let pre, old = emit ~op:"cmpxchg" ~address:(address t d) ~bits:64
        [Bil.Concat (read (Type.Imm 32) o_rdx, read (Type.Imm 32) o_rax);
         Bil.Concat (read (Type.Imm 32) o_rcx, read (Type.Imm 32) o_rbx)] in
    pre @ [Bil.Move (zf, Bil.Var (lane "success" 1));
           Bil.If (Bil.Var (lane "success" 1), [],
             [write (Type.Imm 32) o_rax (Bil.Extract (31,0,old));
              write (Type.Imm 32) o_rdx (Bil.Extract (63,32,old))])]
  | Adc (t,d,s) | Sbb (t,d,s) ->
    let w = bits t in
    let carry = Var.create ~is_virtual:true "atomic_carry" (Type.Imm 1) in
    let original = Var.create ~is_virtual:true "atomic_source" t in
    let c = Bil.Cast (Bil.UNSIGNED,w,Bil.Var carry) in
    let sub = match op with Sbb _ -> true | _ -> false in
    let name,bop,flags = if sub then "sub",Bil.MINUS,sub_flags else "add",Bil.PLUS,add_flags in
    let pre, old = rmw ~op:name ~address:(address t d) ~bits:w Bil.(var original + c) in
    let result = Bil.BinOp (bop,old,Bil.Var (lane "input0" w)) in
    let ext e = Bil.Cast (Bil.UNSIGNED,w+1,e) in
    let rhs = Bil.(ext (var original) + ext c) in
    let cf_value = if sub then Bil.(ext old < rhs)
      else Bil.Extract (w,w,Bil.(ext old + rhs)) in
    [Bil.Move (carry,Bil.Var cf); Bil.Move (original,source t s)] @ pre @
    flags w old (Bil.Var original) result @ [Bil.Move (cf,cf_value)]
  | _ -> invalid_arg "unsupported locked instruction"
