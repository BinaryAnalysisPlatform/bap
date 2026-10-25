open Core_kernel[@@warning "-D"]
open Bap.Std
open OUnit2

let lift arch bytes =
  let memory = Or_error.ok_exn (Memory.create LittleEndian
      (Word.of_int ~width:(match arch with `x86 -> 32 | _ -> 64) 0x1000)
      (Bigstring.of_string bytes)) in
  let memory,insn = Or_error.ok_exn
      (Disasm_expert.Basic.with_disasm ~backend:"llvm" (Arch.to_string arch)
         ~f:(fun dis -> match Disasm_expert.Basic.insn_of_mem dis memory with
           | Ok (m,Some i,_) -> Ok (m,i)
           | _ -> Or_error.error_string "atomic fixture did not decode")) in
  let module Target = (val target_of_arch arch) in
  Target.lift memory insn

let calls bil = List.filter_map bil ~f:(Bil.decode Bil.intrinsic)

let case name arch bytes target = name >:: fun _ ->
  let bil = Or_error.ok_exn (lift arch bytes) in
  assert_equal [target] (calls bil);
  let before,after = List.split_while bil ~f:(fun s ->
      Option.is_none (Bil.decode Bil.intrinsic s)) in
  assert_bool "inputs precede the intrinsic" (List.length before >= 2);
  assert_bool "captured lane identities survive BIL-to-BIR"
    (List.exists before ~f:(function
         | Bil.Move (v,_) -> not (Var.is_virtual v)
             && String.is_prefix (Var.name v) ~prefix:"intrinsic:x86:atomic:"
         | _ -> false));
  let memory = (object inherit [bool] Stmt.visitor
      method! enter_load ~mem:_ ~addr:_ _ _ _ = true
      method! enter_store ~mem:_ ~addr:_ ~exp:_ _ _ _ = true
    end)#run bil false in
  assert_bool "the intrinsic is the only memory effect" (not memory);
  assert_bool "continuation contains result or flags" (not (List.is_empty after))

let captured name bytes role expected = name >:: fun _ ->
  let bil = Or_error.ok_exn (lift `x86_64 bytes) in
  let before = List.take_while bil ~f:(fun s ->
      Option.is_none (Bil.decode Bil.intrinsic s)) in
  let var = List.find_map_exn before ~f:(function
      | Bil.Move (v,_) when String.is_prefix (Var.name v)
          ~prefix:("intrinsic:x86:atomic:" ^ role ^ ":") -> Some v
      | _ -> None) in
  let state = Stmt.eval before (new Bili.context) in
  match state#lookup var with
  | Some result -> (match Bil.Result.value result with
      | Bil.Imm word -> assert_bool "captured value" (Word.equal word expected)
      | _ -> assert_failure "captured lane is not an immediate")
  | None -> assert_failure "captured lane is absent"

let suite = "atomic" >::: [
  case "add8" `x86_64 "\xf0\x00\x37" "x86:atomic_add_8";
  case "add16" `x86_64 "\xf0\x66\x01\x37" "x86:atomic_add_16";
  case "add32" `x86_64 "\xf0\x01\x37" "x86:atomic_add_32";
  case "add64" `x86_64 "\xf0\x48\x01\x37" "x86:atomic_add_64";
  case "ia32" `x86 "\xf0\x01\x37" "x86:atomic_add_32";
  case "implicit_xchg" `x86_64 "\x48\x87\x37" "x86:atomic_xchg_64";
  case "xadd" `x86_64 "\xf0\x48\x0f\xc1\x37" "x86:atomic_add_64";
  case "cmpxchg" `x86_64 "\xf0\x48\x0f\xb1\x37" "x86:atomic_cmpxchg_64";
  case "cmpxchg8b" `x86_64 "\xf0\x0f\xc7\x0f" "x86:atomic_cmpxchg_64";
  case "bts" `x86_64 "\xf0\x48\x0f\xba\x2f\x01" "x86:atomic_or_64";
  captured "neg_rip_has_no_immediate" "\xf0\x48\xf7\x1d\x00\x00\x00\x00"
    "address" (Word.of_int ~width:64 0x1008);
  captured "not_rip_has_no_immediate" "\xf0\x48\xf7\x15\x00\x00\x00\x00"
    "address" (Word.of_int ~width:64 0x1008);
  captured "signed_immediate" "\xf0\x48\x83\x07\xff" "input0" (Word.ones 64);
  "plain_add" >:: (fun _ ->
      assert_equal [] (calls (Or_error.ok_exn (lift `x86_64 "\x48\x01\x37"))));
  "register_xchg" >:: (fun _ ->
      assert_equal [] (calls (Or_error.ok_exn (lift `x86_64 "\x48\x87\xf7"))));
  "unsupported_cmpxchg16b" >:: (fun _ ->
      assert_bool "unsupported width is rejected"
        (Result.is_error (lift `x86_64 "\xf0\x48\x0f\xc7\x0f")));
  "invalid_lock_register" >:: (fun _ ->
      assert_bool "invalid LOCK form is rejected"
        (Result.is_error (lift `x86_64 "\xf0\x48\x01\xf7")));
]
