open Core_kernel.Std
open OUnit2
open Bap.Std


let r8 f  w = f (Word.of_int ~width:8 w)
let r16 f w = f (Word.of_int ~width:16 w)
let r32 f w = f (Word.of_int32 w)

let zero = (Word.of_int32 0l)
let one  = (Word.of_int32 1l)
let two  = (Word.of_int32 2l)
let el = LittleEndian
let be = BigEndian

let undefined = Bil.Bot
let word w = Bil.Imm w
let addr = Bil.int zero
let addr1 = Bil.int one
let addr2 = Bil.int two
let data = Bil.int @@ Word.of_int32 0xDEADBEEFl
let memory = Var.create "mem" (mem32_t `r8)
let mem = Bil.var memory
let r = Var.create "result" reg32_t
let a = Var.create "a" reg32_t
let b = Var.create "b" reg32_t


let printer = function
  | Bil.Bot   -> "<bottom>"
  | Bil.Mem w -> "<memory>"
  | Bil.Imm w -> sprintf "%a" Word.pps w

let assert_exp value exp ctxt =
  assert_equal ~ctxt ~printer value (Exp.eval exp)

let assert_prg  value prg ctxt =
  let open Monad.State.Monad_infix in
  let bili = new bili in
  assert_equal ~ctxt ~printer value @@
  let res = bili#eval prg >>= fun () -> bili#lookup r >>| Bil.Result.value in
  Monad.State.eval res (new Bili.context)
[@@warning "-D"]

let suite () =
  "Bili" >::: [
    "mem[0,el]:32 ~> bot" >::
    assert_exp undefined Bil.(load ~mem ~addr:(int one) el `r32);

    "(mem with [0,el]:u32 <- 0xDEADBEEF)[0,el]:u32 ~> 0xDEADBEEF" >::
    assert_exp (r32 word 0xDEADBEEFl)
      Bil.(load ~mem:(store ~mem ~addr data el `r32) ~addr el `r32);

    "(mem with [0,el]:u32 <- 0xDEADBEEF)[0,be]:u32 ~> 0xEFBEADDEl" >::
    assert_exp (r32 word 0xEFBEADDEl)
      Bil.(load ~mem:(store ~mem ~addr data el `r32) ~addr be `r32);

    "(mem with [0,el]:u32 <- 0xDEADBEEF)[0,be]:u8 ~> 0xEF" >::
    assert_exp (r8 word 0xEF)
      Bil.(load ~mem:(store ~mem ~addr data el `r32) ~addr be `r8);

    "(mem with [0,el]:u32 <- 0xDEADBEEF)[0,el]:u8 ~> 0xEF" >::
    assert_exp (r8 word 0xEF)
      Bil.(load ~mem:(store ~mem ~addr data el `r32) ~addr el `r8);

    "(mem with [0,el]:u32 <- 0xDEADBEEF)[1,be]:u8 ~> 0xBE" >::
    assert_exp (r8 word 0xBE)
      Bil.(load ~mem:(store ~mem ~addr data el `r32) ~addr:addr1 be `r8);

    "(mem with [0,el]:u32 <- 0xDEADBEEF)[1,el]:u8 ~> 0xBE" >::
    assert_exp (r8 word 0xBE)
      Bil.(load ~mem:(store ~mem ~addr data el `r32) ~addr:addr1 el `r8);

    "(mem with [0,el]:u32 <- 0xDEADBEEF)[1,be]:u16 ~> 0xBEAD" >::
    assert_exp (r16 word 0xBEAD)
      Bil.(load ~mem:(store ~mem ~addr data el `r32) ~addr:addr1 be `r16);

    "(mem with [0,el]:u32 <- 0xDEADBEEF)[1,el]:u16 ~> 0xADBE" >::
    assert_exp (r16 word 0xADBE)
      Bil.(load ~mem:(store ~mem ~addr data el `r32) ~addr:addr1 el `r16);

    "(3 * 8 + 3)/3 ~> 9" >:: assert_exp (r8 word 9)
      Bil.((r8 int 3 * r8 int 8 + r8 int 3) / r8 int 3);


    "let a = 2 in let a = a + 3 in a * 2 ~> 10" >::
    assert_exp (r8 word 10)
      Bil.(let_ a (r8 int 2)
             (let_ a (var a + r8 int 3) (var a * r8 int 2)));

    "
    a := 3; a := a + let a = 2 in a; result := a
    --------------------------------------------
    delta[result] |-> 5" >:: assert_prg (r32 word 5l) Bil.([
        a := r32 int 3l;
        a := var a + let_ a (r32 int 2l) (var a);
        r := var a;
      ]);

    "
     result := 2
     --------------------
     delta[result] |-> 2" >::
    assert_prg (r32 word 2l) Bil.([r := r32 int 2l]);

    "
     a := 2; b := 3; result := a + b
     --------------------------------
     delta[result] |-> 5" >:: assert_prg (r32 word 5l) Bil.([
        a := r32 int 2l;
        b := r32 int 3l;
        r := var a + var b]);

    "
     a := 3; b := 0; while (a > 0) { a := a - 1; b := b + 1}; result := b
     ------------------------------------------------------------
     delta[result] |-> 3 " >:: assert_prg (r32 word 3l) Bil.([
        a := r32 int 3l;
        b := r32 int 0l;
        while_ (var a > r32 int 0l) [
          a := var a - r32 int 1l;
          b := var b + r32 int 1l;
        ];
        r := var b;
      ]);

    "collatz" >::
    (* Starting with 17, we take 12 steps in the `3x+1` problem (OEIS). *)
    assert_prg (r32 word 12l) Bil.([
        a := r32 int 17l;
        r := r32 int 0l;
        while_ (r32 int 1l <> var a) [
          if_ (r32 int 0l = var a mod r32 int 2l)
            [a := var a / r32 int 2l]
            [a := var a * r32 int 3l + r32 int 1l];
          r := var r + r32 int 1l;
        ]])
  ]
