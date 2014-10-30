open Bap_types.Std

let zero = Word.of_int32 0l
let one  = Word.of_int32 1l
let x    = Var.create "x" reg32_t
let e    = Exp.(var x + int one)
let s    = Stmt.move x e

let r = Stmt.(While (Exp.(var x <> int zero), [
    s; s; s;
  ]))

let q = Stmt.(If ((Exp.(var x = int zero)), [s;s], [r]))

let x = Bitvector.of_int32 0l
let y = Addr.memref x ~index:2 ~scale:`r32 ~disp:100
