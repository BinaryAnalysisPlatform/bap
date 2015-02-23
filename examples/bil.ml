open Bap.Std

let zero = Word.of_int32 0l
let one  = Word.of_int32 1l
let x    = Var.create ~tmp:true "x" reg32_t
let s    = Bil.(x := var x + int one)

let r = Bil.(while_ (var x <> int zero) [
    s; s; s;
  ])

let q = Bil.(if_ (var x = int zero) [s;s] [r])
