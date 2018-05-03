open Core_kernel.Std
open OUnit2
open Bap.Std

let equal w w' = match w, w' with
  | None, None -> true
  | Some w, Some w' -> Word.equal w w'
  | _ -> false

let width = 32
let var n = Var.create n (Type.imm width)
let word x = Word.of_int ~width x
let const x = Bil.int @@ word x
let expected x = Some (word x)
let no_reduction = None

let x = var "x"
let y = var "y"
let z = var "z"
let result = var "result"
let zero = const 0
let c4 = const 4
let c8 = const 8

let find_result = (object
  inherit [word] Stmt.finder
  method! enter_move y e r =
    if Var.(y = result) then
      match e with
      | Bil.Int w -> r.return (Some w)
      | _ -> r
    else r
end)#find

let check expected bil ctxt =
  let actual = find_result (Bil.reduce bil) in
  assert_equal ~ctxt ~cmp:equal expected actual

let simple =
  check (expected 12)
    Bil.[
      x := c4;
      x := var x + c8;
      result := var x;
    ]

let simple_fail =
  check no_reduction
    Bil.[
      x := c4;
      x := var z;
      result := var x + c8;
    ]

let simple' =
  check (expected 12)
    Bil.[
      x := c4;
      result := var x + c8;
      x := var z;
    ]

let computable_if =
  check (expected 12)
    Bil.[
      x := c4;
      if_ (var x > zero) [
        y := var x + c8;
      ] [
        y := c4;
      ];
      result := var y;
    ]

let computable_if' =
  check (expected 16)
    Bil.[
      x := c4;
      if_ (var x > zero) [
        y := var x + c8;
        result := var y + c4;
      ] [
        y := c4;
      ];
    ]

let uncomputable_if =
  check no_reduction
    Bil.[
      x := c4;
      if_ (var x > var z) [
        y := c8;
      ] [
        y := c4;
      ];
      result := var y;
    ]

let uncomputable_if' =
  check (expected 12)
    Bil.[
      x := c4;
      if_ (var x > var z) [
        y := c8;
        x := c8;
      ] [
        y := c8;
        result := var y + var x;
      ];
    ]

let suite () =
  "Const reduction" >::: [
    "simple reduction #1"      >:: simple;
    "simple reduction #2"      >:: simple';
    "simple fail in reduction" >:: simple_fail;
    "computable if cond #1"    >:: computable_if;
    "computable if cond #2"    >:: computable_if';
    "uncomputable if cond #1"  >:: uncomputable_if;
    "uncomputable if cond #2"  >:: uncomputable_if';
  ]
