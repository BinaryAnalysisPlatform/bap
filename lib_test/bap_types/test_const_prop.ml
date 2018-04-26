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
let propagate = Bil.propagate_consts
let no_propagation = None

let x = var "x"
let y = var "y"
let z = var "z"
let res = var "res"
let zero = const 0
let c4 = const 4
let c8 = const 8

let find_result = (object
  inherit [word] Stmt.finder
  method! enter_move y e r =
    if Var.(y = res) then
      match e with
      | Bil.Int w -> r.return (Some w)
      | _ -> r
    else r
end)#find

let check expected bil ctxt =
  let actual = find_result (propagate bil) in
  assert_equal ~ctxt ~cmp:equal expected actual

let simple =
  check (expected 12)
    Bil.[
      x := c4;
      x := var x + c8;
      res := var x;
    ]

let fail_simple =
  check no_propagation
    Bil.[
      x := c4;
      x := var z;
      res := var x + c8;
    ]

let if_ =
  check (expected 12)
    Bil.[
      x := c4;
      if_ (var x > zero) [
        y := var x + c8;
      ] [
        y := c4;
      ];
      res := var y;
    ]

let fail_if =
  check no_propagation
    Bil.[
      x := c4;
      if_ (var x > var z) [
        y := c8;
      ] [
        y := c4;
      ];
      res := var y;
    ]

let else_ =
  check (expected 8)
    Bil.[
      x := c4;
      if_ (var x < zero) [
        y := var x + c8;
      ] [
        y := c8;
      ];
      res := var y;
    ]

let fail_else =
  check no_propagation
    Bil.[
      x := c4;
      if_ (var x > var z) [
        y := c4;
      ] [
        y := c8;
      ];
      res := var y;
    ]

let while_ =
  check (expected 12)
    Bil.[
      x := c4;
      while_ (var x > zero) [
        y := c4 + c8;
      ];
      res := var y;
    ]

let fail_while =
  check no_propagation
    Bil.[
      x := c4;
      y := c8;
      while_ (var x > zero) [
        x := var y;
        y := c4 + c8;
      ];
      res := var y;
    ]

let nested =
  check (expected 8)
    Bil.[
      x := c4;
      if_ (var x > zero) [
        y := var x + var x + var x;
        while_ (var y > var x) [
          if_ (var y < zero) [
            z := c4;
          ] [
            z := c8;
          ]
        ];
      ] [];
      res := var z;
    ]

let suite () =
  "Const propagation" >::: [
    "simple propagation"       >:: simple;
    "fail propagation"         >:: fail_simple;
    "simple if"                >:: if_;
    "fail propagation in if"   >:: fail_if;
    "simple else"              >:: else_;
    "fail propagation in else" >:: fail_else;
    "simple while"             >:: while_;
    "while with redefines"     >:: fail_while;
    "nested"                   >:: nested;
  ]
