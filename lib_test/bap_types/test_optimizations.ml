open Core_kernel
open OUnit2
open Bap.Std

let width = 8
let typ = Type.imm width
let int x = Bil.int (Word.of_int ~width x)
let physical name = Var.create name typ
let virtual_ name = Var.create ~is_virtual:true name typ

let true_ = Bil.int Word.b1
let zero = int 0
let one = int 1
let two = int 2
let four = int 4
let x42 = int 42

let error_of_bil input got =
  sprintf "something went wrong for\n%s\ngot\n%s\n"
    (Bil.to_string input) (Bil.to_string got)

let alive_anyway = physical "alive_anyway"
let will_survive = virtual_ "will_survive"
let will_be_dead = virtual_ "will_be_dead"
let could_be_dead = will_be_dead

let check f bil expected (_ : test_ctxt) =
  let bil' = f bil in
  assert_bool (error_of_bil bil bil') (Bil.compare expected bil' = 0)

let (==>) = check Bil.prune_dead

let preserves_physical_1 =
  let bil = Bil.[ alive_anyway := zero; ] in
  bil ==> bil

let preserves_physical_2 =
  Bil.[
    alive_anyway := zero;
    will_be_dead := var alive_anyway + one;
    alive_anyway := one;
  ] ==> Bil.[
    alive_anyway := zero;
    alive_anyway := one;
  ]

let preserves_physical_3 =
  Bil.[
    alive_anyway := zero;
    alive_anyway := one;
    will_be_dead := var alive_anyway + one;
  ] ==> Bil.[
    alive_anyway := zero;
    alive_anyway := one;
  ]

let trivial_dead_1 =
  Bil.[
    alive_anyway := one;
    will_be_dead := two;
  ] ==> Bil.[ alive_anyway := one; ]

let trivial_dead_2 =
  Bil.[
    will_be_dead := one;
    alive_anyway := two;
  ] ==> Bil.[ alive_anyway := two; ]

let trivial_dead_3 =
  Bil.[
    will_survive := one;
    alive_anyway := var will_survive + two;
    will_be_dead := lnot (var alive_anyway);
  ] ==> Bil.[
      will_survive := one;
      alive_anyway := var will_survive + two;
    ]

let trivial_dead_4 =
  Bil.[
    will_survive := one;
    will_be_dead := lnot (var alive_anyway);
    alive_anyway := var will_survive + two;
  ] ==> Bil.[
      will_survive := one;
      alive_anyway := var will_survive + two;
    ]

let assigned_twice =
  Bil.[
    could_be_dead := one;
    alive_anyway := var could_be_dead + two;
    will_be_dead := lnot (var alive_anyway)
  ] ==> Bil.[
      could_be_dead := one;
      alive_anyway := var could_be_dead + two;
    ]

let contains_conditions =
  Bil.[
    will_survive := x42;
    if_ (var will_survive > zero) [
      will_be_dead := one;
    ] [
      will_be_dead := two;
    ];
    could_be_dead := one;
    alive_anyway := var could_be_dead;
    will_be_dead := lnot (var alive_anyway);
  ] ==> Bil.[
      will_survive := x42;
      if_ (var will_survive > zero) [] [];
      could_be_dead := one;
      alive_anyway := var could_be_dead;
    ]

let preserves_special =
  Bil.[
    will_be_dead := one;
    special "I will survive!";
  ] ==> Bil.[special "I will survive!";]

let preserves_loop_1 =
  let bil = Bil.[
      will_survive := x42;
      while_ (var will_survive > zero) [
        will_survive := one
      ]
    ] in
  bil ==> bil

let preserves_loop_2 =
  Bil.[
    will_survive := x42;
    while_ (var will_survive > zero) [
      could_be_dead := one
    ];
    will_be_dead := two
  ] ==> Bil.[
      will_survive := x42;
      while_ (var will_survive > zero) [
        could_be_dead := one
      ];
    ]

let deep_nesting =
  let bil = Bil.[
    alive_anyway := one;
    will_survive := x42;
    alive_anyway := two;
    if_ (var will_survive > zero) [
      while_ (var will_survive > zero) [
        if_ (var alive_anyway > zero) [
          will_survive := one
        ] [
          could_be_dead := x42
        ] ;
    ] ] (* else *) [] ] in
  bil ==> bil

let with_jmp =
  let bil = Bil.[
      will_survive := x42;
      jmp (var will_survive);
    ] in
  bil ==> bil

let with_conditional_jmp_1 =
  let bil = Bil.[
      alive_anyway := one;
      will_survive := x42;
      if_ (var alive_anyway > zero) [
        jmp (var will_survive)
      ] [];
    ] in
  bil ==> bil

let with_conditional_jmp_2 =
  let bil = Bil.[
      alive_anyway := one;
      will_survive := x42;
      if_ (var alive_anyway > zero)
        [] [
        jmp (var will_survive)
      ];
    ] in
  bil ==> bil



let ($==>) = check Bil.propagate_consts
let v = virtual_ "v"
let x = physical "x"
let y = physical "y"
let z = physical "z"

let trivial_propagate_1 =
  Bil.[
    v := x42;
    x := var v + one
  ] $==> Bil.[
    v := x42;
    x := x42 + one
  ]

let trivial_propagate_2 =
  Bil.[
    x := x42;
    y := var x + one
  ] $==> Bil.[
    x := x42;
    y := x42 + one
  ]

let trivial_propagate_3 =
  Bil.[
    x := x42;
    jmp (var x)
  ] $==> Bil.[
      x := x42;
      jmp x42;
    ]

let merging_cond_branches_1 =
  Bil.[
    x := x42;
    if_ (var x > zero) [
      y := one;
    ] [
      y := two;
    ];
    z := var y;
  ] $==> Bil.[
      x := x42;
      if_ (x42 > zero) [
        y := one;
      ] [
        y := two;
      ];
      z := var y;
    ]

let merging_cond_branches_2 =
  Bil.[
    x := x42;
    if_ (var x > zero) [
      y := one;
    ] [
      y := one;
    ];
    z := var y;
  ] $==> Bil.[
      x := x42;
      if_ (x42 > zero) [
        y := one;
      ] [
        y := one;
      ];
      z := one;
    ]

let dont_merge_cond_branches = Bil.[
    x := one;
    if_ (var x > zero) [
      y := two;
      jmp (var y);
    ] [
      y := four;
    ];
    z := var y;
  ] $==> Bil.[
    x := one;
    if_ (one > zero) [
      y := two;
      jmp two;
    ] [
      y := four;
    ];
    z := four;
  ]

let propagate_in_while_1 =
  let bil = Bil.[
      x := x42;
      while_ (var x > zero) [
        y := var x + one;
        x := one;
      ];
    ] in
  bil $==> bil

let propagate_in_while_2 =
  let bil = Bil.[
      x := x42;
      while_ (var x > zero) [
        y := var x + one;
        x := one;
        z := var x + two;
        x := var x - one;
      ];
    ] in
  bil $==>  Bil.[
      x := x42;
      while_ (var x > zero) [
        y := var x + one;
        x := one;
        z := one + two;
        x := var x - one;
      ];
    ]



let full_pipeline bil =
  Bil.fixpoint (fun b ->
       Bil.fold_consts b |>
       Bil.propagate_consts |>
       Bil.prune_dead) bil

let (=>) = check full_pipeline

let all_together_trivial = Bil.[
    v := one;
    v := var v + one;
    v := var v + one;
    v := var v + one;
    x := var v;
  ] => Bil.[
    x := four;
  ]

let vars_not_dead = Bil.[
    x := two;
    y := var x + two
  ] => Bil.[
    x := two;
    y := four;
  ]

let with_computable_cond = Bil.[
    x := one;
    v := var x + one;
    y := var v * two;
    if_ (var y < two) [
      z := one;
    ] [
      z := two;
    ];
    y := var z;
  ] => Bil.[
    x := one;
    y := four;
    z := two;
    y := two;
  ]

let dead_unused_with_cond = Bil.[
    x := one;
    v := one;
    if_ (var v > zero) [v := four] [ v := two];
    z := var x;
  ] => Bil.[
    x := one;
    z := one;
  ]

let with_various_input input expected _ctxt =
  let bil =
    Bil.[
      x := input;
      if_ (var x > two) [
        while_ (var x > zero) [
          y := var x * two;
          x := var x - one;
        ]
      ] [
        if_ (var x = zero) [
          y := one;
        ] [
          if_ (var x = one) [
            y := two
          ] [
            y := four;
          ]
        ]
      ];
      z := var y;
    ] in
  let bil' = full_pipeline bil in
  let z_value = Option.value_exn
      ((object inherit [exp option] Stmt.visitor
        method! enter_move var e x =
          if Var.name var = "z" then Some e
          else x
      end)#run bil' None) in
  let error = sprintf "error with various input %s\n"
      (Exp.to_string expected) in
  assert_bool error Exp.(z_value = expected)


let suite () =
  "Bil_optimizations" >::: [
    "preserves physical 1"    >:: preserves_physical_1;
    "preserves physical 2"    >:: preserves_physical_2;
    "preserves physical 3"    >:: preserves_physical_3;
    "trivial dead 1"          >:: trivial_dead_1;
    "trivial dead 2"          >:: trivial_dead_2;
    "trivial dead 3"          >:: trivial_dead_3;
    "trivial dead 4"          >:: trivial_dead_4;
    "assigned twice"          >:: assigned_twice;
    "conditional"             >:: contains_conditions;
    "preserves special"       >:: preserves_special;
    "preserves loop 1"        >:: preserves_loop_1;
    "preserves loop 2"        >:: preserves_loop_2;
    "deep nesting"            >:: deep_nesting;
    "with jmp"                >:: with_jmp;
    "with conditional jmp1"   >:: with_conditional_jmp_1;
    "with conditional jmp2"   >:: with_conditional_jmp_2;

    "trivial propagate 1"     >:: trivial_propagate_1;
    "trivial propagate 2"     >:: trivial_propagate_2;
    "trivial propagate 3"     >:: trivial_propagate_3;
    "propagate in if/else 1"  >:: merging_cond_branches_1;
    "propagate in if/else 2"  >:: merging_cond_branches_2;
    "propagate, cond_jmps"    >:: dont_merge_cond_branches;
    "propagate in while 1"    >:: propagate_in_while_1;
    "propagate in while 2"    >:: propagate_in_while_2;

    "all together trivial"    >:: all_together_trivial;
    "vars not dead"           >:: vars_not_dead;
    "with computable cond"    >:: with_computable_cond;
    "dead unused with cond"   >:: dead_unused_with_cond;
    "with various input 1"    >:: with_various_input one two;
    "with various input 2"    >:: with_various_input four (Bil.var y);
    "with various input 3"    >:: with_various_input (Bil.var v) (Bil.var y);

  ]
