open Core_kernel
open OUnit2
open Bap.Std

[@@@warning "-D"]

let width = 8
let typ = Type.imm width
let int x = Bil.int (Word.of_int ~width x)

let all_vars = Var.Hash_set.create ()
let add_var v = Hash_set.add all_vars v; v
let physical name = add_var (Var.create name typ)
let virtual_ name = add_var (Var.create ~is_virtual:true name typ)

let true_ = Bil.int Word.b1
let zero = int 0
let one = int 1
let two = int 2
let four = int 4
let five = int 5
let eight = int 8
let x42 = int 42
let undefined = Bil.unknown "undefined" typ

let error_of_bil input got expected =
  let s =
    sprintf "something went wrong for\n%s\ngot\n%s\n"
      (Bil.to_string input) (Bil.to_string got) in
  match expected with
  | None -> s
  | Some e ->
    sprintf "%sexpected\n%s\n" s (Bil.to_string e)

let always_alive = physical "always_alive"
let will_survive = virtual_ "will_survive"
let could_be_dead = virtual_ "could_be_dead"
let v = virtual_ "v"
let v1 = virtual_ "v1"
let v2 = virtual_ "v2"
let x = physical "x"
let y = physical "y"
let z = physical "z"

let full_pipeline bil =
  Bil.fixpoint (fun b ->
       Bil.fold_consts b |>
       Bil.propagate_consts |>
       Bil.prune_dead_virtuals) bil

let random =
  let () = Random.self_init () in
  let len = 1000 in
  let xs = List.init len ~f:(fun _ -> Random.int 100 |> int) in
  fun () -> List.nth_exn xs (Random.int len)

let eval bil bil' =
  let init = Hash_set.fold all_vars ~init:[] ~f:(fun bil v ->
      Bil.(v := random ()) :: bil ) in
  let c1 = Stmt.eval (init @ bil)  (new Bili.context) in
  let c2 = Stmt.eval (init @ bil') (new Bili.context) in
  Hash_set.iter all_vars ~f:(fun v ->
      if Var.is_physical v then
        let res =
          match c1#lookup v, c2#lookup v with
          | None, None -> true
          | None, Some _ | Some _, None -> false
          | Some r1, Some r2 ->
            match Bil.Result.value r1, Bil.Result.value r2 with
            | Bil.Imm w1, Bil.Imm w2 -> Word.equal w1 w2
            | Bil.Bot, Bil.Bot
            | Bil.Mem _, Bil.Mem _ -> true
            | _ -> false in
        assert_bool ("random: " ^ error_of_bil bil bil' None) res)

let check f bil expected (_ : test_ctxt) =
  let bil' = f bil in
  eval bil bil';
  assert_bool (error_of_bil bil bil' (Some expected))
    (Bil.compare expected bil' = 0)

let (==>) = check Bil.prune_dead_virtuals

let preserve_physical =
  let bil = Bil.[ always_alive := zero; ] in
  bil ==> bil

let kill_virtual =
  Bil.[
    always_alive := zero;
    could_be_dead := var always_alive + one;
    always_alive := one;
  ] ==> Bil.[
    always_alive := zero;
    always_alive := one;
  ]

let preserve_physical_anyway =
  Bil.[
    always_alive := zero;
    always_alive := one;
    could_be_dead := var always_alive + one;
  ] ==> Bil.[
    always_alive := zero;
    always_alive := one;
  ]

let trivial_dead_1 =
  Bil.[
    always_alive := one;
    could_be_dead := two;
  ] ==> Bil.[ always_alive := one; ]

let trivial_dead_2 =
  Bil.[
    could_be_dead := one;
    always_alive := two;
  ] ==> Bil.[ always_alive := two; ]

let trivial_dead_3 =
  Bil.[
    will_survive := one;
    always_alive := var will_survive + two;
    could_be_dead := lnot (var always_alive);
  ] ==> Bil.[
      will_survive := one;
      always_alive := var will_survive + two;
    ]

let trivial_dead_4 =
  Bil.[
    will_survive := one;
    could_be_dead := lnot (var always_alive);
    always_alive := var will_survive + two;
  ] ==> Bil.[
      will_survive := one;
      always_alive := var will_survive + two;
    ]

let assigned_twice =
  Bil.[
    could_be_dead := one;
    always_alive := var could_be_dead + two;
    could_be_dead := lnot (var always_alive)
  ] ==> Bil.[
      could_be_dead := one;
      always_alive := var could_be_dead + two;
    ]

let survive_anyway_1 =
  let bil = Bil.[
      will_survive := one;
      if_ (var will_survive > zero) [] []
    ] in
  bil ==> bil

let survive_anyway_2 =
  let bil = Bil.[
      x := one;
      will_survive := two;
      if_ (var x > zero) [] [ will_survive := one];
      x := var will_survive;
    ] in
  bil ==> bil

let survive_anyway_3 =
  let bil = Bil.[
      x := one;
      if_ (var x < zero) [
        if_ (var x + var y = one) [
        ] [
          while_ (var z > var y) [
            if_ (var z > four) [
            ] [
              will_survive := one;
            ]
          ]
        ]
      ] [];
    ] in
  bil ==> bil

let includes_conditions =
  Bil.[
    will_survive := x42;
    if_ (var will_survive > zero) [
      could_be_dead := one;
    ] [
      could_be_dead := two;
    ];
    could_be_dead := one;
    always_alive := var could_be_dead;
    could_be_dead := lnot (var always_alive);
  ] ==> Bil.[
      will_survive := x42;
      if_ (var will_survive > zero) [] [];
      could_be_dead := one;
      always_alive := var could_be_dead;
    ]

let preserves_special =
  Bil.[
    could_be_dead := one;
    special "I will survive!";
  ] ==> Bil.[special "I will survive!";]

let preserves_cpuexn =
  Bil.[
    could_be_dead := one;
    cpuexn 42;
  ] ==> Bil.[cpuexn 42;]


let preserves_loop_1 =
  let bil = Bil.[
      will_survive := one;
      while_ (var will_survive < four) [
        will_survive := five
      ]
    ] in
  bil ==> bil

let preserves_loop_2 =
  Bil.[
    will_survive := two;
    while_ (var will_survive < four) [
      could_be_dead := one;
      will_survive := five;
    ];
    could_be_dead := two;
  ] ==> Bil.[
      will_survive := two;
      while_ (var will_survive < four) [
        could_be_dead := one;
        will_survive := five;
      ];
    ]

let preserves_loop_3 =
  let bil = Bil.[
    v1 := zero;
    v := four;
    while_ (var v > zero) [
      y := var v1 + one;
      v1 := one;
      v := var v - one;
    ] ] in
  bil ==> bil

let deep_nesting =
  let bil = Bil.[
    always_alive := one;
    will_survive := two;
    always_alive := four;
    if_ (var will_survive > zero) [
      while_ (var will_survive = two) [
        if_ (var always_alive > zero) [
          will_survive := one
        ] [
          could_be_dead := x42
        ];
      ]
    ] []
  ] in
  bil ==> bil

let with_jmp =
  let bil = Bil.[
      will_survive := x42;
      jmp (var will_survive);
    ] in
  bil ==> bil

let with_conditional_jmp_1 =
  let bil = Bil.[
      always_alive := one;
      will_survive := x42;
      if_ (var always_alive > zero) [
        jmp (var will_survive)
      ] [];
    ] in
  bil ==> bil

let with_conditional_jmp_2 =
  let bil = Bil.[
      always_alive := one;
      will_survive := x42;
      if_ (var always_alive > zero)
        [] [
        jmp (var will_survive)
      ];
    ] in
  bil ==> bil




let ($==>) = check Bil.propagate_consts

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

let trivial_propagate_4 =
  Bil.[
    x := x42;
    jmp (var y);
    z := var x;
  ] $==> Bil.[
      x := x42;
      jmp (var y);
      z := x42;
    ]

let trivial_propagate_5 =
  Bil.[
    x := undefined;
    z := var x;
  ] $==> Bil.[
      x := undefined;
      z := undefined;
    ]

let trivial_propagate_6 =
  Bil.[
    x := one;
    if_ (var x < zero) [
      y := undefined;
    ] [
      y := undefined;
    ];
    z := var y;
  ] $==> Bil.[
      x := one;
      if_ (one < zero) [
        y := undefined;
      ] [
        y := undefined;
      ];
      z := undefined;
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

let dont_merge_cond_branches_1 = Bil.[
    x := one;
    if_ (var x < zero) [
      y := two;
      jmp (var y);
    ] [
      y := four;
    ];
    z := var y;
  ] $==> Bil.[
    x := one;
    if_ (one < zero) [
      y := two;
      jmp two;
    ] [
      y := four;
    ];
    z := four;
  ]

let dont_merge_cond_branches_2 = Bil.[
    x := one;
    if_ (var x > zero) [
      y := two;
    ] [
      y := four;
      jmp (var y);
    ];
    z := var y;
  ] $==> Bil.[
    x := one;
    if_ (one > zero) [
      y := two;
    ] [
      y := four;
      jmp four;
    ];
    z := two;
  ]

let dont_merge_cond_branches_3 = Bil.[
    x := one;
    y := two;
    while_ (var x < zero) [
      y := one;
      jmp (var y);
    ];
    z := var y;
  ] $==> Bil.[
    x := one;
    y := two;
    while_ (one < zero) [
      y := one;
      jmp one;
    ];
    z := two;
  ]

let merging_cond_branches_3 = Bil.[
    x := one;
    y := two;
    if_ (var x > zero) [
      y := one;
      if_ (var y > zero) [
        jmp (var y);
      ] [];
    ] [];
    z := var y;
  ] $==> Bil.[
    x := one;
    y := two;
    if_ (one > zero) [
      y := one;
      if_ (one > zero) [
        jmp one;
      ] [];
    ] [];
    z := var y;
  ]

let merging_cond_branches_4 = Bil.[
    x := one;
    y := two;
    while_ (var x < zero) [
      y := one;
      if_ (var y < zero) [
        jmp (var y);
      ] [];
    ];
    z := var y;
  ] $==> Bil.[
    x := one;
    y := two;
    while_ (one < zero) [
      y := one;
      if_ (one < zero) [
        jmp one;
      ] [];
    ];
    z := var y;
  ]

let propagate_in_while_1 =
  let bil = Bil.[
      x := four;
      while_ (var x < five) [
        y := var x + one;
        x := x42;
      ];
    ] in
  bil $==> bil

let propagate_in_while_2 =
  let bil = Bil.[
      x := x42;
      while_ (var x > two) [
        y := var x + one;
        x := one;
        z := var x + two;
      ];
    ] in
  bil $==>  Bil.[
      x := x42;
      while_ (var x > two) [
        y := var x + one;
        x := one;
        z := one + two;
      ];
    ]

let propagate_in_while_3 =
  let bil = Bil.[
      x := x42;
      while_ (var z > four) [
        if_ (var z > one) [
          y := one;
        ] [
          y := one;
        ];
        if_ (var x + var y > one) [ (* the only that could be computed  *)
          y := zero;
        ] [
          y := two
        ];
        z := var y;
      ];
    ] in
  bil $==> Bil.[
      x := x42;
      while_ (var z > four) [
        if_ (var z > one) [
          y := one;
        ] [
          y := one;
        ];
        if_ (x42 + one > one) [
          y := zero;
        ] [
          y := two
        ];
        z := var y;
      ];
    ]

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

let all_together = Bil.[
    v := two;
    v1 := four + var v;
    if_ (var v1 > zero ) [
      v2 := var v1 + one;
      x := var v2 + one;
    ] [
      v2 := var v1 + two;
      x := var v2;
    ];
    v2 := four;
    y := var x - var v2;
    if_ (var y < zero) [
      z := four;
      jmp (var z + two - var y)
    ] [
      z := two;
    ];
    x := var z;
  ] => Bil.[
    x := eight;
    y := four;
    z := two;
    x := two;
  ]


let suite () =
  "Bil_optimizations" >::: [
    "preserves physical 1"    >:: preserve_physical;
    "preserves physical 2"    >:: preserve_physical_anyway;
    "kills virtual"           >:: kill_virtual;
    "trivial dead 1"          >:: trivial_dead_1;
    "trivial dead 2"          >:: trivial_dead_2;
    "trivial dead 3"          >:: trivial_dead_3;
    "trivial dead 4"          >:: trivial_dead_4;
    "assigned twice"          >:: assigned_twice;
    "survive anyway 1"        >:: survive_anyway_1;
    "survive anyway 2"        >:: survive_anyway_2;
    "survive anyway 3"        >:: survive_anyway_3;
    "conditional"             >:: includes_conditions;
    "preserves special"       >:: preserves_special;
    "preserves cpuexn"        >:: preserves_cpuexn;
    "preserves loop 1"        >:: preserves_loop_1;
    "preserves loop 2"        >:: preserves_loop_2;
    "preserves loop 3"        >:: preserves_loop_3;
    "deep nesting"            >:: deep_nesting;
    "with jmp"                >:: with_jmp;
    "with conditional jmp1"   >:: with_conditional_jmp_1;
    "with conditional jmp2"   >:: with_conditional_jmp_2;

    "trivial propagate 1"     >:: trivial_propagate_1;
    "trivial propagate 2"     >:: trivial_propagate_2;
    "trivial propagate 3"     >:: trivial_propagate_3;
    "trivial propagate 4"     >:: trivial_propagate_4;
    "trivial propagate 5"     >:: trivial_propagate_5;
    "trivial propagate 6"     >:: trivial_propagate_6;
    "propagate in if/else 1"  >:: merging_cond_branches_1;
    "propagate in if/else 2"  >:: merging_cond_branches_2;
    "propagate, cond_jmps 1"  >:: dont_merge_cond_branches_1;
    "propagate, cond_jmps 2"  >:: dont_merge_cond_branches_2;
    "propagate, cond_jmps 3"  >:: dont_merge_cond_branches_3;
    "propagate in if/else 3"  >:: merging_cond_branches_3;
    "merging cond branches"   >:: merging_cond_branches_4;
    "propagate in loop 1"     >:: propagate_in_while_1;
    "propagate in loop 2"     >:: propagate_in_while_2;
    "propagate in loop 3"     >:: propagate_in_while_3;

    "all together trivial"    >:: all_together_trivial;
    "vars not dead"           >:: vars_not_dead;
    "with computable cond"    >:: with_computable_cond;
    "dead unused with cond"   >:: dead_unused_with_cond;
    "different input: x = 1"  >:: with_various_input one two;
    "different input: x = 4"  >:: with_various_input four (Bil.var y);
    "different input: x = v"  >:: with_various_input (Bil.var v) (Bil.var y);
    "all together"            >:: all_together;
  ]
