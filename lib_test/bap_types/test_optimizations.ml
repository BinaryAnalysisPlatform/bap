open Core_kernel
open OUnit2
open Bap.Std

let width = 8
let typ = Type.imm width
let int x = Bil.int (Word.of_int ~width x)
let physical name = Var.create name typ
let virtual_ name = Var.create ~is_virtual:true name typ

let const = int 42

let always_alive = physical "x"
let will_survive = virtual_ "live"
let will_be_dead = virtual_ "dead"

let check bil (_ : test_ctxt) =
  let all = (object
    inherit [Var.Set.t] Stmt.visitor
    method! enter_var v vs = Set.add vs v
  end)#run bil Var.Set.empty in
  let checked =
    Set.mem all always_alive &&
    Set.mem all will_survive &&
    not (Set.mem all will_be_dead) in
  assert_bool "" checked

let bil = Bil.[
    will_survive := const;
    always_alive := var will_survive + const;
    will_be_dead := lnot (var always_alive);
]


let suite () =
  "Bil_optimizations" >::: [
    "check trivial dead" >:: check (Bil.prune_dead bil);
  ]
