open Core_kernel.Std
open OUnit2
open Bap.Std
open Monads.Std

module SM = Monad.State

(* We have to redefine bili to be sure
   Simpl won't be called inside Bap.Std *)
module B = Bili.Make(SM)

class ['a] t = object(self)
  inherit ['a] B.t

  method! eval_exp e = match e with
    | Bil.Var v -> self#eval_var v
    | Bil.BinOp (op,u,v) -> self#eval_binop op u v
    | Bil.UnOp (op,u) -> self#eval_unop op u
    | Bil.Int u -> self#eval_int u
    | _ -> failwith "unexpected exp"
end

let cmps = Bil.[eq; neq; lt; le; slt; sle]

let binops = Bil.[plus; minus; times; divide; divide; modulo; smodulo;
                  lshift; rshift; arshift; bit_and; bit_or; bit_xor]

let eval bil = Monad.State.exec ((new t)#eval bil)
let random max = Random.int max
let get_elt xs = List.nth_exn xs @@ random (List.length xs)
let int x width = Bil.int (Word.of_int ~width x)
let gen_corner = int (get_elt [0; 1; -1])
let gen_int = get_elt [int (random 5); gen_corner]
let gen_binop ()  = get_elt binops
let gen_cmp   ()  = get_elt cmps
let with_unop e   = get_elt Bil.[unop neg; ident; unop not; ident;] @@ e

let get_var width =
  Bil.var @@ Var.create ~is_virtual:true ~fresh:true "tmp" (Type.imm width)

let gen_exp make_binop width =
  let rec get_hs () = match random 3 with
    | 0 -> get_var width
    | 1 -> gen_int width
    | _ -> get ()
  and get () =
    let x = with_unop @@ get_hs () in
    let y = with_unop @@ get_hs () in
    Bil.binop (make_binop ()) x y in
  get ()

type result = [
  | `Result of word
  | `No_result
  | `Div_zero
]

let string_of_result = function
  | `Result w -> sprintf "Result %s" (Word.to_string w)
  | `No_result -> "No result"
  | `Div_zero  -> "Div zero"

let eval var bil =
  try
    let ctxt = eval bil (new Bili.context) in
    match ctxt#lookup var with
    | None -> `No_result
    | Some r ->
      match Bil.Result.value r with
      | Bil.Imm w -> `Result w
      | _ -> `No_result
  with Division_by_zero -> `Div_zero

let report_fail src simpl r =
  let last b = List.hd_exn (List.rev b) |> Stmt.to_string in
  match simpl with
  | None -> eprintf "FAIL to simplify\n%s\n" (last src)
  | Some simpl ->
    let diff = match r with
      | None -> ""
      | Some (r,r') ->
        sprintf "%s and %s"
          (string_of_result r) (string_of_result r') in
    eprintf "FAIL, different results: %s\n" diff;
    let init = List.(rev (tl_exn (rev src))) in
    let () = match init with
      | [] -> ()
      | init -> eprintf "init:\n%s\n" (Bil.to_string init) in
    eprintf "%s\n" (last src);
    eprintf "%s\n" (last simpl)

let same_bil b b' = Bil.compare b b' = 0

let simpl_bil bil =
  try Some (Stmt.simpl bil)
  with Division_by_zero -> None

let init width e =
  Set.fold (Exp.free_vars e) ~init:[]
    ~f:(fun bil v -> Bil.(v := gen_int width) :: bil)

let run width make_exp =
  let e = make_exp width in
  let tmp = Var.create "tmp" (Type.imm width) in
  let bil = init width e @ Bil.[tmp := e ] in
  let result = eval tmp bil in
  match simpl_bil bil with
  | None when result = `Div_zero -> Ok ()
  | None -> Error (bil, None, None)
  | Some bil' when same_bil bil bil' -> Ok ()
  | Some bil' ->
    let result' = eval tmp bil' in
    match result, result' with
    | `Result w, `Result w' when Word.equal w w' -> Ok ()
    | `Div_zero, _ -> Ok ()
    | _ -> Error (bil, Some bil', Some (result,result'))

let random fn ~width ~times ctxt =
  let () = Random.self_init () in
  let calls = List.init times ~f:(fun _ -> run width (gen_exp fn)) in
  let fails = List.fold calls ~init:0
      ~f:(fun n -> function
          | Ok () -> n
          | Error (bil, simpl, res) ->
            report_fail bil simpl res;
            n + 1) in
  assert_equal ~ctxt ~cmp:Int.equal 0 fails

let exps width =
  let x = get_var width in
  let c1 = int 4 width in
  let c2 = int 2 width in
  List.fold binops ~init:[]
    ~f:(fun acc op ->
        acc @
        List.fold binops ~init:[]
          ~f:(fun acc' op' ->
              let e1 = Bil.(binop op (binop op' x c1) c2) in
              let e2 = Bil.(binop op (binop op' c1 x) c2) in
              let e3 = Bil.(binop op c1 (binop op' x c2)) in
              let e4 = Bil.(binop op c1 (binop op' c2 x)) in
              e1 :: e2 :: e3 :: e4 :: acc'))

let run_exps ctxt =
  let () = Random.self_init () in
  let width = 32 in
  let exps = exps width in
  let calls = List.init (List.length exps)
      ~f:(fun i -> run width (fun _ -> List.nth_exn exps i)) in
  let fails = List.fold calls ~init:0
      ~f:(fun n -> function
          | Ok () -> n
          | Error (bil, simpl, res) ->
            report_fail bil simpl res;
            n + 1) in
  assert_equal ~ctxt ~cmp:Int.equal 0 fails

let width = 8
let int x = Bil.int @@ Word.of_int ~width x
let c0 = int 0
let c1 = int 1
let _c1 = int (-1)
let c2 = int 2
let _c2 = int (-2)
let c4 = int 4
let c6 = int 6
let c8 = int 8
let b0 = Bil.int Word.b0
let b1 = Bil.int Word.b1
let x = Bil.var (Var.create "x" (Type.imm width))

let check exp expected ctxt =
  assert_equal ~ctxt ~cmp:Exp.equal (Exp.simpl ~ignore:[Eff.read] exp) expected

let (<=>) = check

let suite () =
  "Simplification" >::: [

    "0 + x = x"       >:: Bil.(c0 + x <=> x);
    "x + 0 = x"       >:: Bil.(x + c0 <=> x);
    "x - 0 = x"       >:: Bil.(x - c0 <=> x);
    "0 - x = -x"      >:: Bil.(c0 - x <=> unop neg x);
    "x - x = 0"       >:: Bil.(x - x <=> c0);
    "x * 0 = 0"       >:: Bil.(x * c0 <=> c0);
    "0 * x = 0"       >:: Bil.(c0 * x <=> c0);
    "x * 1 = x"       >:: Bil.(x * c1 <=> x);
    "1 * x = x"       >:: Bil.(c1 * x <=> x);
    "x / 1 = x"       >:: Bil.(x / c1 <=> x);
    "x /$ 1 = x"      >:: Bil.(x /$ c1 <=> x);
    "x mod 1 = 0"     >:: Bil.(x mod c1 <=> c0);
    "x smod 1 = 0"    >:: Bil.(x %$ c1 <=> c0);
    "x lsl 0 = x"     >:: Bil.(x lsl c0 <=> x);
    "x lsr 0 = x"     >:: Bil.(x lsr c0 <=> x);
    "x asr 0 = x"     >:: Bil.(x asr c0 <=> x);
    "0 lsl x = 0"     >:: Bil.(c0 lsl x <=> c0);
    "0 lsr x = 0"     >:: Bil.(c0 lsr x <=> c0);
    "0 asr x = 0"     >:: Bil.(c0 asr x <=> c0);
    "-1 asr x = -1"   >:: Bil.(_c1 asr x <=> _c1);

    "0 land x = 0"    >:: Bil.(c0 land x <=> c0);
    "x land 0 = 0"    >:: Bil.(x land c0 <=> c0);
    "x land -1 = x"   >:: Bil.(x land _c1 <=> x);
    "-1 land x = x"   >:: Bil.(_c1 land x <=> x);
    "x lor 0 = x"     >:: Bil.(x lor c0 <=> x);
    "0 lor x = x"     >:: Bil.(c0 lor x <=> x);
    "x lor -1 = -1"   >:: Bil.(x lor _c1 <=> _c1);
    "-1 lor x = -1"   >:: Bil.(_c1 lor x <=> _c1);
    "x lor x = x"     >:: Bil.(x lor x <=> x);
    "x lxor x = 0"    >:: Bil.(x lxor x <=> c0);
    "0 lxor x = x"    >:: Bil.(c0 lxor x <=> x);
    "x lxor 0 = x"    >:: Bil.(x lxor c0 <=> x);
    "(x = x) = 1"     >:: Bil.(x = x <=> b1);
    "(x <> x) = 0"    >:: Bil.(x <> x <=> b0);
    "(x < x) = 0"     >:: Bil.(x < x <=> b0);
    "(x <$ x) = 0"    >:: Bil.(x <$ x <=> b0);
    "(x <= x) = 1"    >:: Bil.(x <= x <=> b1);
    "(x <=$ x) = 1"   >:: Bil.(x <=$ x <=> b1);

    "2 * (x * 4) = 8 * x"          >:: Bil.(c2 * (x * c4) <=> c8 * x);
    "2 * (4 * x) = 8 * x"          >:: Bil.(c2 * (c4 * x) <=> c8 * x);
    "2 * (4 + x) = 8 + 2 * x"      >:: Bil.(c2 * (c4 + x) <=> c8 + c2 * x);
    "2 * (x + 4) = 2 * x + 8"      >:: Bil.(c2 * (x + c4) <=> c2 * x + c8);

    "2 + (4 + x) = 6 + x"          >:: Bil.(c2 + (c4 + x) <=> c6 + x);
    "2 + (x + 4) = 6 + x"          >:: Bil.(c2 + (x + c4) <=> c6 + x);
    "(4 + x) + 2 = 6 + x"          >:: Bil.(c2 + (c4 + x) <=> c6 + x);
    "(x + 4) + 2 = x + 6"          >:: Bil.((x + c4) + c2 <=> x + c6);

    "(x + 4) - 2 = x + 2"          >:: Bil.((x + c4) - c2 <=> x + c2);
    "(x - 4) + 2 = x - 2"          >:: Bil.((x - c4) + c2 <=> x + _c2);
    "2 + (x - 4) = x - 2"          >:: Bil.(c2 + (x - c4) <=> _c2 + x);
    "2 - (4 - x) = -2 + x"         >:: Bil.(c2 - (c4 - x) <=> _c2 + x);

    "2 - (x + 4) = -2 - x"         >:: Bil.(c2 - (x + c4) <=> _c2 - x);
    "(4 - x) - 2 = 2 - x"          >:: Bil.((c4 - x) - c2 <=> c2 - x);
    "(x - 4) - 2 = x - 6"          >:: Bil.((x - c4) - c2 <=> x - c6);
    "2 - (x - 4) = 6 - x"          >:: Bil.(c2 - (x - c4) <=> c6 - x);

    "(4 + x) * 2 = 8 + 2 * x"      >:: Bil.((c4 + x) * c2 <=> c8 + c2 * x);
    "(x + 4) * 2 = 2 * x + 8"      >:: Bil.((x + c4) * c2 <=> c2 * x + c8);
    "(x + 4) / 2 = (x + 4) / 2"    >:: Bil.((x + c4) / c2 <=> (x + c4) / c2);

    "2 / (x + 4) = 2 / (x + 4)"    >:: Bil.(c2 / (x + c4) <=> c2 / (x + c4));
    "2 + (4 << x) = 2 + (4 << x)"  >:: Bil.(c2 + (c4 lsl x) <=> c2 + (c4 lsl x));
    "2 * (4 << x) = 2 * (4 << tm)" >:: Bil.(c2 * (c4 lsl x) <=> c2 * (c4 lsl x));
    "(4 & x) * 2 = (4 & x) * 2"    >:: Bil.((c4 land x)*c2  <=> (c4 land x)*c2);

    "-1 = 0"                       >:: Bil.(lnot b1 <=> b0);
    "--1 = 1"                      >:: Bil.(lnot (lnot b1) <=> b1);
    "---1 = 0"                     >:: Bil.(lnot (lnot (lnot b1)) <=> b0);

    "plus, minus, times etc."      >:: random gen_binop ~width:32 ~times:200;
    "<, <=, =, <> etc."            >:: random gen_cmp ~width:1 ~times:200;
    "exps combinations"            >:: run_exps;
  ]
