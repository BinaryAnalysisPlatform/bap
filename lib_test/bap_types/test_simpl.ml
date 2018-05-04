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
let get_corner = int (get_elt [0; 1; -1])
let get_int = get_elt [int (random 5); get_corner]
let gen_binop ()  = get_elt binops
let gen_cmp   ()  = get_elt cmps
let with_unop e   = get_elt Bil.[unop neg; unop not; ident;] @@ e

let get_var width =
  Bil.var @@ Var.create ~is_virtual:true ~fresh:true "tmp" (Type.imm width)

let gen_exp make_binop width =
  let rec get_hs () = match random 3 with
    | 0 -> get_var width
    | 1 -> get_int width
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
      | init -> eprintf "%s\n" (Bil.to_string init) in
    eprintf "%s\n" (last src);
    eprintf "%s\n" (last simpl)

let same_bil b b' = Bil.compare b b' = 0

let simpl_bil bil =
  try Some (Stmt.simpl bil)
  with Division_by_zero -> None

let init width e =
  Set.fold (Exp.free_vars e) ~init:[]
    ~f:(fun bil v -> Bil.(v := get_int width) :: bil)

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

let run fn width times ctxt =
  let () = Random.self_init () in
  let calls = List.init times ~f:(fun _ -> run width (gen_exp fn)) in
  let fails = List.fold calls ~init:0
      ~f:(fun n -> function
          | Ok () -> n
          | Error (bil, simpl, res) ->
            report_fail bil simpl res;
            n + 1) in
  assert_equal  ~ctxt ~cmp:Int.equal 0 fails

let suite () =
  "Simplification" >::: [
    "plus, minus, times etc." >:: run gen_binop 32 100;
    "<, <=, =, <> etc."       >:: run gen_cmp 1 100;
  ]
