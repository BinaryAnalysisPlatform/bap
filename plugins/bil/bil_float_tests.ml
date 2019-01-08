open Core_kernel
open OUnit2
open Bap_plugins.Std
open Bap_primus.Std
open Bap.Std
open Monads.Std
open Bap_knowledge
open Bap_core_theory

module G = Bil_float.Make(Theory.Manager)

[@@@warning "-3"]

let () = Plugins.run ~provides:["bil"] ()


let enum_bits w =
  let bits = Word.(enum_bits w BigEndian) in
  let b_len = Seq.length bits in
  let w_len = Word.bitwidth w in
  if b_len > w_len then
    Seq.drop bits (b_len - w_len)
  else bits

let float_bits x =
  let w = Word.of_int64 (Int64.bits_of_float x) in
  let bits = enum_bits w in
  let (@@) = sprintf "%s%d" in
  Seq.foldi bits ~init:"" ~f:(fun i acc x ->
      let a =
        if i = 1 || i = 12 then "_"
        else "" in
      let s = sprintf "%s%s" acc a in
      if x then s @@ 1
      else s @@ 0)

let deconstruct x =
  let wi = Word.to_int_exn in
  let y = Int64.bits_of_float x in
  let w = Word.of_int64 y in
  let expn = Word.extract_exn ~hi:62 ~lo:52 w in
  let bias = Word.of_int ~width:11 1023 in
  let expn' = Word.(signed (expn - bias)) in
  let frac = Word.extract_exn ~hi:51 w in
  printf "ocaml %f: bits %s, 0x%LX\n" x (float_bits x) y;
  printf "ocaml %f: biased/unbiased expn %d/%d, coef 0x%x\n"
    x (wi expn) (wi expn') (wi frac)


type bits11
type bits53
type bits64

type binop = [
  | `Add
  | `Sub
  | `Mul
  | `Div
] [@@deriving sexp]

type unop = [
  | `Sqrt
] [@@deriving sexp]

type cast_int = [
  | `Of_uint
  | `Of_sint
] [@@deriving sexp]

type cast_float = [
  | `Of_float
] [@@deriving sexp]

type test = [
    binop | cast_int | cast_float
] [@@deriving sexp]

let test_name op = Sexp.to_string (sexp_of_test (op :> test))


module Machine = struct
  type 'a m = 'a
  include Primus.Machine.Make(Monad.Ident)
end
module Main = Primus.Machine.Main(Machine)
module Eval = Primus.Interpreter.Make(Machine)

let proj =
  let nil = Memmap.empty in
  Project.Input.create `x86_64 "/bin/true" ~code:nil ~data:nil |>
  Project.create |>
  ok_exn

let word_of_float x = Word.of_int64 (Int64.bits_of_float x)
let float_of_word x = Int64.float_of_bits (Word.to_int64_exn x)

let exp x =
  let open Knowledge.Syntax in
  let x = x >>| Value.semantics in
  match Knowledge.run x Knowledge.empty with
  | Error _ -> assert false
  | Ok (s,_) -> Semantics.get Bil.Domain.exp s

let eval ?(name="") ~expected test _ctxt =
  let open Machine.Syntax in
  let float_bits w =
    let x = Word.signed w |> Word.to_int64_exn in
    float_bits (Int64.float_of_bits x) in
  match exp test with
  | None -> assert false
  | Some e ->
     let check =
       Eval.exp e >>| fun r ->
       let r = Primus.Value.to_word r in
       let equal = Word.(r = expected) in
       if not equal then
         let () = printf "\n FAIL %s\n" name in
         let () = printf "expected: %s\n" (float_bits expected) in
         printf "got     : %s\n" (float_bits r);
       assert_bool name equal in
     match Main.run proj check with
     | Primus.Normal,_ -> ()
     | _ -> raise (Failure "Something went wrong")

let exps : bits11 bitv sort = Bits.define 11
let sigs : bits53 bitv sort = Bits.define 53
let bitv : bits64 bitv sort = Bits.define 64
let fsort : ((int,bits11,bits53) IEEE754.ieee754,'s) format float sort = IEEE754.(Sort.define binary64)

let knowledge_of_word sort w = Theory.Manager.int sort w

let knowledge_of_float x = knowledge_of_word bitv (word_of_float x)

let gfloat_of_int x =
  let bits = Word.of_int ~width:64 x in
  knowledge_of_word bitv bits

let binop op x y ctxt =
  let bits = Int64.bits_of_float in
  let name = sprintf "%Lx %s %Lx\n" (bits x) (test_name op) (bits y) in
  let real, op = match op with
    | `Add -> x +. y, G.fadd
    | `Sub -> x -. y, G.fsub
    | `Mul -> x *. y, G.fmul
    | `Div -> x /. y, G.fdiv in
  let test = op fsort G.rne (knowledge_of_float x) (knowledge_of_float y) in
  eval ~name ~expected:(word_of_float real) test ctxt

let cast_int cast x ctxt =
  let name = sprintf "%s %d\n" (test_name cast) x in
  let expected = word_of_float (float x) in
  let op = match cast with
    | `Of_uint -> G.cast_float
    | `Of_sint -> G.cast_float_signed in
  let test = op fsort G.rne (gfloat_of_int x) in
  eval ~name ~expected test ctxt

let cast_float x ctxt =
  let name = sprintf "%s %g\n" (test_name `Of_float) x in
  let expected = Word.of_int ~width:64 (int_of_float x) in
  let test = G.cast_int fsort bitv (knowledge_of_float x) in
  eval ~name ~expected test ctxt

let sqrt_exp x ctxt =
  let name = sprintf "sqrt %g\n" x in
  let expected = Float.sqrt x |> word_of_float in
  let x = Theory.Manager.var (Var.define bitv "x") in
  let test = G.fsqrt fsort G.rne x in
  eval ~name ~expected test ctxt

let sqrt_ x ctxt =
  let name = sprintf "sqrt %g %Lx\n" x (Int64.bits_of_float x) in
  let expected = Float.sqrt x |> word_of_float in
  let test = G.fsqrt fsort G.rne (knowledge_of_float x) in
  eval ~name ~expected test ctxt

let ( + ) = binop `Add
let ( - ) = binop `Sub
let ( * ) = binop `Mul
let ( / ) = binop `Div

let of_uint = cast_int `Of_uint
let of_sint = cast_int `Of_sint
let to_int  = cast_float

let make_float s e c =
  let s = Word.of_int ~width:1 s in
  let e = Word.of_int ~width:11 e in
  let c = Word.of_int ~width:52 c in
  let w = Word.(concat (concat s e) c) in
  Word.signed w |> Word.to_int64_exn |> Int64.float_of_bits

let neg x = ~-. x
let nan = Float.nan
let inf = Float.infinity
let ninf = Float.neg_infinity
let smallest_nonzero = make_float 0 0 1
let some_small = make_float 0 0 2
let biggest_subnormal = make_float 0 0 0xFFFF_FFFF_FFFF_F
let smallest_normal = Float.(biggest_subnormal + smallest_nonzero)
let biggest_normal = make_float 0 2046 0xFFFF_FFFF_FFFF_F

let () = Random.self_init ()

let random = Random.int
let random_elt xs = List.nth_exn xs @@ random (List.length xs)

let random_int ~from ~to_ =
  let open Caml in
  let max = to_ - from in
  let x = random max in
  x + from

let random_float () =
  let expn () = random_int ~from:0 ~to_:2046 in
  let frac () = Random.int 0xFFFFFFFFFFFFF in
  let sign () = Random.int 2 in
  let make () =
    let expn = expn () in
    let frac = frac () in
    make_float (sign ()) expn frac in
  let small () =
    let x = Random.int 42 in
    let y = Int64.of_int x in
    Random.float (Int64.float_of_bits y) in
  random_elt [make (); make (); small (); make (); make (); small (); make ()]

let random_floats ~times ops =
  List.init times ~f:(fun i ->
      let f =
        match random_elt ops with
        | `Sqrt ->
           let x = Float.abs @@ random_float () in
           fun (ctxt : test_ctxt) -> sqrt_ x ctxt
        | `Add | `Sub | `Mul | `Div as op ->
           let x = random_float () in
           let y = random_float () in
           fun ctxt -> binop op x y ctxt in
      (sprintf "random%d" i) >:: f)


let of_bits = Int64.float_of_bits

let suite () =

  "Gfloat" >::: [

      (* of uint *)
      "of uint 42" >:: of_uint 42;
      "of uint 0"  >:: of_uint 0;
      "of uint 1"  >:: of_uint 1;
      "of uint 2"  >:: of_uint 2;
      "of uint 10" >:: of_uint 10;
      "of uint 13213" >:: of_uint 13213;
      "of uint 45676" >:: of_uint 45667;
      "of uint 98236723" >:: of_uint 98236723;
      "of uint 0xFFFF_FFFF_FFFF_FFF" >:: of_uint 0xFFFF_FFFF_FFFF_FFF;

      (* of sint *)
      "of sint -42" >:: of_sint (-42);
      "of sint 0"   >:: of_sint 0;
      "of sint -1"  >:: of_sint 1;
      "of sint -2"  >:: of_sint (-2);
      "of sint -10" >:: of_sint (-10);
      "of sint -13213" >:: of_sint (-13213);
      "of sint -45676" >:: of_sint (-45667);
      "of sint -98236723" >:: of_sint (-98236723);

      (* to int *)
      "to int 42.42" >:: to_int 42.42;
      "to int 0.42"  >:: to_int 0.42;
      "to int 0.99999999999" >:: to_int 0.99999999999;
      "to int 13123120.98882344542" >:: to_int 13123120.98882344542;
      "to int -42.42" >:: to_int (-42.42);
      "to int -13123120.98882344542" >:: to_int (-13123120.98882344542);

      (* add *)
      "0.0 + 0.5"     >:: 0.0 + 0.5;
      "4.2 + 2.3"     >:: 4.2 + 2.3;
      "4.2 + 2.98"    >:: 4.2 + 2.98;
      "2.2 + 4.28"    >:: 2.2 + 4.28;
      "2.2 + 2.46"    >:: 2.2 + 2.46;
      "2.2 + -4.28"   >:: 2.2 + (neg 4.28);
      "-2.2 + 4.28"   >:: (neg 2.2) + 4.28;
      "0.0000001 + 0.00000002" >:: 0.0000001 + 0.00000002;
      "123213123.23434 + 56757.05656549151" >:: 123213123.23434 + 56757.05656549151;
      "nan  + nan"    >:: nan  + nan;
      "inf  + inf"    >:: inf  + inf;
      "-inf + -inf"   >:: ninf + ninf;
      "nan  + -inf"   >:: nan  + ninf;
      "-inf + nan"    >:: ninf + nan;
      "nan  + inf"    >:: nan  + inf;
      "inf  + nan"    >:: inf  + nan;
      "-inf + inf"    >:: ninf + inf;
      "inf  + -inf"   >:: inf  + ninf;
      "0.0 + small"   >:: 0.0 + smallest_nonzero;
      "small + small" >:: smallest_nonzero + some_small;
      "biggest_sub + small"  >:: biggest_subnormal + smallest_nonzero;
      "biggest_normal + small"  >:: biggest_normal + smallest_nonzero;
      "biggest_normal + biggest_subnorm"  >:: biggest_normal + biggest_subnormal;
      "near inf case" >:: make_float 0 2046 0xFFFF_FFFF_FFFF_FFF + make_float 0 2046 1;

      (* sub *)
      "4.2 - 2.28"    >:: 4.2 - 2.28;
      "4.28 - 2.2"    >:: 4.28 - 2.2;
      "2.2 - 4.28"    >:: 2.2 - 4.28;
      "2.2 - 2.6"     >:: 2.2 - 2.6;
      "0.0 - 0.0"     >:: 0.0 - 0.0;
      "4.2 - 4.2"     >:: 4.2 - 4.2;
      "2.2 - -4.28"   >:: 2.2 - (neg 4.28);
      "-2.2 - 2.46"   >:: (neg 2.2) - 2.46;
      "-2.2 - -2.46"  >:: (neg 2.2) - (neg 2.46);
      "0.0000001 - 0.00000002" >:: 0.0000001 - 0.00000002;
      "0.0 - 0.00000001"       >:: 0.0 - 0.0000001;
      "123213123.23434 - 56757.05656549151" >:: 123213123.23434 - 56757.05656549151;
      "nan  - nan"    >:: nan  - nan;
      "inf  - inf"    >:: inf  - inf;
      "-inf - -inf"   >:: ninf - ninf;
      "nan  - -inf"   >:: nan  - ninf;
      "-inf - nan"    >:: ninf - nan;
      "nan  - inf"    >:: nan  - inf;
      "inf  - nan"    >:: inf  - nan;
      "-inf - inf"    >:: ninf - inf;
      "inf  - -inf"   >:: inf  - ninf;
      "0.0 - small"   >:: 0.0 - smallest_nonzero;
      "small - 0.0"   >:: smallest_nonzero - 0.0;
      "small - small"  >:: smallest_nonzero - smallest_nonzero;
      "small - small'" >:: smallest_nonzero - some_small;
      "small' - small" >:: some_small - smallest_nonzero;
      "smalles_norm - small" >:: smallest_normal - smallest_nonzero;
      "biggest_sub - small"   >:: biggest_subnormal - smallest_nonzero;
      "biggest_normal - small"  >:: biggest_normal - smallest_nonzero;
      "biggest_normal - biggest_subnorm"  >:: biggest_normal - biggest_subnormal;
      "biggest_subnorm - biggest_normal"  >:: biggest_subnormal - biggest_normal;
      "near inf case" >:: make_float 1 2046 0xFFFF_FFFF_FFFF_FFF - make_float 0 2046 1;

      (* mul *)
      "1.0 * 2.5"    >:: 1.0 * 2.5;
      "2.5 * 0.5"    >:: 2.5 * 0.5;
      "4.2 * 3.4"    >:: 4.2 * 3.4;
      "0.01 * 0.02"  >:: 0.01 * 0.02;
      "1.0 * 0.5"    >:: 1.0 * 0.5;
      "1.0 * -0.5"   >:: 1.0 * (neg 0.5);
      "- 1.0 * -0.5" >:: (neg 1.0) * (neg 0.5);
      "123734.86124324198 * 23967986786.4834517" >:: 123734.86124324198 * 23967986786.4834517;
      "nan  * nan"    >:: nan  * nan;
      "inf  * inf"    >:: inf  * inf;
      "-inf * -inf"   >:: ninf * ninf;
      "nan  * -inf"   >:: nan  * ninf;
      "-inf * nan"    >:: ninf * nan;
      "nan  * inf"    >:: nan  * inf;
      "inf  * nan"    >:: inf  * nan;
      "-inf * inf"    >:: ninf * inf;
      "inf  * -inf"   >:: inf  * ninf;
      "0.0 * big"     >:: 0.0 * biggest_normal;
      "0.0 * small"   >:: 0.0 * biggest_subnormal;
      "0.0 * small'"  >:: 0.0 * smallest_nonzero;
      "2.0 * small"  >:: 2.0 * smallest_nonzero;
      "1123131.45355 * small"  >:: 1123131.45355 * smallest_nonzero;
      "small * small" >:: smallest_nonzero * some_small;
      "smallest normal * small"    >:: smallest_normal * smallest_nonzero;
      "biggest subnormal * small"     >:: biggest_subnormal * smallest_nonzero;
      "biggest normal * small"  >:: biggest_normal * smallest_nonzero;
      "biggest normal * 2.0"    >:: biggest_normal * 2.0;
      "biggest normal * biggest subnormal"  >:: biggest_normal * biggest_subnormal;
      "biggest subnormal * small" >:: biggest_subnormal * smallest_nonzero;
      "biggest subnormal * biggest subnormal" >:: biggest_subnormal *  biggest_subnormal;
      "biggest normal * biggest normal" >:: biggest_normal *  biggest_normal;
      "test with underflow" >:: of_bits 974381688320862858L * of_bits (-5590604654947855237L);
      "test1" >:: of_bits 0xec9059c2619517d5L + of_bits 0x6c52387cdb6aefadL;
      "test2" >:: of_bits 0xa10d89faaef35527L - of_bits 0xa130e0fee63e0e6fL;
      "test3" >:: of_bits 0x400199999999999aL - of_bits 0x4004cccccccccccdL;
      "test4" >:: of_bits 0x7fefffffffffffffL - of_bits 0xfffffffffffffL;

      (* div *)
      "2.0 / 0.5"   >:: 2.0 / 0.5;
      "1.0 / 3.0"   >:: 1.0 / 3.0;
      "3.0 / 32.0"  >:: 3.0 / 32.0;
      "324.32423 / 1.2" >:: 324.32423 / 1.2;
      "2.4 / 3.123131"  >:: 2.4 / 3.123131;
      "0.1313134 / 0.578465631" >:: 0.1313134 / 0.578465631;
      "9991132.2131363434 / 2435.05656549153" >:: 9991132.2131363434 / 2435.05656549153;
      "nan  / nan"    >:: nan  / nan;
      "inf  / inf"    >:: inf  / inf;
      "-inf / -inf"   >:: ninf / ninf;
      "nan  / -inf"   >:: nan  / ninf;
      "-inf / nan"    >:: ninf / nan;
      "nan  / inf"    >:: nan  / inf;
      "inf  / nan"    >:: inf  / nan;
      "-inf / inf"    >:: ninf / inf;
      "inf  / -inf"   >:: inf  / ninf;
      "0.0  / small"  >:: 0.0 / smallest_nonzero;
      "small  / small'" >:: smallest_nonzero / some_small;
      "small' / small" >:: some_small / smallest_nonzero;
      "small  / small" >:: smallest_nonzero / smallest_nonzero;
      "smallest_norm / small" >:: smallest_normal / smallest_nonzero;
      "biggest_sub / small"   >:: biggest_subnormal / smallest_nonzero;
      "biggest_normal / small"  >:: biggest_normal / smallest_nonzero;
      "biggest_normal / biggest_subnorm"  >:: biggest_normal / biggest_subnormal;
      "biggest_normal / smallest_normal"  >:: biggest_normal / smallest_normal;

] @ random_floats ~times:50000 [`Add; `Sub; `Mul; `Div]

let _suite () =

  "Gfloat" >::: [
      "test1" >:: of_bits 0xec9059c2619517d5L + of_bits 0x6c52387cdb6aefadL;
      "test2" >:: of_bits 0xcf55560fac913244L + of_bits 0xcc0c60b10442b9bfL;
      "test3" >:: of_bits 0x95a1b73736807a27L + of_bits 0x92587d2dcebfa014L;
      "test4" >:: of_bits 0x290dc46c1cff15fdL + of_bits 0x2c5ae8bbac4b6065L;
      "test5" >:: of_bits 0xa10d89faaef35527L - of_bits 0xa130e0fee63e0e6fL;
      "test6" >:: of_bits 0x400199999999999aL - of_bits 0x4004cccccccccccdL;
      "test7" >:: of_bits 0x7fefffffffffffffL - of_bits 0xfffffffffffffL;
      "test8" >:: of_bits 0x419d60550ceff6d3L - of_bits 0x40ebb6a1cf626f04L;
      "test9"  >:: of_bits 0x1068a846325c1af9L - of_bits 0x8d1317a5d5fcc64aL;
      "test10" >:: of_bits 0x23a935d41c3874ffL - of_bits 0xa6f2c3867f5ab404L;
      "test11" >:: of_bits 0xc311e2fd9d831929L - of_bits 0x46680df05fa67db8L;
      "test12" >:: of_bits 0x9f1eb84311212bc5L - of_bits 0x2262d23b1d5380d0L;

      "test13" >:: of_bits 0x3L / of_bits 0x4009dbbfb8e40a2aL;
      "test14" >:: of_bits 0x20L / of_bits 0xbd5ce754a116cb3aL;
      "test15" >:: of_bits 0xdL / of_bits 0x3cfff734e5abf313L;
      "test16" >:: of_bits 0x7L / of_bits 0x401f19b00a01a8dcL;
      "test17" >:: of_bits 0x838fa41971b1d4abL / of_bits 0xc39274e6f88b913bL;
      "test18" >:: of_bits 0x7L / of_bits 0x3d0e5becc4792655L;
      "test19" >:: of_bits 0x1L / of_bits 0xbff1b827c706d47cL;
      "test20" >:: of_bits 0x61f141df4f4486fL / of_bits 0x461865ee4b18b233L;
      "test21" >:: of_bits 0xe9d4c54247f233L / of_bits 0xc0db8ed1f3ba2817L;
    ]

let _suite () = "Gfloat" >::: random_floats ~times:10 [`Sqrt]

let () = run_test_tt_main (suite ())
