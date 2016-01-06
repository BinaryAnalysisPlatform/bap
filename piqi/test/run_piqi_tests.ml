open OUnit2
open Core_kernel.Std
open Bap.Std

module Example = struct
  let entry = Blk.create ()
  let b1 = Blk.create ()
  let b2 = Blk.create ()
  let b3 = Blk.create ()
  let b4 = Blk.create ()
  let b5 = Blk.create ()
  let b6 = Blk.create ()
  let exit = Blk.create ()

  let i = Var.create "i" reg32_t
  let j = Var.create "j" reg32_t
  let k = Var.create "k" bool_t

  let _1 = Bil.int (Word.one 32)
  let _2 = Bil.int (Word.of_int32 2l)
  let _F = Bil.int Word.b0
  let _T = Bil.int Word.b1

  let def var exp b = Term.append def_t b @@ Def.create var exp
  let cond blk cond t f =
    let jt = Jmp.create_goto ~cond (Label.direct (Term.tid t)) in
    let jf = Jmp.create_goto (Label.direct (Term.tid f)) in
    let blk = Term.append jmp_t blk jt in
    Term.append jmp_t blk jf
  let goto dst src = Term.append jmp_t src @@
    Jmp.create_goto (Label.direct (Term.tid dst))

  let entry = entry |>
              goto b1

  let b1 = b1       |>
           def k _F |>
           def i _1 |>
           def j _2 |>
           goto b2

  let b2 = cond b2 Bil.(var i <= var j) b3 b4

  let b3 = b3                     |>
           def j Bil.(var j * _2) |>
           def k _T               |>
           def i Bil.(var i + _1) |>
           goto b2

  let b3_without_jumps = Term.filter jmp_t ~f:(fun _ -> false) b3

  let b4 = cond b4 Bil.(var k) b5 b6

  let b5 =
    let call = Call.create ()
        ~return:(Label.direct (Term.tid exit))
        ~target:(Label.indirect (Bil.var j)) in
    let use = Jmp.create_call call in
    Term.append jmp_t b5 use

  let b3_with_jump_to_b5 = Term.map jmp_t ~f:(fun _ ->
      Jmp.create_goto (Label.direct (Term.tid b5))) b3



  let b6 = def i Bil.(var i + _1) b6 |> goto exit

  let sub_of_blk blks =
    let sub = Sub.create ~name:"example" () in
    List.fold blks ~init:sub ~f:(Term.append blk_t)

  let blks = [entry; b1; b2; b3; b4; b5; b6; exit]
  let sub = sub_of_blk blks
  let program =
    Term.append sub_t (Program.create ()) sub
end

let test fmt ctxt =
  let prog = Example.program |>
             Bir_piqi.string_of_program fmt |>
             Bir_piqi.program_of_string fmt in
  let cmp = Program.equal in
  let printer = Program.to_string in
  assert_equal ~ctxt ~cmp ~printer Example.program prog



let suite =
  let open Bil.Types in
  "bir" >:::
  [
    "json" >:: test `json;
    "pb" >:: test `pb;
    "piq" >:: test `piq;
    "pib" >:: test `pib;
    "xml" >:: test `xml;
  ]


let () = run_test_tt_main suite
