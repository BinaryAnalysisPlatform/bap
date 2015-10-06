open Core_kernel.Std
open Bap.Std
open OUnit2

let x = Var.create "x" bool_t
let y = Var.create "y" bool_t
let z = Var.create "z" bool_t
let o = Var.create "o" bool_t
let s = Var.create "s" bool_t
let p = Var.create "p" bool_t
let q = Var.create "q" bool_t
let r = Var.create "r" bool_t
let a = Var.create "a" bool_t
let b = Var.create "b" bool_t
let c = Var.create "c" bool_t
let u = Var.create "arg_1" bool_t
let v = Var.create "arg_2" bool_t


let def_x = Def.create x (Bil.(int Word.b1))
let def_y = Def.create y (Bil.(int Word.b0))
let def_z = Def.create z (Bil.(int Word.b1))
let def_o = Def.create o (Bil.(int Word.b0))
let def_s = Def.create s (Bil.(var o land var z))
let arg_1 = Arg.create u (Bil.(int Word.b1))
let arg_2 = Arg.create v (Bil.(int Word.b0))
let arg_3 = Term.clone arg_1
let arg_4 = Term.clone arg_2

let def_p = Def.with_lhs (Term.clone def_x) p
let def_q = Def.with_lhs (Term.clone def_y) q
let def_r = Def.with_lhs (Term.clone def_z) r
let def_a = Def.with_lhs (Term.clone def_x) a
let def_b = Def.with_lhs (Term.clone def_y) b
let def_c = Def.with_lhs (Term.clone def_z) c

let xid = Term.tid def_x
let yid = Term.tid def_y
let zid = Term.tid def_z
let sid = Term.tid def_s

let block_of_defs defs =
  let b = Blk.Builder.create ~defs:(List.length defs) () in
  List.iter defs ~f:(Blk.Builder.add_def b);
  Blk.Builder.result b

let xyz = block_of_defs [def_x; def_y; def_z]
let pqr = block_of_defs [def_p; def_q; def_r]
let abc = block_of_defs [def_a; def_b; def_c]

let xyoz = Term.prepend def_t ~before:zid xyz def_o
let oxyz = Term.prepend def_t ~before:xid xyz def_o
let xyzo = Term.append  def_t ~after:zid  xyz def_o
let xoyz = Term.append  def_t ~after:xid  xyz def_o
let xsyz = Term.prepend def_t ~before:yid xyz def_s
let xysz = Term.append  def_t ~after:yid  xyz def_s
let xyzs = Term.append  def_t xyz def_s
let sxyz = Term.prepend def_t xyz def_s

let xyoz_label = Label.direct Term.(tid xyoz)
let xysz_label = Label.direct Term.(tid xysz)
let xyz_label = Label.direct Term.(tid xyz)
let pqr_label = Label.direct Term.(tid pqr)


let make_sub name args blks : sub term =
  let b = Sub.Builder.create ~name () in
  List.iter args ~f:(Sub.Builder.add_arg b);
  List.iter blks ~f:(Sub.Builder.add_blk b);
  Sub.Builder.result b

let abc = Term.append jmp_t abc
    (Jmp.create_ret (Label.indirect Bil.(var ARM.CPU.lr)))

let sub1 = make_sub "f" [arg_1;arg_2] [abc]
let sub1_label = Label.direct Term.(tid sub1)
let goto_xyz =
  Jmp.create_goto ~cond:Bil.(var s = var @@ Arg.lhs arg_1) xyz_label
let call_sub1 =
  let call = Call.create ~return:pqr_label ~target:sub1_label () in
  Jmp.create_call call

let xyz = Term.append jmp_t xyz call_sub1
let pqr = Term.append jmp_t pqr goto_xyz


let sub2 = make_sub "g" [arg_3; arg_4] [xyz; pqr]

let program =
  Term.append sub_t
    (Term.append sub_t (Program.create ()) sub1) sub2

let equal_var ctxt x y =
  assert_equal ~ctxt
    ~cmp:Var.equal
    ~printer:Var.to_string
    x y

let assert_vars get xs blk ctxt =
  let ys = Seq.(get def_t blk >>| Def.lhs |> to_list) in
  List.iter2_exn xs ys ~f:(equal_var ctxt)

let def_order = assert_vars Term.to_sequence

let seq f ?rev def =
  assert_vars (fun t blk -> Term.(f t ?rev blk (tid def)))

let look which z id blk ctxt =
  match z, which def_t xyz id with
  | Some z, Some x -> equal_var ctxt z (Def.lhs x)
  | None, Some _ -> assert_string "unexpected neighbour"
  | Some _, None -> assert_string "neighbour wasn't found"
  | None, None -> ()

let transform g f xs v blk ctxt =
  let blk = g def_t blk ~f:(fun d -> f v d) in
  def_order xs blk ctxt

let filter_var = transform Term.filter (fun v d -> Var.(Def.lhs d <> v))
let triplicate = transform Term.concat_map (fun v d ->
    if Var.(Def.lhs d = v) then [d;d;d] else [d])
let remove_with_nil = transform Term.concat_map (fun v d ->
    if Var.(Def.lhs d = v) then [] else [d])
let remove_with_none = transform Term.filter_map (fun v d ->
    if Var.(Def.lhs d = v) then None else Some d)
let rename_to_s = transform Term.map (fun v d ->
    if Var.(Def.lhs d = v) then def_s else d)

let update_to_s xs def blk ctxt =
  let blk = Term.update def_t blk (Def.with_lhs def s) in
  def_order xs blk ctxt

let split split xs ys blk ctxt =
  let b1,b2 = split blk in
  def_order xs b1 ctxt;
  def_order ys b2 ctxt

let after_def def blk = Blk.split_after blk def
let before_def def blk = Blk.split_before blk def
let top = Blk.split_top
let bot = Blk.split_bot

let lookup cls hay ctxt =
  match Program.lookup cls program (Term.tid hay) with
  | Some t -> assert_bool "Found wrong" (Term.same hay t)
  | None -> assert_string "Not_found"

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

  let in_set vars set = List.for_all vars ~f:(Set.mem set)

  let free_vars free expect blk _ =
    let free = free blk in
    assert_bool (Term.name blk) @@ in_set expect free

  let blk_free_vars = free_vars Blk.free_vars
  let sub_free_vars = free_vars Sub.free_vars

  let check_free_vars = "free_vars" >::: [
      "entry" >:: blk_free_vars [] entry;
      "b1"    >:: blk_free_vars [] b1;
      "b2"    >:: blk_free_vars [i;j] b2;
      "b3"    >:: blk_free_vars [i] b3;
      "b4"    >:: blk_free_vars [k] b4;
      "b5"    >:: blk_free_vars [j] b5;
      "b6"    >:: blk_free_vars [i] b6;
      "exit"  >:: blk_free_vars [] exit;
      "sub"   >:: sub_free_vars [] sub;
    ]


  let phi_node sub var var_ver blk blk_ver ctxt =
    Term.enum blk_t sub |> Seq.find_map ~f:(fun blk ->
        Term.enum phi_t blk |> Seq.find ~f:(fun phi ->
            let v = Phi.lhs phi in
            Var.same v var && Var.version v = var_ver)) |> function
    | None -> assert_string "no such phi-node"
    | Some phi -> Phi.values phi |> Seq.find ~f:(fun (tid,exp) ->
        Term.tid blk = tid && match exp with
        | Bil.Var v -> Var.version v = blk_ver
        | _ -> assert false) |> function
                  | None -> assert_string "wrong phi-node"
                  | Some _ -> ()

  let phi_node = phi_node (Sub.ssa sub)

  let check_phi_nodes = "phi nodes" >::: [
      "i.2=i.1" >:: phi_node i 2 b1 1;
      "i.2=i.3" >:: phi_node i 2 b3 3;
      "j.2=j.1" >:: phi_node j 2 b1 1;
      "j.2=j.3" >:: phi_node j 2 b3 3;
      "k.2=k.1" >:: phi_node k 2 b1 1;
      "k.2=k.3" >:: phi_node k 2 b3 3;
      "i.5=i.2" >:: phi_node i 5 b5 2;
      "i.5=i.4" >:: phi_node i 5 b6 4;
    ]

  let tests = [
    check_free_vars;
    check_phi_nodes;
  ]
end


open Term
let suite = "Sema.IR" >::: [
    "order(xyoz)" >:: def_order [x;y;o;z] xyoz;
    "order(oxyz)" >:: def_order [o;x;y;z] oxyz;
    "order(xyzo)" >:: def_order [x;y;z;o] xyzo;
    "order(xoyz)" >:: def_order [x;o;y;z] xoyz;
    "order(xsyz)" >:: def_order [x;s;y;z] xsyz;
    "order(xysz)" >:: def_order [x;y;s;z] xysz;
    "order(sxyz)" >:: def_order [s;x;y;z] sxyz;
    "order(xyzs)" >:: def_order [x;y;z;s] xyzs;
    "after(Xyzs)" >:: seq after def_x [y;z;s] xyzs;
    "after(xYzs)" >:: seq after def_y   [z;s] xyzs;
    "after(xyZs)" >:: seq after def_z     [s] xyzs;
    "after(xyzS)" >:: seq after def_s      [] xyzs;
    "terfa(Xyzs)" >:: seq after ~rev:true def_x [s;z;y] xyzs;
    "terfa(xYzs)" >:: seq after ~rev:true def_y   [s;z] xyzs;
    "terfa(xyZs)" >:: seq after ~rev:true def_z     [s] xyzs;
    "terfa(xyzS)" >:: seq after ~rev:true def_s      [] xyzs;
    "before(Xyzs)" >:: seq before def_x [] xyzs;
    "before(xYzs)" >:: seq before def_y [x] xyzs;
    "before(xyZs)" >:: seq before def_z [x;y] xyzs;
    "before(xyzS)" >:: seq before def_s [x;y;z] xyzs;
    "erofeb(Xyzs)" >:: seq before ~rev:true def_x [] xyzs;
    "erofeb(xYzs)" >:: seq before ~rev:true def_y [x] xyzs;
    "erofeb(xyZs)" >:: seq before ~rev:true def_z [y;x] xyzs;
    "erofeb(xyzS)" >:: seq before ~rev:true def_s [z;y;x] xyzs;
    "after(xyzs)" >:: seq after def_o      [] xyzs;
    "terfa(xyzs)" >:: seq after ~rev:true def_o [] xyzs;
    "before(xyzs)" >:: seq before def_o [] xyzs;
    "erofeb(xyzs)" >:: seq before ~rev:true def_o [] xyzs;
    "next(Xyz)" >:: look next (Some y) xid xyz;
    "next(xYz)" >:: look next (Some z) yid xyz;
    "next(xyZ)" >:: look next None     zid xyz;
    "next(xyz)" >:: look next None     sid xyz;
    "prev(Xyz)" >:: look prev None     xid xyz;
    "prev(xYz)" >:: look prev (Some x) yid xyz;
    "prev(xyZ)" >:: look prev (Some y) zid xyz;
    "prev(xyz)" >:: look prev None     sid xyz;
    "filter(xyoz)" >:: filter_var [x;y;z] o xyoz;
    "triplicate(xyoz)" >:: triplicate [x;y;o;o;o;z] o xyoz;
    "remove_nil(xyoz)" >:: remove_with_nil [x;y;z] o xyoz;
    "remove_none(xyoz)" >:: remove_with_none [x;y;z] o xyoz;
    "rename_to_s(xyoz)" >:: rename_to_s [x;y;s;z] o xyoz;
    "update_to_s(Xyoz)" >:: update_to_s [s;y;o;z] def_x xyoz;
    "update_to_s(xyOz)" >:: update_to_s [x;y;s;z] def_o xyoz;
    "find(Xyz)" >:: look find (Some x) xid xyz;
    "find(xYz)" >:: look find (Some y) yid xyz;
    "find(xyZ)" >:: look find (Some z) zid xyz;
    "split_after(xyOz)"  >:: split (after_def def_o)  [x;y;o] [z] xyoz;
    "split_before(xyOz)" >:: split (before_def def_o) [x;y] [o;z] xyoz;
    "split_after(xyoZ)"  >:: split (after_def def_z)  [x;y;o;z] [] xyoz;
    "split_before(xyoZ)" >:: split (before_def def_z) [x;y;o] [z] xyoz;
    "split_after(Xyoz)"  >:: split (after_def def_x)  [x] [y;o;z] xyoz;
    "split_before(Xyoz)" >:: split (before_def def_x) [] [x;y;o;z] xyoz;
    "split_after(xyoz)"  >:: split (after_def def_s)  [x;y;o;z] [] xyoz;
    "split_top(xyoz)" >:: split top [] [x;y;o;z]  xyoz;
    "split_bot(xyoz)" >:: split bot [x;y;o;z] [] xyoz;
    "lookup(arg1)" >:: lookup arg_t arg_1;
    "lookup(arg2)" >:: lookup arg_t arg_2;
    "lookup(arg2)" >:: lookup arg_t arg_2;
    "lookup(arg3)" >:: lookup arg_t arg_3;
    "lookup(arg4)" >:: lookup arg_t arg_4;
    "lookup(a)" >:: lookup def_t def_a;
    "lookup(y)" >:: lookup def_t def_y;
    "lookup(r)" >:: lookup def_t def_r;
    "lookup(goto_xyz)" >:: lookup jmp_t goto_xyz;
    "lookup(call_xyz)" >:: lookup jmp_t call_sub1;
  ] @ Example.tests
