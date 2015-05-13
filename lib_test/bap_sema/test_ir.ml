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


let def_x = Def.create x (Bil.(int Word.b1))
let def_y = Def.create y (Bil.(int Word.b0))
let def_z = Def.create z (Bil.(int Word.b1))
let def_o = Def.create o (Bil.(int Word.b0))
let def_s = Def.create s (Bil.(var o land var z))
let arg_1 = Arg.create ~name:"arg_1" bool_t
let arg_2 = Arg.create ~name:"arg_2" bool_t
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
    (Jmp.create_goto (Label.indirect Bil.(var ARM.CPU.lr)))

let sub1 = make_sub "f" [arg_1;arg_2] [abc]
let sub1_label = Label.direct Term.(tid sub1)
let goto_xyz =
  Jmp.create_goto ~cond:Bil.(var s = var @@ Arg.to_var arg_1) xyz_label
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
  ]
