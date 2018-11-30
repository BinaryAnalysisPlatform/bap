open Core_kernel
open Bap.Std
open Format

open Bap_knowledge
open Bap_core_theory

open Knowledge.Syntax
open Link.Syntax

type def = {
  name : string;
  sort : Sort.exp;
  sema : semantics;
}

type dst = Direct of Label.t | Indirect of semantics

type jmp = {
  cnd : bit value option;
  dst : dst;
}

type blk = {
  name : Label.t;
  defs : def list;
  jmps : jmp list;
}

type cfg = {
  blks : blk list;
  entry : Label.t;
}

type t = cfg

module Graph = struct
  type t = cfg
  let partial _ _ = Domain.Order.NC
  let inspect _ = Sexp.Atom "graph"
  let empty = {blks=[]; entry = Label.root}


  let pp_jmp pp_sema ppf {cnd; dst} =
    let pp_cnd ppf =
      Option.iter cnd ~f:(fun bit ->
          fprintf ppf "when %a " pp_sema (Value.semantics bit)) in
    match dst with
    | Direct dst ->
      fprintf ppf "  %tgoto L%a@\n" pp_cnd Label.pp dst
    | Indirect dst ->
      fprintf ppf "  %tjump %a@\n" pp_cnd pp_sema dst


  let pp_def pp_sema ppf {name; sema} =
    fprintf ppf "  %s := %a@\n" name pp_sema sema

  let pp_blk pp_sema ppf {name; defs; jmps} =
    fprintf ppf "L%a:@\n" Label.pp name;
    List.iter (List.rev defs) ~f:(pp_def pp_sema ppf);
    List.iter (List.rev jmps) ~f:(pp_jmp pp_sema ppf)


  let pp pp_sema ppf {entry; blks} =
    fprintf ppf "start:@\n  goto L%a@\n" Label.pp entry;
    List.iter (List.rev blks) ~f:(pp_blk pp_sema ppf)

  let pp_bil_sema ppf v = match Semantics.get Bil.Domain.exp v with
    | None -> fprintf ppf "<undefined>"
    | Some exp -> Exp.pp ppf exp


  let pp_bil = pp pp_bil_sema
end

let graph = Semantics.declare
    ~name:"cfg"
    (module Graph)

let t = graph

module BIR = struct
  type t = blk term list
  module Tid = Bap.Std.Tid
  module Var = Bap.Std.Var

  let tid_of_label labels label =
    Hashtbl.find_or_add labels label
      ~default:(fun () -> Tid.create ())

  let init_labels () = Label.Table.create ()

  let add_def b {name; sema} =
    match Semantics.get Bil.Domain.exp sema with
    | None -> ()
    | Some exp ->
      (* alternatively we can look into the sort *)
      let typ = Type.infer_exn exp in
      let var = Var.create name typ in
      Blk.Builder.add_def b (Def.create var exp)

  let reify_cnd = function
    | None -> Bil.int Word.b1
    | Some s -> match Value.get Bil.Domain.exp s with
      | None -> Bil.unknown "unrepresentable" bool_t
      | Some x -> x


  let add_indirect b cond dst = match Semantics.get Bil.Domain.exp dst with
    | None -> ()
    | Some exp ->
      Blk.Builder.add_jmp b @@
      Jmp.create_goto ~cond (Indirect exp)

  (* later we will use the obtained knowledge to decide,
     which is call, and which is not, but so far, just
     let's create a goto *)
  let add_direct labels b cond lbl =
    Blk.Builder.add_jmp b @@
    Jmp.create_goto ~cond (Direct (tid_of_label labels lbl))

  let add_jmp labels b {cnd; dst} =
    let cnd = reify_cnd cnd in
    match dst with
    | Indirect x -> add_indirect b cnd x
    | Direct lbl -> add_direct labels b cnd lbl


  let make_blk labels {name; defs; jmps} =
    let tid = tid_of_label labels name in
    let b = Blk.Builder.create ~tid () in
    List.iter (List.rev defs) ~f:(add_def b);
    List.iter (List.rev jmps) ~f:(add_jmp labels b);
    Blk.Builder.result b

  (* postconditions:
     - the list is not empty
     - the first block is the entry block
     - the last block is the exit block
  *)
  let reify {entry; blks} =
    let labels = init_labels () in
    let start = tid_of_label labels entry in
    List.fold blks ~init:(None,[]) ~f:(fun (s,blks) b ->
        let blk = make_blk labels b in
        if Tid.equal start (Term.tid blk)
        then (Some blk, blks)
        else (s, blk::blks)) |> function
    | None,[] -> []
    | None,_ -> failwith "No entry in IR builder"
    | Some x, xs -> x :: xs
end

let is_null x = Label.equal Label.root x

module IR = struct
  include Theory.Core.Empty
  let ret = Knowledge.return

  let blk tid = {name=tid; defs=[]; jmps=[]}


  let def = (fun x -> x.defs), (fun x d -> {x with defs = d})
  let jmp = (fun x -> x.jmps), (fun x d -> {x with jmps = d})
  let push_to_blk (get,put) blk elt =
    put blk @@ elt :: get blk


  let push fld elt cfg = match cfg with
    | {blks=[]} -> assert false  (* the precondition - we have a block *)
    | {blks=blk::blks} -> {
        cfg with blks = push_to_blk fld blk elt :: blks
      }
  let fresh = Label.Generator.fresh
  let (++) b j = push_to_blk jmp b j
  let reify x = Eff.get graph x

  let ret kind cfg = !!(Eff.put graph (Eff.empty kind) cfg)
  let data = ret Kind.data
  let ctrl = ret Kind.ctrl

  let set v x =
    x >>= fun x ->
    fresh >>= fun entry ->
    data @@ {
      entry;
      blks = [{name=entry; jmps=[]; defs=[{
          name = Var.name v;
          sort = Sort.exp (Var.sort v);
          sema = Value.semantics x;
        }]}]
    }

  (** reifies a [while (<cnd>) <body>] loop to

      {v
        head:
          goto tail
        loop:
          <body>
        tail:
          when <cnd> goto loop
     v}

      or to just

      {v
        head:
          when <cnd> goto head
      v}

      if <body> is empty.
  *)
  let repeat cnd body =
    cnd >>= fun cnd ->
    body >>| reify >>= function
    | {blks=[]} ->
      fresh >>= fun head -> data {
        entry = head;
        blks = [{
            name = head;
            defs = [];
            jmps = [{cnd = Some cnd; dst = Direct head}]
          }]}
    | {entry=loop; blks=b::blks} ->
      fresh >>= fun head ->
      fresh >>= fun tail ->
      let goto_tail = {cnd = None; dst = Direct tail} in
      let goto_loop = {cnd = Some cnd; dst = Direct loop} in
      data {
        entry = head;
        blks = blk tail ++ goto_loop ::
               blk head ++ goto_tail ::
               b ++ goto_tail ::
               blks
      }

  let jump cnd dst = {cnd = Some cnd; dst = Direct dst}
  let fall dst = {cnd = None; dst = Direct dst}


  let branch cnd yes nay =
    fresh >>= fun head ->
    fresh >>= fun tail ->
    cnd >>= fun cnd ->
    yes >>= fun yes ->
    nay >>= fun nay ->
    let return = ret (Eff.kind yes) in
    let jump = jump cnd in
    match reify yes, reify nay with
    | {entry=lhs; blks=b::blks},{blks=[]} -> return {
        entry = head;
        blks =
          blk tail ::
          blk head ++ jump lhs ++ fall tail ::
          b ++ fall tail ::
          blks
      }
    | {blks=[]}, {entry=rhs; blks=b::blks} -> return {
        entry = head;
        blks =
          blk tail ::
          blk head ++ jump tail ++ fall rhs ::
          b ++ fall tail ::
          blks
      }
    | {entry=lhs; blks=yes::ayes}, {entry=rhs; blks=nay::nays} -> return {
        entry = head;
        blks =
          blk tail ::
          blk head ++ jump lhs ++ fall rhs ::
          yes ++ fall tail ::
          nay ++ fall tail ::
          List.rev_append ayes nays
      }
    | {blks=[]}, {blks=[]} -> return {
        entry = head;
        blks = [
          blk tail;
          blk head ++ jump tail ++ fall tail
        ]
      }



  let jmp dst =
    fresh >>= fun entry ->
    dst >>= fun dst ->
    let dst = Indirect (Value.semantics dst) in
    ctrl {
      entry;
      blks = [blk entry ++ {dst; cnd=None}]
    }


  let goto dst =
    fresh >>= fun entry ->
    ctrl {
      entry;
      blks = [blk entry ++ {dst=Direct dst; cnd=None}]
    }

  let appgraphs k fst snd =
    let return = ret k in
    match fst, snd with
    | {entry; blks}, {blks=[]}
    | {blks=[]}, {entry; blks} -> return {entry; blks}
    | {entry; blks={jmps=[]} as x :: xs},{blks=[y]} -> return {
        entry;
        blks = {x with defs = y.defs @ x.defs; jmps = y.jmps} :: xs
      }
    | {entry; blks=x::xs}, {entry=snd; blks=y::ys} -> return {
        entry;
        blks =
          y ::
          x ++ fall snd ::
          List.rev_append xs ys
      }

  let seq fst snd =
    fst >>= fun fst ->
    snd >>= fun snd ->
    appgraphs (Eff.kind fst) (reify fst) (reify snd)


  let unlabeled_blk defs jmps =
    match defs, jmps with
    | {blks=[]}, {entry; blks}
    | {entry; blks}, {blks=[]} -> {entry; blks}

    | _ -> assert false

  let blk entry defs jmps =
    defs >>| reify >>= fun defs ->
    jmps >>| reify >>= fun jmps ->
    if is_null entry then appgraphs Kind.unit defs jmps
    else
      let return = ret Kind.unit in
      match defs, jmps with
      | {blks=[]}, {blks=[]} ->
        return {
          entry;
          blks = [blk entry]
        }
      | {blks=[]}, {entry=next; blks=b::blks}
      | {entry=next; blks=b::blks}, {blks=[]} ->
        return {
          entry;
          blks = b :: blk entry ++ fall next :: blks
        }
      | {entry=fst; blks=x::xs},
        {entry=snd; blks=y::ys} ->
        return {
          entry;
          blks =
            y ::
            blk entry ++ fall fst ::
            x ++ fall snd ::
            List.rev_append xs ys
        }
end

let reify = BIR.reify

let init () =
  Theory.register
    ~desc:"CFG generator"
    ~name:"cfg"
    (module IR)
