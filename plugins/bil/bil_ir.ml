open Core_kernel
open Bap.Std
open Format

open Bap_knowledge
open Bap_core_theory

open Knowledge.Syntax

type blk = {
  name : Theory.label;
  defs : def term list;
  jmps : jmp term list;
}

type cfg = {
  blks : blk list;
  entry : Theory.label;
}

type t = cfg

module Graph = struct
  type t = cfg option

  let empty = None
  let pp_sema = Knowledge.Value.pp

  let pp_jmp ppf {cnd; dst} =
    let pp_cnd ppf =
      Option.iter cnd ~f:(fun bit ->
          fprintf ppf "when %a " pp_sema bit) in
    match dst with
    | Indirect dst ->
      fprintf ppf "  %tjump %a@\n" pp_cnd pp_sema dst
    | Direct (Label dst) ->
      fprintf ppf "  %tgoto L%a@\n" pp_cnd Label.pp dst
    | Direct (Addr dst) ->
      fprintf ppf "  %tgoto L%a@\n" pp_cnd Addr.pp dst
    | Direct (CpuExn dst) ->
      fprintf ppf "  %tgoto L%a@\n" pp_cnd Int.pp dst


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

  let inspect = function
    | {blks=[]} -> Sexp.List []
    | cfg -> Sexp.Atom (asprintf "%a" pp_bil cfg)

  let domain = Knowledge.Domain.define

end

let graph = Semantics.declare
    ~name:"cfg"
    (module Graph)

let slot = graph

module BIR = struct
  type t = blk term list
  module Tid = Bap.Std.Tid
  module Var = Bap.Std.Var

  let tid_of_label labels label =
    Hashtbl.find_or_add labels label
      ~default:(fun () -> Tid.create ())

  let init_labels () = Label.Table.create ()

  let add_def b {name; sema; virt} =
    match Semantics.get Bil.Domain.exp sema with
    | None -> ()
    | Some exp ->
      (* alternatively we can look into the sort *)
      let typ = Type.infer_exn exp in
      let var = Var.create ~is_virtual:virt name typ in
      let def = Def.Semantics.create var sema in
      Blk.Builder.add_def b def

  let reify_cnd = function
    | None -> Bil.int Word.b1
    | Some s -> match Semantics.get Bil.Domain.exp s with
      | None -> Bil.unknown "unrepresentable" bool_t
      | Some x -> x

  let add_indirect b cond dst = match Semantics.get Bil.Domain.exp dst with
    | None -> ()
    | Some exp ->
      let typ = Type.infer_exn exp in
      let jmp = Jmp.Semantics.jump ?cond typ dst in
      Blk.Builder.add_jmp b jmp

  (* later we will use the obtained knowledge to decide,
     which is call, and which is not, but so far, just
     let's create a goto *)
  let add_direct labels b cond lbl =
    let cond = reify_cnd cond in
    Blk.Builder.add_jmp b @@ match lbl with
    | Label lbl ->
      Jmp.create_goto ~cond (Direct (tid_of_label labels lbl))
    | Addr addr ->
      Jmp.create_goto ~cond (Indirect Bil.(int addr))
    | CpuExn n ->
      let fall = Tid.create () in
      Jmp.create_int ~cond n fall


  let add_jmp labels b {cnd; dst} =
    let cnd = Option.map cnd ~f:Value.semantics in
    match dst with
    | Indirect x -> add_indirect b cnd x
    | Direct lbl -> add_direct labels b cnd lbl

  module Expressible = struct
    let check f x = Option.is_some (f Bil.Domain.exp x)

    let def {sema} = check Semantics.get sema

    let cnd = function
      | None -> true
      | Some v -> check Value.get v

    let dst = function
      | Direct _ -> true
      | Indirect s -> check Semantics.get s

    let jmp t = dst t.dst
  end

  let add_term is_expressible add (blks,b) t =
    match is_expressible t with
    | true -> add b t; (blks,b)
    | false ->
      let empty = Blk.create () in
      let tid = Tid.create () in
      let b' = Blk.Builder.create ~tid () in
      let j1 = Jmp.create_goto (Direct (Term.tid empty)) in
      let j2 = Jmp.create_goto (Direct tid) in
      Blk.Builder.add_jmp b j1;
      let empty = Term.append jmp_t empty j2 in
      empty :: Blk.Builder.result b :: blks, b'

  let add_terms terms expressible add (blks,b) =
    List.fold ~init:(blks,b) (List.rev terms)
      ~f:(add_term expressible add)

  (* creates a tid block from the IR block,
     expands non-representable terms into empty blocks.
     postconditions:
     - the list is not empty;
     - the first element of the list, is the entry
  *)
  let make_blk labels {name; defs; jmps} =
    let tid = tid_of_label labels name in
    let b = Blk.Builder.create ~tid () in
    ([],b) |>
    add_terms defs Expressible.def add_def |>
    add_terms jmps Expressible.jmp (add_jmp labels) |> fun (blks,b) ->
    Blk.Builder.result b :: blks |>
    List.rev


  (* postconditions:
     - the list is not empty
     - the first block is the entry block
     - the last block is the exit block
  *)
  let reify {entry; blks} =
    let labels = init_labels () in
    let start = tid_of_label labels entry in
    List.fold blks ~init:(None,[]) ~f:(fun (s,blks) b ->
        match make_blk labels b with
        | [] -> assert false
        | blk::blks' ->
          if Tid.equal start (Term.tid blk)
          then (Some blk, List.rev_append blks' blks)
          else (s, List.rev_append (blk::blks') blks)) |> function
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
  let jmp = (fun x -> x.jmps), (fun x d -> match x.jmps with
      | {cnd = None} :: _ -> x
      | _ -> {x with jmps = d})

  let push_to_blk (get,put) blk elt =
    put blk @@ elt :: get blk


  let push fld elt cfg : cfg = match cfg with
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
      blks = [{name=entry; jmps=[]; defs=[Def.reify v x]}]
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
            jmps = [{cnd = Some cnd; dst = Direct (Label head)}]
          }]}
    | {entry=loop; blks=b::blks} ->
      fresh >>= fun head ->
      fresh >>= fun tail ->
      let goto_tail = {cnd = None; dst = Direct (Label tail)} in
      let goto_loop = {cnd = Some cnd; dst = Direct (Label loop)} in
      data {
        entry = head;
        blks = blk tail ++ goto_loop ::
               blk head ++ goto_tail ::
               b ++ goto_tail ::
               blks
      }

  let jump cnd dst = {cnd = Some cnd; dst = Direct (Label dst)}
  let fall dst = {cnd = None; dst = Direct (Label dst)}


  let branch cnd yes nay =
    fresh >>= fun head ->
    fresh >>= fun tail ->
    cnd >>= fun cnd ->
    yes >>= fun yes ->
    nay >>= fun nay ->
    let return = ret (Eff.kind yes) in
    let jump = jump cnd in
    match reify yes, reify nay with
    | {entry; blks=[{defs=[]; jmps=[j]} as blk]},{blks=[]} -> return {
        entry;
        blks = [{blk with defs=[]; jmps=[{j with cnd = Some cnd}]}]
      }
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


  let resolve_dst dst =
    resolve_addr dst >>= function
    | Some known -> Knowledge.return (Addr known)
    | None ->
      resolve_ivec dst >>= function
      | Some known -> Knowledge.return (CpuExn known)
      | None -> Knowledge.return (Label dst)


  let goto dst =
    fresh >>= fun entry ->
    resolve_dst dst >>= fun dst ->
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

let pp = Graph.pp_bil

let init () =
  Theory.register
    ~desc:"CFG generator"
    ~name:"cfg"
    (module IR)

module Theory = IR
