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

type t = cfg option

module BIR = struct
  type t = blk term list

  let add_def = Blk.Builder.add_def
  let add_jmp = Blk.Builder.add_jmp
  let add_term add (blks,b) t = add b t; (blks,b)
  let add_terms terms add (blks,b) =
    List.fold ~init:(blks,b) (List.rev terms)
      ~f:(add_term add)

  (* creates a tid block from the IR block,
     expands non-representable terms into empty blocks.
     postconditions:
     - the list is not empty;
     - the first element of the list, is the entry
  *)
  let make_blk {name; defs; jmps} =
    let b = Blk.Builder.create ~tid:name () in
    ([],b) |>
    add_terms defs add_def |>
    add_terms jmps add_jmp |> fun (blks,b) ->
    Blk.Builder.result b :: blks |>
    List.rev


  (* postconditions:
     - the list is not empty
     - the first block is the entry block
     - the last block is the exit block
  *)
  let reify {entry; blks} =
    List.fold blks ~init:(None,[]) ~f:(fun (s,blks) b ->
        match make_blk b with
        | [] -> assert false
        | blk::blks' ->
          if Tid.equal entry (Term.tid blk)
          then (Some blk, List.rev_append blks' blks)
          else (s, List.rev_append (blk::blks') blks)) |> function
    | None,[] -> []
    | None,_ -> failwith "No entry in IR builder"
    | Some x, xs -> x :: xs
end

let pp_cfg ppf ir =
  fprintf ppf "%a" (pp_print_list Blk.pp) (BIR.reify ir)

let inspect cfg = Sexp.Atom (asprintf "%a" pp_cfg cfg)

let null = KB.Symbol.intern "null" Theory.Program.cls
let is_null x =
  null >>| fun null ->
  Theory.Label.equal null x

let domain = KB.Domain.optional ~inspect "graph"

let graph = KB.Class.property Theory.Program.Semantics.cls "ir-graph" domain
let slot = graph

module IR = struct
  include Theory.Core.Empty
  let ret = Knowledge.return

  let blk tid = {name=tid; defs=[]; jmps=[]}


  let def = (fun x -> x.defs), (fun x d -> {x with defs = d})
  let jmp = (fun x -> x.jmps), (fun x d -> match x.jmps with
      | t :: _ when Option.is_none (Jmp.guard t) -> x
      | _ -> {x with jmps = d})

  let push_to_blk (get,put) blk elt =
    put blk @@ elt :: get blk


  let push fld elt cfg : cfg = match cfg with
    | {blks=[]} -> assert false  (* the precondition - we have a block *)
    | {blks=blk::blks} -> {
        cfg with blks = push_to_blk fld blk elt :: blks
      }

  let fresh = KB.Object.create Theory.Program.cls

  let (++) b j = push_to_blk jmp b j

  let reify x : cfg knowledge = match KB.Value.get graph x with
    | Some g -> KB.return g
    | None -> null >>| fun entry -> {
        blks = [];
        entry;
      }

  let empty = KB.Value.empty Theory.Program.Semantics.cls
  let ret cfg = !!(KB.Value.put graph empty (Some cfg))
  let data cfg = ret cfg
  let ctrl cfg = ret cfg

  let set v x =
    x >>= fun x ->
    fresh >>= fun entry ->
    data @@ {
      entry;
      blks = [{name=entry; jmps=[]; defs=[Def.reify v x]}]
    }

  let goto ?cnd dst = Jmp.resolved ?cnd dst Jmp.Role.local

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
    body >>= reify >>= function
    | {blks=[]} ->
      fresh >>= fun head -> data {
        entry = head;
        blks = [{
            name = head;
            defs = [];
            jmps = [goto ~cnd head]}]}
    | {entry=loop; blks=b::blks} ->
      fresh >>= fun head ->
      fresh >>= fun tail ->
      data {
        entry = head;
        blks = blk tail ++ goto ~cnd loop ::
               blk head ++ goto tail ::
               b ++ goto tail ::
               blks
      }

  let branch cnd yes nay =
    fresh >>= fun head ->
    fresh >>= fun tail ->
    cnd >>= fun cnd ->
    yes >>= fun yes ->
    nay >>= fun nay ->
    reify yes >>= fun yes ->
    reify nay >>= fun nay ->
    let jump = goto ~cnd in
    match yes, nay with
    | {entry; blks=[{defs=[]; jmps=[j]} as blk]},{blks=[]} -> ret {
        entry;
        blks = [{blk with defs=[]; jmps=[Jmp.with_guard j (Some cnd)]}]
      }
    | {entry=lhs; blks=b::blks},{blks=[]} -> ret {
        entry = head;
        blks =
          blk tail ::
          blk head ++ jump lhs ++ goto tail ::
          b ++ goto tail ::
          blks
      }
    | {blks=[]}, {entry=rhs; blks=b::blks} -> ret {
        entry = head;
        blks =
          blk tail ::
          blk head ++ jump tail ++ goto rhs ::
          b ++ goto tail ::
          blks
      }
    | {entry=lhs; blks=yes::ayes}, {entry=rhs; blks=nay::nays} -> ret {
        entry = head;
        blks =
          blk tail ::
          blk head ++ jump lhs ++ goto rhs ::
          yes ++ goto tail ::
          nay ++ goto tail ::
          List.rev_append ayes nays
      }
    | {blks=[]}, {blks=[]} -> ret {
        entry = head;
        blks = [
          blk tail;
          blk head ++ jump tail ++ goto tail
        ]
      }

  let jmp dst =
    fresh >>= fun entry ->
    dst >>= fun dst ->
    ctrl {
      entry;
      blks = [blk entry ++ Jmp.indirect dst]
    }

  let appgraphs fst snd =
    match fst, snd with
    | {entry; blks}, {blks=[]}
    | {blks=[]}, {entry; blks} -> ret {entry; blks}
    | {entry; blks={jmps=[]} as x :: xs},{blks=[y]} -> ret {
        entry;
        blks = {x with defs = y.defs @ x.defs; jmps = y.jmps} :: xs
      }
    | {entry; blks=x::xs}, {entry=snd; blks=y::ys} -> ret {
        entry;
        blks =
          y ::
          x ++ goto snd ::
          List.rev_append xs ys
      }

  let (>>->) x f = x >>= reify >>= f

  let seq fst snd =
    fst >>-> fun fst ->
    snd >>-> fun snd ->
    appgraphs fst snd


  let unlabeled_blk defs jmps =
    match defs, jmps with
    | {blks=[]}, {entry; blks}
    | {entry; blks}, {blks=[]} -> {entry; blks}
    | _ -> assert false


  let do_goto dst =
    fresh >>= fun entry ->
    ctrl {
      entry;
      blks = [blk entry ++ goto dst]
    }

  let blk entry defs jmps =
    defs >>-> fun defs ->
    jmps >>-> fun jmps ->
    is_null entry  >>= function
    | true -> appgraphs defs jmps
    | false ->
      match defs, jmps with
      | {blks=[]}, {blks=[]} -> ret {
          entry;
          blks = [blk entry]
        }
      | {blks=[]}, {entry=next; blks=b::blks}
      | {entry=next; blks=b::blks}, {blks=[]} -> ret {
          entry;
          blks = b :: blk entry ++ goto next :: blks
        }
      | {entry=fst; blks=x::xs},
        {entry=snd; blks=y::ys} ->
        ret {
          entry;
          blks =
            y ::
            blk entry ++ goto fst ::
            x ++ goto snd ::
            List.rev_append xs ys
        }

  let goto = do_goto

end

let reify = function
  | None -> []
  | Some g -> BIR.reify g

let init () =
  Theory.register
    ~desc:"CFG generator"
    ~name:"cfg"
    (module IR)

module Theory = IR
