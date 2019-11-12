open Core_kernel
open Bap.Std
open Format

open Bap_knowledge
open Bap_core_theory

open Knowledge.Syntax

include Self()

type blk = {
  name : Theory.Label.t;
  defs : def term list;
  jmps : jmp term list;
} [@@deriving bin_io]

type cfg = {
  blks : blk list;
  entry : Theory.Label.t;
} [@@deriving bin_io]

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
    ~equal:(fun x y -> Theory.Label.equal x.entry y.entry)

let graph =
  KB.Class.property Theory.Program.Semantics.cls "ir-graph" domain
    ~persistent:(KB.Persistent.of_binable (module struct
                   type t = cfg option [@@deriving bin_io]
                 end))
    ~package:"bap.std"
    ~public:true
    ~desc:"the graphical representation of the program"

let slot = graph

module IR = struct
  include Theory.Empty
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

  let empty = Theory.Effect.empty Theory.Effect.Sort.bot
  let ret cfg = !!(KB.Value.put graph empty (Some cfg))
  let data cfg = ret cfg
  let ctrl cfg = ret cfg

  let set v x =
    x >>= fun x ->
    fresh >>= fun entry ->
    fresh >>= fun tid ->
    data {
      entry;
      blks = [{name=entry; jmps=[]; defs=[Def.reify ~tid v x]}]
    }

  let goto ?cnd ~tid dst =
    Jmp.reify ?cnd ~tid ~dst:(Jmp.resolved dst) ()

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
      fresh >>= fun head ->
      fresh >>= fun tid ->
      data {
        entry = head;
        blks = [{
            name = head;
            defs = [];
            jmps = [goto ~cnd ~tid head]}]}
    | {entry=loop; blks=b::blks} ->
      fresh >>= fun head ->
      fresh >>= fun tail ->
      fresh >>= fun jmp1 ->
      fresh >>= fun jmp2 ->
      fresh >>= fun jmp3 ->
      data {
        entry = head;
        blks = blk tail ++ goto ~tid:jmp1 ~cnd loop ::
               blk head ++ goto ~tid:jmp2 tail ::
               b ++ goto ~tid:jmp3 tail ::
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
    | {entry=lhs; blks=b::blks},{blks=[]} ->
      fresh >>= fun jmp1 ->
      fresh >>= fun jmp2 ->
      fresh >>= fun jmp3 ->
      ret {
        entry = head;
        blks =
          blk tail ::
          blk head ++
          jump ~tid:jmp1 lhs ++
          goto ~tid:jmp2 tail ::
          b ++ goto ~tid:jmp3 tail ::
          blks
      }
    | {blks=[]}, {entry=rhs; blks=b::blks} ->
      fresh >>= fun jmp1 ->
      fresh >>= fun jmp2 ->
      fresh >>= fun jmp3 ->
      ret {
        entry = head;
        blks =
          blk tail ::
          blk head ++
          jump ~tid:jmp1 tail ++
          goto ~tid:jmp2 rhs ::
          b ++ goto ~tid:jmp3 tail ::
          blks
      }
    | {entry=lhs; blks=yes::ayes}, {entry=rhs; blks=nay::nays} ->
      fresh >>= fun jmp1 ->
      fresh >>= fun jmp2 ->
      fresh >>= fun jmp3 ->
      fresh >>= fun jmp4 ->
      ret {
        entry = head;
        blks =
          blk tail ::
          blk head ++
          jump ~tid:jmp1 lhs ++
          goto ~tid:jmp2 rhs ::
          yes ++ goto ~tid:jmp3 tail ::
          nay ++ goto ~tid:jmp4 tail ::
          List.rev_append ayes nays
      }
    | {blks=[]}, {blks=[]} ->
      fresh >>= fun jmp1 ->
      fresh >>= fun jmp2 ->
      ret {
        entry = head;
        blks = [
          blk tail;
          blk head ++ jump ~tid:jmp1 tail ++ goto ~tid:jmp2 tail
        ]
      }

  let jmp dst =
    fresh >>= fun entry ->
    dst >>= fun dst ->
    fresh >>= fun tid ->
    ctrl {
      entry;
      blks = [blk entry ++ Jmp.reify ~tid ~dst:(Jmp.indirect dst) ()]
    }

  let appgraphs fst snd =
    match fst, snd with
    | {entry; blks}, {blks=[]}
    | {blks=[]}, {entry; blks} -> ret {entry; blks}
    | {entry; blks={jmps=[]} as x :: xs},{blks=[y]} -> ret {
        entry;
        blks = {x with defs = y.defs @ x.defs; jmps = y.jmps} :: xs
      }
    | {entry; blks=x::xs}, {entry=snd; blks=y::ys} ->
      fresh >>= fun tid -> ret {
        entry;
        blks =
          y ::
          x ++ goto ~tid snd ::
          List.rev_append xs ys
      }

  let (>>->) x f = x >>= reify >>= f

  let seq fst snd =
    fst >>-> fun fst ->
    snd >>-> fun snd ->
    appgraphs fst snd

  let do_goto dst =
    fresh >>= fun entry ->
    fresh >>= fun tid ->
    ctrl {
      entry;
      blks = [blk entry ++ goto ~tid dst]
    }


  let blk _entry defs jmps =
    defs >>-> fun defs ->
    jmps >>-> fun jmps ->
    appgraphs defs jmps

  let goto = do_goto
end

let reify = function
  | None -> []
  | Some g -> BIR.reify g

let init () = Theory.declare !!(module IR : Theory.Core)
    ~package:"bap.std" ~name:"bir"
    ~desc:"Builds the graphical representation of a program."
    ~provides:[
      "cfg";
    ]

module Theory = IR
