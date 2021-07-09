open Core_kernel
open Bap.Std
open Format

open Bap_knowledge
open Bap_core_theory

open Knowledge.Syntax
open KB.Let

include Self()

type blk = {
  name : Theory.Label.t;
  keep : bool;
  defs : def term list;
  jmps : jmp term list;
} [@@deriving bin_io]

type cfg = {
  blks : blk list;
  entry : Theory.Label.t;
} [@@deriving bin_io]

type t = cfg

let null = KB.Object.null Theory.Program.cls
let is_null x = KB.Object.is_null x
let is_empty {entry} = is_null entry

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
     - the first block is the entry block
     - the last block is the exit block
  *)
  let reify {entry; blks} =
    if is_null entry then [] else
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
  fprintf ppf "@[<v>%a@]" (pp_print_list Blk.pp) (BIR.reify ir)

let inspect cfg = Sexp.Atom (asprintf "%a" pp_cfg cfg)

let domain = KB.Domain.flat ~inspect "graph"
    ~empty:{entry=null; blks=[]}
    ~equal:(fun x y -> Theory.Label.equal x.entry y.entry)

let graph =
  KB.Class.property Theory.Semantics.cls "ir-graph" domain
    ~persistent:(KB.Persistent.of_binable (module struct
                   type t = cfg [@@deriving bin_io]
                 end))
    ~package:"bap"
    ~public:true
    ~desc:"the graphical representation of the program"


let slot = graph


(*
   Invariants:
   1. all non-empty denotations have a non-empty list of blocks
   1.1 is_null entry <=> (blks = [])
   2. if the denotation is non-empty then the list of blocks
      has a block with the name equal to entry.
   3. explicitly labeled blocks keep their names.


  *)
module IR = struct
  include Theory.Empty
  let ret = Knowledge.return
  let blk ?(keep=true) tid = {name=tid; defs=[]; jmps=[]; keep}

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

  let reify x : cfg = KB.Value.get graph x
  let (>>->) x f = x >>| reify >>= f


  let empty = Theory.Effect.empty Theory.Effect.Sort.bot
  let ret cfg = !!(KB.Value.put graph empty cfg)
  let data cfg = ret cfg
  let ctrl cfg = ret cfg

  let target =
    KB.Object.scoped Theory.Program.cls @@ fun lbl ->
    Theory.Label.target lbl


  let goto ?(is_call=false) ?cnd ~tid dst =
    if is_call
    then Jmp.reify ?cnd ~tid ~alt:(Jmp.resolved dst) ()
    else Jmp.reify ?cnd ~tid ~dst:(Jmp.resolved dst) ()

  let relink label {entry; blks} =
    if is_null entry then KB.return {
        entry = label;
        blks = [{name=label; keep=true; defs=[]; jmps=[]}]
      } else
      let+ blks = List.fold_map blks ~init:`Unbound ~f:(fun r blk ->
          if Theory.Label.equal blk.name entry
          then if blk.keep then `Relink blk.name, blk
            else `Relinked, {blk with name = label; keep=true}
          else r,blk) |> function
                  | `Relinked,blks -> KB.return blks
                  | `Relink dst, blks ->
                    let+ tid = fresh in
                    blks @ [blk label ++ goto ~tid dst]
                  | `Unbound,[] -> assert false
                  | `Unbound,_ -> assert false      in
      {entry = label; blks}

  let set v x =
    target >>= fun t ->
    if Theory.Target.has_roles t [Theory.Role.Register.constant] v
    then !!empty
    else
      x >>= fun x ->
      fresh >>= fun entry ->
      fresh >>= fun tid ->
      data {
        entry;
        blks = [{
            name=entry;
            keep=false;
            jmps=[];
            defs=[Def.reify ~tid v x]
          }]
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
    | {blks=[]} ->               (* empty denotation *)
      fresh >>= fun head ->
      fresh >>= fun tid ->
      data {
        entry = head;
        blks = [{
            name = head;
            keep = true;
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
    yes >>-> fun yes ->
    nay >>-> fun nay ->
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
      blks = [blk ~keep:false entry ++ Jmp.reify ~tid ~dst:(Jmp.indirect dst) ()]
    }

  let appgraphs fst snd =
    if is_empty fst then ret snd else
    if is_empty snd then ret fst
    else match fst, snd with
      | _,{blks=[]} | {blks=[]}, _ -> assert false
      | {entry; blks={jmps=[]} as x :: xs},{blks=[{keep=false} as y]} ->
        ret {
          entry;
          blks = {x with defs = y.defs @ x.defs; jmps = y.jmps} :: xs
        }
      | {entry; blks=x::xs}, {entry=esnd; blks=y::ys} ->
        fresh >>= fun tid -> ret {
          entry;
          blks =
            y ::
            x ++ goto ~tid esnd ::
            List.rev_append xs ys
        }

  let seq fst snd =
    fst >>-> fun fst ->
    snd >>-> fun snd ->
    appgraphs fst snd

  let do_goto dst =
    fresh >>= fun entry ->
    fresh >>= fun tid ->
    KB.collect Theory.Label.is_subroutine dst >>= fun is_call ->
    ctrl {
      entry;
      blks = [blk ~keep:false entry ++ goto ?is_call ~tid dst]
    }

  let blk label defs jmps =
    defs >>-> fun defs ->
    jmps >>-> fun jmps ->
    appgraphs defs jmps >>-> fun res ->
    if is_null label then ret res
    else relink label res >>= ret

  let goto = do_goto
end

let reify = BIR.reify

let init () = Theory.declare !!(module IR : Theory.Core)
    ~package:"bap" ~name:"bir"
    ~desc:"Builds the graphical representation of a program."
    ~provides:[
      "cfg";
    ]

module Theory = IR
