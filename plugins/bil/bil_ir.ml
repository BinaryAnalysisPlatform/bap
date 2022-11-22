open Core_kernel[@@warning "-D"]
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
  weak : bool;
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
let is_call jmp = Option.is_some (Jmp.alt jmp)
let is_empty = function
  | {entry; blks=[]} -> is_null entry
  | _ -> false
let is_unconditional jmp = match Jmp.cond jmp with
  | Int w when Word.(w = b1) -> true
  | _ -> false

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

  let dst jmp =
    match Option.(Jmp.(dst jmp >>| resolve)) with
    | Some First tid -> Some tid
    | _ -> None

  let dsts blk =
    List.filter_map blk.jmps ~f:dst

  let references blks =
    List.fold ~init:Tid.Map.empty ~f:(fun refs {jmps} ->
        List.fold jmps ~init:refs ~f:(fun refs jmp ->
            match dst jmp with
            | Some tid when Map.mem blks tid ->
              Map.update refs tid ~f:(function
                  | None -> 1
                  | Some refs -> refs+1)
            | _ -> refs))

  let build_graph = List.fold
      ~init:Tid.Map.empty
      ~f:(fun blks blk ->
          Map.add_exn blks blk.name blk)

  let single_dst = function
    | [] | _ :: _ :: _ -> None
    | [x] -> match dst x with
      | Some tid when not (is_call x) && is_unconditional x -> Some tid
      | _ -> None

  let can_contract refs b1 b2 =
    not (Tid.equal b1.name b2.name) &&
    (not b2.keep || b2.weak) &&
    match single_dst b1.jmps with
    | None -> false
    | Some dst ->
      Tid.equal dst b2.name &&
      match Map.find refs dst with
      | Some 1 -> true
      | _ -> false

  (* pre: can_contract b1 b2 *)
  let join x y = {
    x with
    defs = y.defs @ x.defs;
    jmps = y.jmps
  }

  let (//) graph node =
    Map.remove graph node.name

  let has_name name blk = Tid.equal name blk.name

  let contracted exit parent dst =
    if has_name exit dst then parent.name else exit

  let contract refs graph ~entry ~exit =
    let rec contract output graph exit node =
      match Option.(single_dst node.jmps >>= Map.find graph) with
      | Some dst when can_contract refs node dst ->
        let node = join node dst in
        contract output (graph//dst) (contracted exit node dst) node
      | _ -> follow output graph exit node
    and follow output graph exit node = List.fold (dsts node)
        ~init:(node::output,graph//node,exit)
        ~f:(fun (output,graph,exit) name ->
            match Map.find graph name with
            | None -> output,graph,exit
            | Some node -> contract output graph exit node) in
    contract [] graph exit (Map.find_exn graph entry)

  let normalize entry blks =
    let {name=exit} = List.hd_exn blks in
    let graph = build_graph blks in
    let refs = references graph blks in
    let blks,leftovers,exit = contract refs graph ~entry ~exit in
    assert (Map.is_empty leftovers);
    match blks with
    | blk::_ as blks when has_name exit blk -> blks
    | blks ->
      List.find_exn blks ~f:(has_name exit) ::
      List.filter blks ~f:(Fn.non (has_name exit))

  let normalize entry = function
    | [] | [_] as xs -> xs
    | xs ->
      try normalize entry xs
      with _ ->
        warning "can't normalize invalid IR: %a" Tid.pp entry;
        xs

  (* postconditions:
     - the first block is the entry block
     - the last block is the exit block
  *)
  let reify {entry; blks} =
    if is_null entry then [] else
      normalize entry blks |>
      List.fold ~init:(None,[]) ~f:(fun (s,blks) b ->
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
  let blk ?(keep=false) ?(weak=false) tid =
    {name=tid; defs=[]; jmps=[]; keep; weak}

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

  let is_subinstruction label =
    KB.collect Insn.Seqnum.slot label >>|
    Option.is_some

  let relink label {entry; blks} =
    let* weak = is_subinstruction label in
    if is_null entry then KB.return {
        entry = label;
        blks = [{name=label; keep=true; weak; defs=[]; jmps=[]}]
      } else
      let+ blks = List.fold_map blks ~init:`Unbound ~f:(fun r blk ->
          if Theory.Label.equal blk.name entry
          then if blk.keep then `Relink blk.name, blk
            else `Relinked, {blk with name = label; keep=true; weak}
          else r,blk) |> function
                  | `Relinked,blks -> KB.return blks
                  | `Relink dst, blks ->
                    let+ tid = fresh in
                    blks @ [blk ~weak label ++ goto ~tid dst]
                  | `Unbound,[] -> assert false
                  | `Unbound,_ -> assert false      in
      {entry = label; blks}

  let set v x =
    x >>= fun x ->
    fresh >>= fun entry ->
    fresh >>= fun tid ->
    data {
      entry;
      blks = [{
          name=entry;
          keep=false;
          weak=true;
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
    let keep = true in
    cnd >>= fun cnd ->
    body >>| reify >>= function
    | {blks=[]} ->               (* empty denotation *)
      fresh >>= fun head ->
      fresh >>= fun tid ->
      data {
        entry = head;
        blks = [{
            name = head;
            keep;
            weak = false;
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
        blks = blk ~keep tail ++ goto ~tid:jmp1 ~cnd loop ::
               blk ~keep head ++ goto ~tid:jmp2 tail ::
               b ++ goto ~tid:jmp3 tail ::
               blks
      }

  let branch cnd yes nay =
    let keep = true in
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
          blk ~keep tail ::
          blk ~keep head ++
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
          blk ~keep tail ::
          blk ~keep head ++
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
          blk ~keep tail ::
          blk ~keep head ++
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
          blk ~keep tail;
          blk ~keep head ++ jump ~tid:jmp1 tail ++ goto ~tid:jmp2 tail
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

  let fall ~tid x dst = match x.jmps with
    | [jmp] when is_call jmp ->
      let jmp' = Jmp.with_dst jmp @@ Some (Jmp.resolved dst) in
      if is_unconditional jmp then {
        x with jmps = [jmp'];
      } else {
        x with jmps = [
          Jmp.create_goto (Direct dst);
          jmp';
        ]
      }
    | [jmp] when is_unconditional jmp -> x
    | jmps -> {x with jmps = goto ~tid dst :: jmps}

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
            fall ~tid x esnd ::
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
