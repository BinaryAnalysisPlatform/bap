open Core_kernel.Std
open Bap.Std
open Graphlib.Std

module SM = Monad.State
open SM.Monad_infix

let visit vis tid = Map.change vis tid ~f:(function
    | None   -> Some 1
    | Some n -> Some (n+1))

let create_mapping prog : word -> tid option =
  let addrs = Addr.Table.create () in
  let add t a = Hashtbl.set addrs ~key:a ~data:(Term.tid t) in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      Term.enum blk_t sub |> Seq.iter  ~f:(fun blk ->
          match Term.get_attr blk Disasm.block with
          | Some addr -> add blk addr
          | None -> ());
      match Term.get_attr sub subroutine_addr with
      | Some addr -> add sub addr
      | None -> ());
  Hashtbl.find addrs

type 's checkpoint = tid * 's

let merge_visited = Map.merge ~f:(fun ~key -> function
    | `Left x | `Right x -> Some x
    | `Both (x,y) -> Some (Int.max x y))



module Limit = struct
  (* virtual class allows us to omit type annotation
     for the update method in the subclasses  *)
  class virtual t = object(_ : 's)
    method virtual update : 't 'p. ('p,'t) cls -> 't term -> 's
    method virtual reached : bool
  end

  let max_trace_length jumps : t = object(self: 's)
    inherit t
    val limit = jumps
    method update cls t =
      Term.cata cls t ~init:self ~blk:(fun _ -> {< limit = limit - 1 >})
    method reached = limit <= 0
  end

  let backedges_of_sub sub : int Tid.Map.t =
    let module G = Graphs.Ir in
    Sub.to_cfg sub |> Graphlib.depth_first_search (module G)
      ~init:Tid.Map.empty
      ~enter_edge:(fun kind edge backedges -> match kind with
          | `Back -> Map.add backedges ~key:(G.Edge.tid edge) ~data:0
          | _ -> backedges)

  let max_loop_length limit : t = object(self : 's)
    inherit t
    val backedges = Tid.Map.empty
    val continue = true

    method update cls t =
      Term.cata cls t ~init:self
        ~sub:(fun b -> {< backedges = backedges_of_sub b >})
        ~jmp:(fun j ->
            let key = Term.tid j in
            match Map.find backedges key with
            | Some n -> {<
                continue = n <= limit;
                backedges = Map.add backedges ~key ~data:(n-1)
              >}
            | _ -> self)

    method reached = not continue
  end

  let nothing : t = object(self)
    inherit t
    method update _ _ = self
    method reached = false
  end

  let all ls = object
    inherit t
    val limits = ls
    method update cls t = {<
      limits = List.map limits ~f:(fun (lim : t) -> lim#update cls t)
    >}
    method reached =
      List.exists limits ~f:(fun limit -> limit#reached)
  end
end

module Crawler = struct

  class ['c] t = object(self : 's)
    val visited = Tid.Set.empty
    method save : tid -> 'c -> 's = fun _ _ -> self
    method visit t = {< visited = Set.add visited t >}
    method visited = visited
    method backtrack : ('s * 'c) option = None
  end

  (* don't store any checkpoints, no backtracking  *)
  class ['s] deterministic = object
    inherit ['s] t
  end

  (* store all checkpoints, and backtrack to them all *)
  class ['s] exponential = object(self)
    val checkpoints : 's Tid.Map.t = Tid.Map.empty
    inherit ['s] deterministic
    method! save tid ctxt =
      {< checkpoints = Map.add checkpoints ~key:tid ~data:ctxt >}

    method! backtrack = match Map.min_elt checkpoints with
      | None -> None
      | Some (k,v) ->
        Some ({< checkpoints = Map.remove checkpoints k>},v)
  end

  (* don't backtrack to visited destinations, same implementation as
     exponential, but prune visited destinations *)
  class ['s] linear = object
    inherit ['s] exponential as super
    method! visit tid =
      (super#visit tid)#prune tid
    method prune tid = {<
      checkpoints = Map.remove checkpoints tid
    >}
  end
end

type limit = Limit.t
type 's crawler = 's Crawler.t

class context limit crawler p = object(self : 's)
  inherit Biri.context p
  val limit : limit = limit
  val crawler : 's crawler = crawler
  method limit = limit
  method crawler = crawler
  method with_crawler c = {< crawler = c >}
  method with_limit : (#Limit.t as 'a) -> 's = fun l -> {< limit = l >}
end

class ['a] main = object(self)
  constraint 'a = #context
  inherit ['a] Biri.t as super

  method! enter_term cls t =
    let tid = Term.tid t in
    SM.update (fun (ctxt : 'a) ->
        let crawler = ctxt#crawler#visit tid in
        let limit = ctxt#limit#update cls t in
        (ctxt#with_crawler crawler)#with_limit limit) >>= fun () ->
    super#enter_term cls t

  method private stop = SM.update @@ fun ctxt -> ctxt#set_next None

  method private finish =
    SM.get () >>= fun ctxt ->
    match ctxt#crawler#backtrack with
    | None -> self#stop
    | Some (crawler,ctxt) ->
      SM.update @@ fun _ -> ctxt#with_crawler crawler

  method! eval_blk blk =
    super#eval_blk blk >>= fun () ->
    self#add_checkpoints blk >>= fun () ->
    SM.get () >>= fun ctxt ->
    if ctxt#limit#reached then self#finish
    else match ctxt#next with
      | None -> self#finish
      | _ -> SM.return ()

  method private add_checkpoints blk =
    Term.enum jmp_t blk |>
    Seq.fold ~init:(SM.return ()) ~f:(fun m jmp ->
        m >>= fun () -> self#next_of_jmp jmp >>= fun dst ->
        SM.update (fun ctxt ->
            let src = Term.tid jmp in
            let ctxt = ctxt#crawler#visit src |>
                       ctxt#with_crawler in
            ctxt#set_next dst |>
            ctxt#crawler#save src|>
            ctxt#with_crawler))

  method private next_of_jmp jmp =
    let goto dst =
      self#eval_goto dst >>= fun () ->
      SM.get () >>| fun ctxt -> ctxt#next in
    SM.get () >>= fun ctxt ->
    let next = match Jmp.kind jmp with
      | Int _ -> SM.return None
      | Goto dst | Ret dst -> goto dst
      | Call dst -> goto (Call.target dst) in
    SM.put ctxt >>= fun () ->
    next

  method! eval_indirect exp = self#stop

  method! eval_exn _ ret = self#eval_direct ret
end
