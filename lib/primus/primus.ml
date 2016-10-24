open Core_kernel.Std
open Regular.Std
open Bap.Std
open Graphlib.Std
open Monads.Std


module Multi = struct

  module type S2 = sig
    include Monad.Trans.S1
    type id

    module Id : Regular.S with type t = id


    type status = [`Current | `Live | `Dead]

    val global : id

    val fork : unit -> (unit,'e) t
    val switch : id -> (unit,'e) t
    val parent : unit -> (id,'e) t
    val ancestor : id list -> (id,'e) t
    val siblings : unit -> (Id.Set.t,'e) t
    val current : unit -> (id,'e) t
    val kill : id -> (unit,'e) t
    val forks : unit -> (id seq,'e) t
    val status : id -> (status,'e) t

    include Monad.State.S2 with type ('a,'e) t := ('a,'e) t
                            and type ('a,'e) e := ('a,'e) e
                            and type 'a m := 'a m
  end

  module Id = struct
    type t = int
    let zero = Int.zero
    let succ = Int.succ
    include Regular.Make(struct
      type t = int [@@deriving compare, bin_io, sexp]
      let version = "1.0"
      let hash = Int.hash
      let module_name = Some "Primus.Std"
      let pp = Int.pp
    end)
  end
  type id = Id.t


  type 'e contexts = {
    created : id;             (* the id of last created fork *)
    current : id;             (* the id of current context   *)
    parents : id Id.Map.t;    (* tree of forks               *)
    children : Id.Set.t Id.Map.t;
    init  : 'e;                (* father of all forks         *)
    forks : 'e Id.Map.t;       (* all forks of the Father     *)
  }

  module T2(M : Monad.S) = struct
    type ('a,'e) t = ('a,'e contexts) Monad.State.T2(M).t
    type ('a,'e) e = ('a,'e) Monad.State.T2(M).e
    type 'a m = 'a M.t
  end

  module Make2(M : Monad.S) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type ('a,'e) e := ('a,'e) T2(M).e
     and type 'a m := 'a T2(M).m
     and type id := id
     and module Id := Id
  = struct
    module SM = struct
      include Monad.State.T2(M)
      include Monad.State.Make2(M)
    end


    open SM.Syntax
    type 'a m = 'a M.t
    type ('a,'e) state = ('a,'e contexts) SM.t
    type id = Id.t
    type ('a,'e) t = ('a,'e) state
    type ('a,'e) e = ('a,'e) SM.e
    type status = [`Current | `Live | `Dead]

    let global = Id.zero
    let init ctxt = {
      init = ctxt;
      created = global;
      current = global;
      parents = Id.Map.empty;
      children = Id.Map.empty;
      forks = Id.Map.empty;
    }

    let rec alive_parent k child =
      match Map.find k.parents child with
      | None -> global
      | Some p when Map.mem k.forks p -> p
      | Some zombie -> alive_parent k zombie

    let rec ancestor k cs =
      match List.map cs ~f:(alive_parent k) with
      | [] -> global
      | p :: ps when List.for_all ps ~f:(fun p' -> p = p') -> p
      | ps -> ancestor k ps

    let ancestor cs = SM.gets @@ fun k -> ancestor k cs

    let alive k id : id  =
      if Map.mem k.forks id then id else alive_parent k id

    let forks () = SM.gets (fun k ->
        Map.to_sequence k.forks |> Seq.map ~f:fst)

    let siblings () =
      SM.gets (fun k ->
          match Map.find k.children (alive_parent k k.current) with
          | None -> Id.Set.empty
          | Some cs -> Set.filter cs ~f:(Map.mem k.forks))

    let context k =
      let id = alive k k.current in
      if id = global then k.init
      else Map.find_exn k.forks k.current

    let fork () = SM.update @@ fun k ->
      let ctxt = context k in
      let current = Id.succ k.created in
      let parents = Map.add k.parents ~key:current ~data:k.current in
      let forks = Map.add k.forks ~key:current ~data:ctxt in
      {k with created=current; current; parents; forks}

    let switch id = SM.update @@ fun k -> {k with current = alive k id}
    let current () = SM.gets @@ fun k -> alive k k.current

    let get () : ('e,'e) t = SM.get () >>| context
    let put ctxt = SM.update @@ fun k ->
      let id = alive k k.current in
      if id = global then {k with init=ctxt} else {
        k with forks = Map.add k.forks ~key:id ~data:ctxt
      }

    let parent () = SM.get () >>| fun k -> alive_parent k k.current

    let status id = SM.get () >>| fun k ->
      if id = k.current then `Current else
      if Map.mem k.forks id then `Live else `Dead

    let remove_dead_parents k =
      let parents =
        Map.fold k.forks ~init:Id.Map.empty ~f:(fun ~key ~data:_ ps ->
              Map.add ps ~key ~data:(alive_parent k key)) in
      let children = Map.filter_keys k.children ~f:(Map.mem parents) in
      {k with children; parents}

    let gc k =
      if k.created mod 1024 = 0 then remove_dead_parents k else k

    let kill id = SM.update @@ fun k ->
      gc {
        k with
        forks = Map.remove k.forks id;
        current = if id = k.current then alive_parent k id else id;
      }

    let gets f = get () >>| f
    let update f = get () >>= fun x -> put (f x)
    let modify m f = m >>= fun x -> update f >>= fun () -> SM.return x


    let run (m : ('a,'e) state) = fun ctxt -> M.bind (SM.run m (init ctxt)) @@ fun (x,cs) ->
      M.return (x,cs.init)

    include Monad.Make2(struct
        type ('a,'e) t = ('a,'e) state
        let return = SM.return
        let bind = SM.bind
        let map = `Custom SM.map
      end)

    let lift m = SM.lift m
    let eval m s = M.map (run m s) ~f:fst
    let exec m s = M.map (run m s) ~f:snd
    module Id = Id
  end

  include T2(Monad.Ident)
  include Make2(Monad.Ident)
end


(*

   In a general case a block is terminated by a sequence of jumps:

   {v
     when C1 jmp D1
     when C2 jmp D2
     ...
     when Cm jmp Dm
   v}

   The IR requires the following invariant:

   {v C1 \/ C2 \/ ... \/ Cm v}.

   The infeasible interpreter is a non-deterministic interperter, that
   for every block B that is terminated with m jumps, will fork m
   context after a last definition, so that under the n-th context
   the Dn destination will be taken by the interpreter.

   For the Dm-th destination to be taken, the following condition must
   hold: {v ~C1 /\ ~C2 /\ ... ~C(n-1) /\ Cn v}, where [~] symbol
   denotes logical negation.

   However, we would require an SMT solver to find such contexts. So,
   the intepreter requires a program to be in a trivial condition form
   (TCF). In TCF every jmp condition must be a single variable or a
   constant true.


*)
module Infeasible = struct

  type assn = {
    use : tid;
    var : var;
    res : bool;
  }

  type 'a assm = Assume of assn list * 'a

  module type S2 = sig
    type ('a,'e) t

    type id

    val fork : blk term -> (id assm list,'e)  t
  end

  (* pre: number of jumps is greater than 1
     post: number of jumps is the same, each jump is in TCF.*)
  let blk blk =
    Term.enum jmp_t blk |>
    Seq.fold ~init:(Term.filter jmp_t blk ~f:(fun _ -> true))
      ~f:(fun blk jmp -> match Jmp.cond jmp with
          | Bil.Int _ | Bil.Var _ -> Term.append jmp_t blk jmp
          | cond ->
            let var =
              Var.create ~is_virtual:true ~fresh:true "c" bool_t in
            let def = Def.create var cond in
            let blk = Term.append def_t blk def in
            let jmp = Jmp.with_cond jmp (Bil.var var) in
            Term.append jmp_t blk jmp)

  let sub = Term.map blk_t ~f:(fun b ->
      if Term.length jmp_t b = 1 then b else blk b)

  let prog = Term.map sub_t ~f:sub


  let neg = List.map ~f:(fun assn -> {assn with res = not assn.res})

  let assumptions blk =
    Term.enum jmp_t blk |> Seq.fold ~init:([],[])
      ~f:(fun (assns, assms) jmp -> match Jmp.cond jmp with
          | Bil.Var c ->
            let assn = {use=Term.tid jmp; var=c; res=true} in
            assn :: assns, (assn :: neg assns) :: assms
          | Bil.Int _ -> assns, (neg assns) :: assms
          | _ -> (assns,assms)) |> snd



  module Make(SM : Multi.S2) = struct
    open SM.Syntax
    type id = SM.id
    module Expi = Expi.Make(SM)

    let apply self  =
      SM.List.iter ~f:(fun assn ->
          self#eval_int (Word.of_bool assn.res) >>=
          self#update assn.var)

    let fork (self : 'e #Expi.t) blk : (id assm list,'e) SM.t =
      SM.List.map (assumptions blk)
        ~f:(SM.List.filter ~f:(fun assn ->
              let exp = Word.of_bool assn.res in
              self#lookup assn.var >>| Bil.Result.value >>| function
              | Bil.Imm r -> Word.(r <> exp)
              | _ -> true)) >>=
      SM.List.map ~f:(fun assns ->
          SM.fork () >>= fun () ->
          apply self assns >>= fun () ->
          SM.current () >>= fun id ->
          SM.parent () >>= fun pid ->
          SM.switch pid >>| fun () ->
          Assume (assns,id))
  end


  include Make(Multi)
end

module Concretizer = struct

  module Make(SM : Monad.State.S2) = struct

  end
end

(* now we can implement different execution policies as different
   scheduling strategies. However, we need to establish some abstract
   language, between an intepreter that will obey to the scheduler,
   and the schedulers.
*)


(* continue with the same context, until a path terminates,
   then backtrack to the closest relative
 *)
module Greedy = struct
  module Make(SM : Multi.S2) = struct
    open SM.Syntax

    type state = {
      finished : SM.Id.Set.t;
    }

    let schedule {finished} =
      SM.get () >>= fun ctxt -> match ctxt#next with
      | Some _ -> SM.return {finished}
      | None ->
        SM.current () >>= fun id ->
        let finished = Set.add finished id in
        SM.siblings () >>= fun ids ->
        let ids = Set.diff ids finished in
        match Set.choose ids with
        | None -> SM.return {finished}
        | Some id -> SM.switch id >>| fun () -> {finished}
  end
end
