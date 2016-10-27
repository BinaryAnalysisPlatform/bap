open Core_kernel.Std
open Regular.Std
open Bap.Std
open Graphlib.Std
open Monads.Std



module Multi = struct
  type status = [`Current | `Live | `Dead]


  module type S2 = sig
    include Monad.Trans.S1
    type id

    module Id : Regular.S with type t = id


    val global : id

    val fork : unit -> (unit,'e) t
    val switch : id -> (unit,'e) t
    val parent : unit -> (id,'e) t
    val ancestor : id list -> (id,'e) t
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
        let fs = Map.to_sequence k.forks |> Seq.map ~f:fst in
        Seq.cons global fs)

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

  type 'a conflict = Conflict of assn list * 'a

  module type S2 = sig
    type ('a,'e) t

    type id

    val fork : blk term -> (id conflict list,'e)  t
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

    let fork (self : 'e #Expi.t) blk : (id conflict list,'e) SM.t =
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
          Conflict (assns,id))
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



module Scheduler = struct
  module type S = sig
    type  t
    type ('a,'e) m
    val schedule : t -> (t,#Biri.context) m
  end
end

(* continue with the same context, until a path terminates,
   then switch to a next thread that is not yet terminated.

   A thread  is terminated, if [ctxt#next = None]
*)
module Greedy = struct
  module Make(SM : Multi.S2) : Scheduler.S
    with type ('a,'e) m := ('a,'e) SM.t =  struct
    open SM.Syntax

    type ('a,'e) m = ('a,'e) SM.t constraint 'e = #Biri.context

    type t = unit

    let schedule () =
      SM.get () >>= fun ctxt -> match ctxt#next with
      | Some _ -> SM.return ()
      | None -> SM.forks () >>= SM.Seq.find ~f:(fun id ->
            SM.switch id >>= fun () ->
            SM.get () >>| fun ctxt -> match ctxt#next with
            | None -> false
            | Some _ -> true) >>| ignore
  end
end

module RR = struct
  module Make(SM : Multi.S2) : Scheduler.S = struct
    open SM.Syntax

    type ('a,'e) m = ('a,'e) SM.t

    type t = {
      pending : SM.id Fqueue.t
    }

    let rec schedule t = match Fqueue.dequeue t.pending with
      | None -> SM.forks () >>= fun fs -> schedule {
          pending = Seq.fold fs ~init:Fqueue.empty ~f:Fqueue.enqueue
        }
      | Some (next,pending) -> SM.status next >>= function
        | `Dead -> schedule {pending}
        | _ -> SM.switch next >>| fun () -> {pending}
  end
end

module type Rng = sig
  type t
  type dom
  val next  : t -> t
  val value : t -> dom
end

module LCG = struct
  type t = int
  type dom = int
  let a = 1103515245
  let c = 12345
  let m = 31
  let next x = (a * x + c) mod 31
  let value = ident
end

(* pick the next thread on a random basis *)
module Random = struct
  module Make
      (Dom : Int_intf.S)
      (Rng : Rng with type dom = Dom.t)
      (SM : Multi.S2) : Scheduler.S = struct
    open SM.Syntax

    let attempts = 4

    type ('a,'e) m = ('a,'e) SM.t

    type t = {
      cs : SM.id Dom.Map.t;
      rng : Rng.t;
    }

    let rec reschedule n t =
      if n > 0 then
        let rng = Rng.next t.rng in
        let c = Dom.(Rng.value rng % of_int_exn (Map.length t.cs)) in
        match Map.find t.cs c with
        | None -> reschedule (n-1) {rng; cs = Map.remove t.cs c}
        | Some id -> SM.status id >>= function
          | `Dead -> reschedule (n-1) {rng; cs = Map.remove t.cs c}
          | _ -> SM.switch id >>| fun () -> {t with rng}
      else
        SM.forks () >>| fun fs -> {
          t with
          cs = Seq.foldi fs ~init:Dom.Map.empty ~f:(fun i cs id ->
              Map.add cs ~key:(Dom.of_int_exn i) ~data:id)
        }

    let schedule t = reschedule attempts t

  end

  module Fast = Make(Int)(LCG)
end


(* will prefer threads that will wonder into less explored areas *)
module Explorer = struct

  module Make(SM : Multi.S2) = struct
    open SM.Syntax

    type t = {
      explored : int Tid.Map.t;
      pending  : SM.id Fqueue.t;
    }

    let enqueue level t id  =
      let add tid = {
        pending = Fqueue.enqueue t.pending id;
        explored = Map.add t.explored ~key:tid ~data:0
      } in
      SM.switch id >>= fun () ->
      SM.get () >>| fun ctxt -> match ctxt#next with
      | None -> t
      | Some tid -> match Map.find t.explored tid with
        | None -> add tid
        | Some 0 -> t
        | Some n when n = level -> add tid
        | Some _ -> t

    let remove_planned_explorations t = {
      t with explored = Map.filteri t.explored
                 ~f:(fun ~key ~data -> data <> 0)
    }

    let rec reschedule level t =
      SM.current () >>= fun id ->
      SM.forks () >>=
      SM.Seq.fold ~init:t ~f:(enqueue level) >>= fun t ->
      SM.switch id >>= fun () ->
      if Fqueue.is_empty t.pending then reschedule (level+1) t
      else SM.return @@ remove_planned_explorations t

    let rec schedule t =
      match Fqueue.dequeue t.pending with
      | None -> reschedule 0 t >>= schedule
      | Some (id,pending) ->
        SM.get () >>= fun ctxt -> match ctxt#trace with
        | [] -> SM.switch id >>| fun () -> {t with pending}
        | curr::_ -> SM.switch id >>| fun () -> {
            pending;
            explored = Map.update t.explored curr ~f:(function
                | None -> 1
                | Some n -> n+1)
        }
  end
end

module Context = struct
  module Level = struct
    type nil = Nil
    type top = program

    type ('a,'b) level = {
      me : 'a term;
      up : 'b;
    }

    type level3 = (top,nil) level
    type level2 = (sub,level3) level
    type 'a level1 = ('a,level2) level
    type 'a level0 = ('a,blk level1) level

    type t =
      | Top of level3
      | Sub of level2
      | Arg of arg level1
      | Blk of blk level1
      | Phi of phi level0
      | Def of def level0
      | Jmp of jmp level0

    type name = [`top | `sub | `arg | `blk | `phi | `def | `jmp]
    type invariant = {
      level : t;
      dst : name
    }

    exception Broken_invariant of invariant
    let broken_invariant level dst = raise (Broken_invariant {level; dst})
  end
  open Level


  class t ?main proj =
    let prog = Project.program proj in
    object(self : 's)
      inherit Biri.context ?main prog
      val level = Top {me=prog; up=Nil}
      method project = proj
      method curr =
        let (!) {me} = Term.tid me in
        match level with
        | Top t -> !t | Sub t -> !t | Arg t -> !t | Blk t -> !t
        | Phi t -> !t | Def t -> !t | Jmp t -> !t
      method step : type p t. (p,t) cls -> t term -> 's option =
        Term.switch
          ~program:(fun p -> match level with
              | Top _ -> Some {< level = Top {me=p; up=Nil} >}
              | _ -> None)
          ~sub:(fun sub -> match level with
              | Top top | Sub {up=top} ->
                Some {< level = Sub {me=sub; up=top} >}
              | _ -> None)
          ~arg:(fun arg -> match level with
              | Sub sub | Blk {up=sub} ->
                Some {< level = Arg {me=arg; up=sub} >}
              | _ -> None)
          ~blk:(fun blk -> match level with
              | Blk {up=sub} | Sub sub | Arg {up=sub} ->
                Some {< level = Blk {me=blk; up=sub} >}
              | _ -> None)
          ~phi:(fun phi -> match level with
              | Blk blk | Phi {up=blk} ->
                Some {< level = Phi {me=phi; up=blk} >}
              | _ -> None)
          ~def:(fun def -> match level with
              | Blk blk | Phi {up=blk} | Def {up=blk}->
                Some {< level = Def {me=def; up=blk} >}
              | _ -> None)
          ~jmp:(fun jmp -> match level with
              | Blk blk | Phi {up=blk} | Def {up=blk} | Jmp {up=blk} ->
                Some {< level = Jmp {me=jmp; up=blk} >}
              | _ -> None)

    end
end

module type State = sig
  type ('a,'e) m
  type 'a t
  val create : ?observe:('a -> Sexp.t) -> name:string -> (Context.t -> 'a) -> 'a t

  val get : 'a t -> ('a,#Context.t) m
  val put : 'a t -> 'a -> (unit,#Context.t) m
  val update : 'a t -> f:('a -> 'a) -> (unit,#Context.t) m
end

module type Machine = sig
  type ('a,'e) t

  module Local  : State with type ('a,'e) m = ('a,'e) t
  module Global : State with type ('a,'e) m = ('a,'e) t

  include Multi.S2 with type ('a,'e) t := ('a,'e) t
end


module Values = struct

  module type S = sig
    type ('a,'e) m constraint 'e = #Context.t
    val step : ('p,'t) cls -> 't term -> (unit,'e) m
  end
end

module Storage = struct

  type cell =
    | Addr of addr
    | Var  of var

  module type S = sig
    type ('a,'e) m constraint 'e = #Context.t
    val load : cell -> (word option,'e) m
    val save : cell -> (unit,'e) m
  end

  module type T = functor (M : Multi.S2) -> S with type ('a,'e) m = ('a,'e) M.t
  type t = (module T)



  module Null(SM : Multi.S2) = struct
    type ('a,'e) m = ('a,'e) SM.t constraint 'e = #Context.t
    let load _ = SM.return None
    let save _ = SM.return ()
  end

  module Basic(SM : Machine) = struct
    type ('a,'e) m = ('a,'e) SM.t constraint 'e = #Context.t
    type storage = {
      regs : word Var.Map.t;
      vars : word Var.Map.t;
      data : word Addr.Map.t;
    }

    let state = SM.Local.create ~name:"basic.storage" (fun ctxt ->
        SM.return {
          regs = Var.Map.empty;
          vars = Var.Map.empty;
          data = Addr.Map.empty;
        })

    type t = storage option
  end

  let null : t = (module Null)
end


module Machine = struct
  module type S2 = Machine

  module Make(M : Monad.S) = struct
    module Multi : Machine = struct
      module SM = struct
        include Multi.T2(M)
        include Multi.Make2(M)
      end
      type 'e state = {
        ctxt : 'e;
        local : Univ_map.t;
        global : Univ_map.t;
      }
      type ('a,'e) t = ('a,'e state) SM.t
      type ('a,'e) e = ('a,'e) SM.e
      type 'a m = 'a M.t
      module Basic = struct
        type ('a,'e) t = ('a,'e state) SM.t
        let return = SM.return
        let bind = SM.bind
        let map = `Custom SM.map
      end
      include Monad.Make2(Basic)

      type id = Multi.id
      module Id = Multi.Id

      let with_global_context f =
        SM.current ()       >>= fun id ->
        SM.switch SM.global >>= fun () ->
        f ()                >>= fun r  ->
        SM.switch id        >>| fun () ->
        r

      let get_local () = SM.gets @@ fun s -> s.local
      let get_global () = with_global_context @@ fun () ->
          SM.gets @@ fun s -> s.global

      let set_local local = SM.update @@ fun s ->
        {s with local}

      let set_global global = with_global_context @@ fun () ->
        SM.update @@ fun s -> {s with global}

      module State(S : sig
          val get : unit -> (Univ_map.t,'e) t
          val set : Univ_map.t -> (unit,'e) t
          val typ : string
        end) = struct
        module Dict = Univ_map
        module Key = Dict.Key
        type ('a,'e) m = ('a,'e) ts
        type 'a t = {
          key : 'a Key.t;
          init : Context.t -> 'a; (* must be total...*)
        }

        let create ?(observe=sexp_of_opaque) ~name init =
          let sexp x = Sexp.List [
              Sexp.Atom (sprintf "%s:%s" name S.typ);
              observe x;
            ] in {
            key = Key.create ~name sexp;
            init
          }

        let get data =
          S.get () >>= fun d ->
          match Dict.find d data.key with
          | Some r -> return r
          | None -> SM.get () >>= fun {ctxt} ->
            return (data.init (ctxt :> Context.t))

        let put data x =
          S.get () >>= fun d -> S.set (Dict.set d data.key x)

        let update data ~f =
          get data >>= fun s -> put data (f s)
      end

      module Local = State(struct
          let typ = "local"
          let get = get_local
          let set = set_local
        end)

      module Global = State(struct
          let typ = "global"
          let get = get_global
          let set = set_global
        end)

      let put ctxt = SM.update @@ fun s -> {s with ctxt}
      let get () = SM.gets @@ fun s -> s.ctxt
      let gets f = get () >>| f
      let update f = get () >>= fun s -> put (f s)
      let modify m f = m >>= fun x -> update f >>= fun () -> SM.return x

      let run m ctxt =
        M.bind (SM.run m {
            global = Univ_map.empty;
            local = Univ_map.empty;
            ctxt}) @@ fun (x,{ctxt}) -> M.return (x,ctxt)

      let eval m s = M.map (run m s) ~f:fst
      let exec m s = M.map (run m s) ~f:snd
      let lift = SM.lift
      let status = SM.status
      let forks = SM.forks
      let kill = SM.kill
      let fork = SM.fork
      let ancestor = SM.ancestor
      let parent = SM.parent
      let switch = SM.switch
      let global = SM.global
      let current = SM.current
    end


  end

  open Multi.Syntax

  module Biri = Biri.Make(Multi)

end
