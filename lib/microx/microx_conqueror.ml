open Core_kernel.Std
open Bap.Std

module SM = Monad.State
open SM.Monad_infix
open Format

let visit vis tid = Map.change vis tid ~f:(function
    | None   -> Some 1
    | Some n -> Some (n+1))

let create_mapping prog =
  let addrs = Addr.Table.create () in
  let add t a = Hashtbl.set addrs ~key:a ~data:(Term.tid t) in
  Term.enum sub_t prog |> Seq.iter ~f:(fun sub ->
      Term.enum blk_t sub |> Seq.iter  ~f:(fun blk ->
          match Term.get_attr blk address with
          | Some addr -> add blk addr
          | None -> ());
      match Term.get_attr sub address with
      | Some addr -> add sub addr
      | None -> ());
  Hashtbl.find addrs

type 's checkpoint = tid * 's

let merge_visited = Map.merge ~f:(fun ~key -> function
    | `Left x | `Right x -> Some x
    | `Both (x,y) -> Some (Int.max x y))

class context
    ?(max_steps=Int.max_value)
    ?(max_loop= min 10 (max_steps / 10)) p  = object(self : 's)
  inherit Biri.context p

  val blk : blk term option = None
  val callstack : sub term list = []
  val steps_left = max_steps
  val vis : int Tid.Map.t = Tid.Map.empty (* visited *)
  val cps : 's Tid.Map.t Tid.Map.t = Tid.Map.empty
  val rets : tid list = []

  method visited = vis
  method checkpoints = cps

  method backtrack : 's option = match List.hd callstack with
    | None -> None
    | Some sub ->
      let key = Term.tid sub in
      match Map.find cps key with
      | None -> None
      | Some ps ->
        let ps = Map.filter_keys ps ~f:(fun p -> not(Map.mem vis p)) in
        match Map.min_elt ps with
        | None -> None
        | Some (p,old) ->
          let ps = Map.remove ps p in
          let self = {< cps = Map.add cps ~key ~data:ps >} in
          let old = old#merge self in
          Some (old#set_next (Some p))

  method add_checkpoint p = match List.hd callstack with
    | None -> self
    | Some sub ->
      let key = Term.tid sub in
      if Map.mem vis p then self
      else {<
        cps = Map.update cps key ~f:(function
            | None -> Tid.Map.singleton p self
            | Some ps -> Map.add ps p self)
      >}

  method merge runner = {<
    vis = runner#visited;
    cps = runner#checkpoints;
  >}

  method store_return ret = {< rets = ret :: rets >}

  method return = match rets with
    | [] -> self#set_next None
    | r :: rs -> {< rets = rs >}#set_next (Some r)

  method blk = blk
  method enter_blk blk = {< blk = Some blk >}
  method enter_sub sub = {< callstack = sub :: callstack >}

  method leave_sub (_ : sub term) = match callstack with
    | _ :: top -> {< callstack = top >}
    | [] -> {< callstack = [] >}

  method step =
    if steps_left > 0
    then Some {< steps_left = steps_left - 1 >}
    else None

  method visit_term tid = {< vis = visit vis tid >}

  method will_loop tid = match callstack with
    | [] -> false
    | sub :: _ -> match Map.find vis tid with
      | None -> false
      | Some n -> n > max_loop && Term.find blk_t sub tid <> None

  method will_return tid = match callstack with
    | cur :: _ ->
      let blk = Option.value_exn blk in
      let will what = what tid <> None in
      let fall = Term.find jmp_t blk in
      let jump = Term.find blk_t cur in
      let call = Term.find sub_t   p in
      not(will fall || will jump || will call)
    | _ -> false
end

class ['a] main ?(deterministic=false) p =
  object(self)
    constraint 'a = #context
    inherit ['a] Biri.t as super

    method! enter_term cls t =
      let tid = Term.tid t in
      SM.update (fun ctxt -> ctxt#visit_term tid) >>= fun () ->
      super#enter_term cls t

    method! eval_blk blk =
      SM.update (fun ctxt -> ctxt#enter_blk blk) >>= fun () ->
      super#eval_blk blk

    method! eval_sub sub =
      SM.update (fun ctxt -> ctxt#enter_sub sub) >>= fun () ->
      super#eval_sub sub >>= fun () ->
      SM.update (fun ctxt -> ctxt#leave_sub sub)

    method! eval_jmp jmp =
      self#add_checkpoints >>= fun () ->
      SM.get () >>= fun ctxt ->
      match ctxt#step with
      | None -> self#break
      | Some ctxt ->
        SM.put ctxt >>= fun () ->
        super#eval_jmp jmp >>= fun () ->
        SM.get () >>= fun ctxt ->
        match ctxt#next with
        | Some dst when ctxt#will_loop dst -> self#return
        | Some dst when ctxt#will_return dst -> self#return
        | Some dst -> SM.return ()
        | None -> self#return

    method private break = SM.update @@ fun ctxt -> ctxt#set_next None

    method private return =
      SM.update @@ fun ctxt -> match ctxt#backtrack with
      | None -> ctxt#return
      | Some next -> next

    method private add_checkpoints =
      SM.get () >>= fun ctxt ->
      match ctxt#blk with
      | Some blk when not deterministic ->
        Term.enum jmp_t blk |>
        Seq.fold ~init:(SM.return ()) ~f:(fun m jmp ->
            m >>= fun () ->
            SM.update (fun ctxt -> ctxt#visit_term (Term.tid jmp)) >>= fun () ->
            self#next_of_jmp jmp >>= function
            | None -> SM.return ()
            | Some tid -> SM.update (fun ctxt -> ctxt#add_checkpoint tid))
      | _ -> SM.return ()

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

    method! eval_indirect exp = self#break

    method! eval_exn _ ret = self#eval_direct ret

    method! eval_call call =
      match Call.return call with
      | None | Some (Indirect _) -> self#break
      | Some (Direct ret) ->
        SM.update (fun ctxt -> ctxt#store_return ret) >>= fun () ->
        super#eval_call call

  end
