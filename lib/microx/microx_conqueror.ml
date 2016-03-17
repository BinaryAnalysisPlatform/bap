open Core_kernel.Std
open Bap.Std

module SM = Monad.State
open SM.Monad_infix

open Format

let debug_enabled = ref false

let fdebug ppf = match debug_enabled with
  | {contents=true} -> fprintf ppf
  | {contents=false} -> ifprintf ppf

let debug fmt = fdebug Format.err_formatter fmt

let def_summary _ = None


let skip () = SM.return false
let pass () = SM.return true

let summaries = String.Map.of_alist_exn [
  ]

let def_summary call = match Call.target call with
  | Indirect _ -> None
  | Direct tid ->
    Map.find summaries (Tid.name tid)

let target_of_goto jmp = match Jmp.kind jmp with
  | Goto (Direct tid) -> Some tid
  | _ -> None

let visit vis tid = Map.change vis tid ~f:(function
    | None   -> Some 1
    | Some n -> Some (n+1))

let create_mapping prog =
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

class context
    ?(max_steps=Int.max_value)
    ?(max_loop= min 10 (max_steps / 10)) p  = object(self : 's)
  inherit Biri.context p

  val blk : blk term option = None
  val callstack : sub term list = []
  val steps_left = max_steps
  val vis : int Tid.Map.t = Tid.Map.empty (* visited *)
  val cps : 's checkpoint list Tid.Map.t = Tid.Map.empty
  val rets : tid list = []

  method visited = vis
  method checkpoints = cps

  method backtrack = match List.hd callstack with
    | None -> None
    | Some sub ->
      let key = Term.tid sub in
      match Map.find cps key with
      | None -> None
      | Some ps -> match List.filter ps ~f:(fun (p,_) -> not(Map.mem vis p)) with
        | [] -> None
        | (p,ctxt) :: ps ->
          let self = {< cps = Map.add cps ~key ~data:ps >} in
          let self = ctxt#merge self in
          Some (self#set_next (Some p))

  method add_checkpoint p = match List.hd callstack with
    | None -> self
    | Some sub ->
      let key = Term.tid sub in
      if Map.mem vis p then self
      else {< cps = Map.add_multi cps ~key ~data:(p,self) >}

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
      let will_jump = Term.find blk_t cur tid <> None in
      let will_call = Term.find sub_t   p tid <> None in
      not(will_jump || will_call)
    | _ -> false
end

let update f =
  SM.get () >>= fun s -> SM.put (f s)

class ['a] main ?(deterministic=false) p =
  object(self)
    constraint 'a = #context
    inherit ['a] Biri.t as super

    method! enter_term cls t =
      let tid = Term.tid t in
      update (fun ctxt -> ctxt#visit_term tid) >>= fun () ->
      super#enter_term cls t

    method! eval_blk blk =
      if Term.length jmp_t blk = 0
      then self#return
      else
        update (fun ctxt -> ctxt#enter_blk blk) >>= fun () ->
        self#add_checkpoints >>= fun () ->
        super#eval_blk blk

    method! eval_sub sub =
      update (fun ctxt -> ctxt#enter_sub sub) >>= fun () ->
      super#eval_sub sub >>= fun () ->
      update (fun ctxt -> ctxt#leave_sub sub)

    method! eval_jmp jmp =
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

    method private break = update @@ fun ctxt -> ctxt#set_next None

    method private return =
      update @@ fun ctxt -> match ctxt#backtrack with
      | None -> ctxt#return
      | Some next -> next

    method private add_checkpoints =
      SM.get () >>= fun ctxt ->
      match ctxt#blk with
      | Some blk when not deterministic ->
        Term.enum jmp_t blk |>
        Seq.fold ~init:(SM.return ()) ~f:(fun m jmp ->
            m >>= fun () ->
            update (fun ctxt -> ctxt#visit_term (Term.tid jmp)) >>= fun () ->
            self#next_of_jmp jmp >>= function
            | None -> SM.return ()
            | Some tid ->
              update (fun ctxt -> ctxt#add_checkpoint tid))
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
        update (fun ctxt -> ctxt#store_return ret) >>= fun () ->
        super#eval_call call

  end
