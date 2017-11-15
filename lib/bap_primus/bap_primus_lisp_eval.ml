module Lisp(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)
  module Eval = Bap_primus_interpreter.Make(Machine)
  module Vars = Locals(Machine)
  module Env = Bap_primus_env.Make(Machine)
  module Mem = Bap_primus_memory.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)

  let error kind = Format.ksprintf
      (fun msg -> fun () -> Machine.raise (Runtime_error msg))

  let failf fmt = error (fun m -> Runtime_error m) fmt
  let linkerf fmt = error (fun m -> Link_error m) fmt

  let word width value typ =
    let width = match typ with
      | Word -> width
      | Type n -> n in
    Word.of_int64 ~width value

  let var width {data;typ} =
    let typ = match typ with
      | Word -> Type.Imm width
      | Type n -> Type.Imm n in
    Var.create data typ

  let width () = Machine.Local.get state >>| fun {width} -> width


  let stack_slot exp =
    let open Bil.Types in match exp with
    | BinOp (PLUS, Var sp, Int off) -> Some (sp,`down,off)
    | BinOp (MINUS,Var sp, Int off) -> Some (sp,`up,off)
    | _ -> None

  let update_frame slot addr n =
    match slot, stack_slot addr with
    | slot,None -> slot
    | None, Some (sp,dir,off) ->
      Some (sp,dir,Word.(off ++ Size.in_bytes n))
    | Some (sp,dir,off), Some (sp',dir',off') ->
      if Var.same sp sp' && dir = dir' then
        let off = Word.max off off' in
        Some (sp,dir,Word.(off ++ Size.in_bytes n))
      else Some (sp,dir,off)


  let find_max_slot =
    Seq.fold ~init:None ~f:(fun slot arg ->
        match Arg.rhs arg with
        | Bil.Load (_,addr,_,n) -> update_frame slot addr n
        | _ -> slot)

  let allocate_stack_frame args =
    match find_max_slot args with
    | None -> Machine.return None
    | Some (sp,dir,max) ->
      let sign = if dir = `down then Bil.MINUS else Bil.PLUS in
      Eval.get sp >>= fun sp_value ->
      Eval.const max >>= fun frame_size ->
      Eval.binop sign sp_value frame_size >>=
      Eval.set sp >>= fun () ->
      Machine.return (Some (sp,sp_value))


  let bil_of_lisp op =
    let open Bil in
    let binop = Eval.binop in
    match op with
    | Add  -> binop plus
    | Sub  -> binop minus
    | Mul  -> binop times
    | Div  -> binop divide
    | Mod  -> binop modulo
    | Divs -> binop sdivide
    | Mods -> binop smodulo
    | Lsl  -> binop lshift
    | Lsr  -> binop rshift
    | Asr  -> binop arshift
    | Land  -> binop AND
    | Lior   -> binop OR
    | Lxor  -> binop XOR
    | Cat  -> Eval.concat
    | Equal   -> binop eq
    | Less   -> binop le
    | And | Or -> assert false


  let eval_sub : value list -> 'x = function
    | [] -> failf "invoke-subroutine: requires at least one argument" ()
    | sub_addr :: sub_args ->
      Machine.get () >>= fun proj ->
      Term.enum sub_t (Project.program proj) |>
      Seq.find ~f:(fun sub -> match Term.get_attr sub address with
          | None -> false
          | Some addr -> Word.(addr = sub_addr.value)) |> function
      | None ->
        failf "invoke-subroutine: no function for %a" Addr.pps
          sub_addr.value ()
      | Some sub ->
        let args = Term.enum arg_t sub in
        allocate_stack_frame args >>= fun frame ->
        Seq.zip args (Seq.of_list sub_args) |>
        Machine.Seq.iter ~f:(fun (arg,x) ->
            let open Bil.Types in
            if Arg.intent arg <> Some Out
            then match Arg.rhs arg with
              | Var v -> Eval.set v x
              | Load (_,BinOp (op, Var sp, Int off),endian,size) ->
                Eval.get sp  >>= fun sp ->
                Eval.const off >>= fun off ->
                Eval.binop op sp off >>= fun addr ->
                Eval.store addr x endian size
              | exp ->
                failf "%s: can't pass argument %s - %s %a"
                  "invoke-subroutine" (Arg.lhs arg |> Var.name)
                  "unsupported ABI" Exp.pps exp ()
            else Machine.return ()) >>= fun () ->
        Linker.exec (`addr sub_addr.value) >>= fun () ->
        Machine.Seq.find_map args ~f:(fun arg ->
            if Arg.intent arg = Some Out
            then Eval.get (Arg.lhs arg) >>| Option.some
            else Machine.return None) >>= fun rval ->
        let teardown_frame = match frame with
          | Some (sp,bp) -> Eval.set sp bp
          | None -> Machine.return () in
        teardown_frame >>= fun () -> match rval with
        | None -> Eval.const Word.b0
        | Some rval -> Machine.return rval

  let rec eval_lisp name args : value Machine.t =
    Machine.get () >>= fun proj ->
    let arch = Project.arch proj in
    Machine.Local.get state >>= fun s ->
    match Resolve.defun s.contexts s.program.defs name arch args with
    | {stage1=[]},None ->
      eval_primitive name args
    | resolution,None ->
      Machine.raise (Resolve.Failed (name, s.contexts, resolution))
    | _,Some (fn,bs) ->
      Eval.const Word.b0 >>= fun init ->
      Machine.Observation.make Bap_primus_linker.will_exec (`symbol name) >>= fun () ->
      eval_advices `before init name args >>= fun _ ->
      Machine.Local.put state {s with env = bs @ s.env} >>= fun () ->
      Machine.Observation.make Trace.entered (fn,bs) >>= fun () ->
      eval_exp fn.code.body >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop (List.length bs)) >>= fun () ->
      Machine.Observation.make Trace.left ((fn,bs),r) >>= fun () ->
      eval_advices `after r name args

  and eval_advices stage init primary args =
    Machine.Local.get state >>= fun {program={advices}} ->
    match Map.find advices primary with
    | Some advices ->
      Machine.List.fold advices ~init ~f:(fun r (s,name) ->
          if s = stage
          then match stage with
            | `after ->  eval_lisp name (args @ [r])
            | `before -> eval_lisp name args
          else Machine.return r)
    | None -> Machine.return init

  and eval_primitive name args =
    Machine.Local.get state >>= fun {contexts; primitives} ->
    let defs = List.concat_map primitives (fun (module Make) ->
        let module Primitives = Make(Machine) in
        Primitives.defs ()) in
    match Resolve.primitive contexts defs name with
    | _,None -> failf "unresolved name %s" name ()
    | _,Some (def,_) ->
      Eval.const Word.b0 >>= fun init ->
      eval_advices `before init name args >>= fun _ ->
      def.code args >>= fun r ->
      eval_advices `after r name args

  and eval_exp exp  =
    let int v t = width () >>= fun width ->
      Eval.const (word width v t) in
    let rec eval = function
      | Int {arg={data;typ}} -> int data typ
      | Var {arg=v} -> lookup v
      | Ite {arg=(c,e1,e2)} -> ite c e1 e2
      | Let {arg=(v,e1,e2)} -> let_ v e1 e2
      | Ext {arg=(hi,lo,e)} -> ext hi lo e
      | App {arg=(n,args)} -> app n args
      | Rep {arg=(c,e)} -> rep c e
      | Bop {arg=(op,e1,e2)} -> bop op e1 e2
      | Uop {arg=(op,e)} -> uop op e
      | Seq {arg=es} -> seq es
      | Set {arg=(v,e)} -> eval e >>= set v
      | Msg {arg=(fmt,es)} -> msg fmt es
      | Err {arg=msg} -> Machine.raise (Runtime_error msg)
    and rep c e =
      eval c >>= function {value} as r ->
        if Word.is_zero value then Machine.return r
      else eval e >>= fun _ -> rep c e
    and ite c e1 e2 =
      eval c >>= fun {value=w} ->
      if Word.is_zero w then eval e2 else eval e1
    and let_ v e1 e2 =
      eval e1 >>= fun w ->
      Machine.Local.update state ~f:(Vars.push v w) >>=  fun () ->
      eval e2 >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop 1) >>= fun () ->
      Machine.return r
    and ext hi lo w =
      let eval_to_int e =
        eval e >>= fun {value=x} -> match Word.to_int x with
        | Ok x -> Machine.return x
        | Error _ -> failf "expected smallint" () in
      eval_to_int hi >>= fun hi ->
      eval_to_int lo >>= fun lo ->
      eval w >>= fun w ->
      Eval.extract ~hi ~lo w
    and lookup v =
      Machine.Local.get state >>= fun {env; width} ->
      match List.Assoc.find ~equal:[%compare.equal : var] env v with
      | Some w -> Machine.return w
      | None -> Eval.get (var width v)
    and app n args =
      Machine.List.map args ~f:eval >>= fun args -> match n with
      | "invoke-subroutine" -> eval_sub args
      | n -> eval_lisp n args
    and seq es =
      let rec loop = function
        | [] -> Eval.const Word.b0
        | e :: [] -> eval e
        | e :: es -> eval e >>= fun _ -> loop es in
      loop es
    and set v w =
      Machine.Local.get state >>= fun s ->
      if List.Assoc.mem ~equal:[%compare.equal : var] s.env v
      then
        Machine.Local.put state {s with env = Vars.replace s.env v w}
        >>= fun () -> Machine.return w
      else
        Eval.set (var s.width v) w >>= fun () ->
        Machine.return w
    and bop op e1 e2 =
      eval e1 >>= fun e1 ->
      eval e2 >>= fun e2 ->
      bil_of_lisp op e1 e2
    and uop op e =
      eval e >>= fun e ->
      let op = match op with
        | Lneg -> Bil.NEG
        | Lnot -> Bil.NOT
        | Not -> assert false in
      Eval.unop op e
    and msg fmt es =
      let pp_exp e =
        Machine.catch
          (eval e >>| fun {value=x} -> fprintf library.log "%a" Word.pp x)
          (fun err ->
             fprintf library.log "<%s>" (Exn.to_string err);
             Machine.return ()) in
      Machine.List.iter fmt ~f:(function
          | Lit s -> Machine.return (pp_print_string library.log s)
          | Exp e -> pp_exp e
          | Pos n -> match List.nth es n with
            | None -> Machine.raise (Runtime_error "fmt pos")
            | Some e -> pp_exp e) >>= fun () ->
      pp_print_newline library.log ();
      Eval.const Word.b0 in
    eval exp

end
