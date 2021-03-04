open Core_kernel
open Bap_core_theory
open Bap.Std
open Format
open Bap_c.Std
open Bap_primus_types
open Bap_primus_sexp


module Lisp = struct
  module Attribute = Bap_primus_lisp_attribute
  module Attributes = Bap_primus_lisp_attributes
  module Def = Bap_primus_lisp_def
  module Var = Bap_primus_lisp_var
  module Resolve = Bap_primus_lisp_resolve
  module State = Bap_primus_state
  module Check = Bap_primus_lisp_type.Check
  module Context = Bap_primus_lisp_context
  module Program = Bap_primus_lisp_program
  module Type = Bap_primus_lisp_type
end

open Bap_primus_lisp_types
open Bap_primus_lisp_attributes
open Lisp.Program.Items


type exn += Runtime_error of string
type exn += Unresolved of KB.Name.t * Lisp.Resolve.resolution

type bindings = {
  vars  : int Var.Map.t;
  stack : (Var.t * value) list
} [@@deriving sexp_of]
type state = {
  program : Lisp.Program.t;
  typeenv : Lisp.Program.Type.env;
  signals : subscription String.Map.t;
  width : int;
  env : bindings;
  cur : Id.t;
}

let inspect {env} = sexp_of_bindings env

let width_of_ctxt proj = Theory.Target.bits (Project.target proj)

let state = Bap_primus_state.declare ~inspect
    ~name:"lisp-env"
    ~uuid:"0360697d-febe-4982-a528-152ada72bf4a"
    (fun proj -> {
         env = {
           stack = [];
           vars = Var.Map.empty;
         };
         cur = Id.null;
         program = Lisp.Program.empty;
         typeenv = Lisp.Program.Type.empty;
         signals = String.Map.empty;
         width = width_of_ctxt proj;
       })


let lisp_primitive,primitive_called = Bap_primus_observation.provide
    ~inspect:Bap_primus_linker.sexp_of_call "lisp-primitive"
    ~desc:"Occurs when the Lisp primitive is invoked."

module Errors(Machine : Machine) = struct
  open Machine.Syntax

  let with_loc msg =
    Machine.Local.get state >>| fun {cur; program} ->
    let source = Lisp.Program.sources program in
    let loc = Source.loc source cur in
    let pos = Format.asprintf "%a" Loc.pp loc in
    String.concat ~sep:"\n" [pos; msg]

  let error kind = Format.ksprintf
      (fun msg () ->
         with_loc msg >>= fun msg ->
         Machine.raise (kind msg))

  let failf fmt = error (fun m -> Runtime_error m) fmt
end


let var_of_lisp_var width {data={exp;typ}} =
  let exp = KB.Name.show exp in
  match typ with
  | Type t -> Var.create exp (Type.Imm t)
  | _ -> Var.create exp (Type.Imm width)

module Locals(Machine : Machine) = struct
  open Machine.Syntax
  include Errors(Machine)


  let make_frame width bs =
    List.fold ~init:([],0) bs ~f:(fun (xs,n) (v,x) ->
        (var_of_lisp_var width v,x)::xs, n+1)

  let rec update xs x ~f = match xs with
    | [] -> []
    | (x',w) :: xs when Var.(x' = x) -> (x,f w) :: xs
    | xw :: xs -> xw :: update xs x ~f

  let replace x w s = {
    s with env = {
      s.env with
      stack = update s.env.stack x ~f:(fun _ -> w)
    }
  }

  let push v w s = {
    s with env = {
      stack = (v,w) :: s.env.stack;
      vars = Map.update s.env.vars v ~f:(function
          | None -> 1
          | Some n -> n + 1)
    }
  }

  (* the order in which arguments are pushed to the stack should
   * irrelevant since we are not allowing repetetive names in the
   * arguments list. Though, apparently this is not really checked,
   * anywhere.
  *)
  let push_frame bs s =
    List.fold bs ~init:s ~f:(fun s (v,w) ->
        push v w s)

  let unbind vars unbound =
    List.fold unbound ~init:vars ~f:(fun vars (v,_) ->
        Map.change vars v ~f:(function
            | None -> assert false
            | Some n -> if n = 1 then None else Some (n-1))
      )

  let pop n s = {
    s with env = {
      stack = List.drop s.env.stack n;
      vars = unbind s.env.vars (List.take s.env.stack n)
    }
  }
end


let () = Exn.add_printer (function
    | Runtime_error msg -> Some ("Primus Lisp runtime error - " ^ msg)
    | Unresolved (name,res) ->
      let msg = asprintf "unable to resolve function %a, because %a"
          KB.Name.pp name Lisp.Resolve.pp_resolution res in
      Some msg
    | _ -> None)


type message = string

module Message = struct
  type t = message
  let pp = Format.pp_print_string
end


let message,new_message =
  Bap_primus_observation.provide
    ~inspect:sexp_of_string "lisp-message"
    ~desc:"Occurs with X when (msg x) is evaluated."

type closure_frame = {
  name : string;
  args : value list;
}

let closure_context = Bap_primus_state.declare
    ~uuid:"b31c12fc-a131-4966-bcc9-e360ed76fb8e"
    ~name:"lisp-closure-context" @@ fun _ -> []


module Trace = Bap_primus_linker.Trace

type dir = [`down | `up] [@@deriving equal]

module Interpreter(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)
  module Eval = Bap_primus_interpreter.Make(Machine)
  module Env = Bap_primus_env.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)
  module Vars = Locals(Machine)

  include Errors(Machine)

  let say fmt =
    ksprintf (Machine.Observation.make new_message) fmt

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
      if Var.same sp sp' && equal_dir dir dir' then
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
      let sign = if equal_dir dir `down then Bil.MINUS else Bil.PLUS in
      Eval.get sp >>= fun sp_value ->
      Eval.const max >>= fun frame_size ->
      Eval.binop sign sp_value frame_size >>=
      Eval.set sp >>= fun () ->
      Machine.return (Some (sp,sp_value))

  let is_out_intent a = match Arg.intent a with
    | Some Out -> true
    | _ -> false

  let is_zero x =
    Value.zero (Value.bitwidth x) >>= fun zero ->
    Eval.binop Bil.EQ x zero

  (* we won't notify linker about a call, since the callee will
     notify it itself. Basically, it is the responsibility of a
     callee to register itself as a call, if it is a call. *)
  let eval_sub : value list -> 'x = function
    | [] -> failf "invoke-subroutine: requires at least one argument" ()
    | sub_addr :: sub_args ->
      Machine.get () >>= fun proj ->
      Term.enum sub_t (Project.program proj) |>
      Seq.find ~f:(fun sub -> match Term.get_attr sub address with
          | None -> false
          | Some addr -> Word.(addr = sub_addr.value)) |> function
      | None  ->
        failf "invoke-subroutine: no function for %a" Addr.pps
          sub_addr.value ()
      | Some sub ->
        let args = Term.enum arg_t sub in
        allocate_stack_frame args >>= fun frame ->
        Seq.zip args (Seq.of_list sub_args) |>
        Machine.Seq.iter ~f:(fun (arg,x) ->
            let open Bil.Types in
            if not (is_out_intent arg)
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
            if is_out_intent arg
            then Eval.get (Arg.lhs arg) >>| Option.some
            else Machine.return None) >>= fun rval ->
        let teardown_frame = match frame with
          | Some (sp,bp) -> Eval.set sp bp
          | None -> Machine.return () in
        teardown_frame >>= fun () -> match rval with
        | None -> Eval.const Word.b0
        | Some rval -> Machine.return rval


  let is_external_call name def =
    let calls =
      Attribute.Set.get External.t (Lisp.Def.attributes def) in
    Set.mem calls (KB.Name.unqualified name)

  let notify_when ?rval cond obs name args =
    if cond then Machine.Observation.post obs ~f:(fun notify ->
        match rval with
        | None -> notify (name, args)
        | Some r -> notify (name, args@[r]))
    else Machine.return ()

  let push_context name args =
    Machine.Local.update closure_context ~f:(fun frames ->
        {name; args} :: frames)

  let pop_context =
    Machine.Local.update closure_context ~f:(function
        | [] -> failwith "Bug: empty closure context"
        | _ :: frames -> frames)

  (* Still an open question. Shall we register an call to an external
     function, that is done not directly from a program, but
     internally from other lisp function?

     Pros: a call to an external function is an effect, i.e., if a
     function implementation calls malloc, then it comes directly from
     the implementation.

     Cons: the same, though it is an effect, it is not really present
     in a program and becomes an assumption that is rather hidden. For
     example, we might assume that a function calls malloc, as our
     implementation does this, though an actual implementation just
     uses static memory, or relies on a custom allocator.

     So far, we will register all calls to externals when the happen
     inside the Lisp interpreter since this looks more conservative,
     though, of course, it depends on a particular policy.
  *)
  let rec eval_lisp name args : value Machine.t =
    Machine.Local.get state >>= fun s ->
    Lisp.Resolve.defun Lisp.Check.value s.program func name args |>
    function
    | None -> eval_primitive name args
    | Some (Error resolution) ->
      Machine.raise (Unresolved (name,resolution))
    | Some (Ok (fn,bs)) ->
      let is_external = is_external_call name fn in
      let name = KB.Name.show name in
      let bs,frame_size = Vars.make_frame s.width bs in
      Eval.const Word.b0 >>= fun init ->
      notify_when is_external Trace.call_entered name args >>= fun () ->
      eval_advices Advice.Before init name args >>= fun _ ->
      Machine.Local.put state (Vars.push_frame bs s) >>= fun () ->
      eval_exp (Lisp.Def.Func.body fn) >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop frame_size) >>= fun () ->
      notify_when is_external Trace.call_returned name args ~rval:r >>= fun () ->
      eval_advices Advice.After r name args

  and eval_advices stage init primary args =
    Machine.Local.get state >>= fun {program} ->
    Lisp.Program.get program func |>
    Machine.List.fold ~init ~f:(fun r def ->
        let name = Lisp.Def.name def in
        let adv =
          Attribute.Set.get Advice.t (Lisp.Def.attributes def) in
        if Set.mem (Advice.targets adv stage) primary
        then match stage with
          | After  -> eval_lisp name (args @ [r])
          | Before -> eval_lisp name args
        else Machine.return r)

  and eval_primitive name args =
    Machine.Local.get state >>= fun {program} ->
    match Lisp.Resolve.primitive program primitive name () with
    | None -> failf "unresolved primitive %s" (KB.Name.show name) ()
    | Some (Error _) -> failf "conflicting primitive %s" (KB.Name.show name) ()
    | Some (Ok (code,())) ->
      let module Body = (val (Lisp.Def.Closure.body code)) in
      let module Code = Body(Machine) in
      let name = KB.Name.show name in
      Eval.const Word.b0 >>= fun init ->
      eval_advices Advice.Before init name args >>= fun _ ->
      push_context name args >>= fun () ->
      Code.run args >>= fun r ->
      pop_context >>= fun () ->
      Eval.tick >>= fun () ->
      Machine.Observation.post primitive_called ~f:(fun k ->
          k (name,args@[r])) >>= fun () ->
      eval_advices Advice.After r name args

  and eval_exp exp  =
    let int v t =
      let width = match t with
        | Type t -> Machine.return t
        | Symbol -> Machine.return Lisp.Type.symbol_size
        | Any | Name _ -> width () in
      width >>= fun width ->
      let bv = Bitvec.(bigint v mod modulus width) in
      Eval.const (Word.create bv width) in
    let sym v = Value.Symbol.to_value (KB.Name.show v.data) in
    let rec eval = function
      | {data=Int {data={exp;typ}}} -> int exp typ
      | {data=Var v} -> lookup v
      | {data=Sym v} -> sym v
      | {data=Ite (c,e1,e2)}  -> ite c e1 e2
      | {data=Let (v,e1,e2)} -> let_ v e1 e2
      | {data=App (n,args)} -> app n args
      | {data=Rep (c,e)} -> rep c e
      | {data=Seq es} -> seq es
      | {data=Set (v,e)} -> eval e >>= set v
      | {data=Msg (fmt,es)} -> msg fmt es
      | {data=Err msg} -> Machine.raise (Runtime_error msg)
    and rep c e = Eval.repeat (eval c) (eval e)
    and ite c e1 e2 =
      eval c >>= fun c ->
      Eval.branch c (eval e1) (eval e2)
    and let_ v e1 e2 =
      Machine.Local.get state >>= fun {width} ->
      eval e1 >>= fun w ->
      let v = var_of_lisp_var width v in
      Machine.Local.update state ~f:(Vars.push v w) >>=  fun () ->
      eval e2 >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop 1) >>= fun () ->
      Machine.return r
    and lookup v =
      Machine.Local.get state >>= fun {env; width; program} ->
      let v = var_of_lisp_var width v in
      if Map.mem env.vars v
      then
        Machine.return @@
        List.Assoc.find_exn ~equal:Var.equal env.stack v
      else Env.has v >>= function
        | true -> Eval.get v
        | false ->
          Lisp.Program.get program para |>
          List.find ~f:(fun p ->
              let v = KB.Name.read (Var.name v) in
              KB.Name.equal (Lisp.Def.name p) v) |> function
          | None -> Eval.get v
          | Some p ->
            eval (Lisp.Def.Para.default p) >>= fun x ->
            Eval.set v x >>| fun () -> x
    and app n args =
      Machine.List.map args ~f:eval >>= fun args -> match n with
      | Static _ -> assert false
      | Dynamic n ->
        if String.equal (KB.Name.unqualified n) "invoke-subroutine"
        then eval_sub args
        else eval_lisp n args
    and seq es =
      let rec loop = function
        | [] -> Eval.const Word.b0
        | e :: [] -> eval e
        | e :: es -> eval e >>= fun _ -> loop es in
      loop es
    and set v w =
      Machine.Local.get state >>= fun s ->
      let v = var_of_lisp_var s.width v in
      if Map.mem s.env.vars v
      then
        Machine.Local.put state (Vars.replace v w s) >>| fun () ->
        w
      else Eval.set v w >>| fun () -> w
    and msg fmt es =
      let str e =
        Machine.catch
          (eval e >>= fun v ->
           Value.Symbol.of_value v >>| function
           | "" -> asprintf "%a" Word.pp v.value
           | s  -> asprintf "#<%s %a>" s Word.pp v.value)
          (fun err ->
             Machine.return (asprintf "<%s>" (Exn.to_string err))) in
      Machine.List.map fmt ~f:(function
          | Lit s -> Machine.return s
          | Pos n -> match List.nth es n with
            | None -> failf "bad positional argument $%d" n ()
            | Some e -> str e) >>= fun contents ->
      let msg = String.concat contents in
      Machine.Observation.make new_message msg >>= fun () ->
      Eval.const Word.b0 in
    Machine.Local.update state (fun s -> {s with cur = exp.id}) >>= fun () ->
    eval exp

  let eval_signal name args : unit Machine.t =
    Machine.Local.get state >>= fun s ->
    let res =
      Lisp.Resolve.meth Lisp.Check.value s.program meth name args in
    match res with
    | None -> Machine.return ()
    | Some (Error resolution) ->
      Machine.raise (Unresolved (name,resolution))
    | Some (Ok mets) ->
      Machine.List.iter mets ~f:(fun (met,bs) ->
          let bs,frame_size = Vars.make_frame s.width bs in
          Machine.Local.update state ~f:(Vars.push_frame bs) >>= fun () ->
          eval_exp (Lisp.Def.Meth.body met) >>= fun _ ->
          Machine.Local.update state ~f:(Vars.pop frame_size))
end

let init ?log:_ ?paths:_ _features  =
  failwith "Lisp library no longer requires initialization"

module Closure = struct
  module type S = Lisp.Def.Closure
  type t = (module S)
  module Make(Machine : Machine) = struct
    open Machine.Syntax
    module Lisp = Interpreter(Machine)
    let name =
      Machine.Local.get closure_context >>= function
      | [] -> Lisp.failf "Closure.name is called not from a closure" ()
      | {name} :: _ -> Machine.return name
  end
end

module type Closure = Closure.S


type primitives = Lisp.Def.primitives
module type Primitives = Lisp.Def.Primitives
module Primitive = Lisp.Def.Primitive
type closure = Lisp.Def.closure
type program = Lisp.Program.t
module Load = Bap_primus_lisp_parse
module Type = struct
  include Lisp.Program.Type
  type t = Theory.Target.t -> Lisp.Type.t
  type signature = Theory.Target.t -> Lisp.Type.signature

  type parameters = [
    | `All of t
    | `Gen of t list * t
    | `Tuple of t list
  ]


  let error,notify_error =
    Bap_primus_observation.provide "lisp-type-error"
      ~desc:"Occurs when the Lisp type error is detected \
             by the type checker"

  module Spec = struct
    let any _ = Lisp.Type.any
    let var s _ = Lisp.Type.var s
    let sym _ = Lisp.Type.sym
    let word n _ = Lisp.Type.word n
    let int t =
      Lisp.Type.word (Theory.Target.bits t)
    let bool = word 1
    let byte = word 8
    let a : t = var "a"
    let b : t = var "b"
    let c : t = var "c"
    let d : t = var "d"

    let tuple ts = `Tuple ts
    let unit = tuple []
    let one t = tuple [t]
    let all t = `All t

    let (//) : [`Tuple of t list] -> [`All of t] -> parameters =
      fun (`Tuple ts) (`All t) -> `Gen (ts,t)

    let (@->) (dom : [< parameters]) (cod : t) : signature =
      let args,rest = match dom with
        | `All t -> [],Some t
        | `Tuple ts -> ts,None
        | `Gen (ts,t) -> ts, Some t in
      fun arch ->
        let args = List.map args ~f:(fun t -> t arch) in
        let cod = cod arch in
        let rest = Option.map rest ~f:(fun t -> t arch) in
        Lisp.Type.signature args ?rest cod
  end
end

module Make(Machine : Machine) = struct
  open Machine.Syntax
  module Self = Interpreter(Machine)
  module Linker = Bap_primus_linker.Make(Machine)
  module Eval = Bap_primus_interpreter.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)
  module Vars = Locals(Machine)
  module Env = Bap_primus_env.Make(Machine)
  include Errors(Machine)


  module Typechecker = struct
    module Env = Bap_primus_env.Make(Machine)

    let signature_of_sub sub =
      let lisp_type_of_arg arg = match Var.typ (Arg.lhs arg) with
        | Imm 1 -> Type.Spec.bool
        | Imm n -> Type.Spec.word n
        | Unk | Mem _ -> Type.Spec.any in
      if Term.length arg_t sub = 0
      then Type.Spec.(all any @-> any)
      else Term.enum ~rev:true arg_t sub |>
           Seq.fold ~init:([],Type.Spec.any) ~f:(fun (args,ret) arg ->
               match Arg.intent arg with
               | Some Out -> args, lisp_type_of_arg arg
               | _ -> lisp_type_of_arg arg :: args,ret) |> fun (args,ret) ->
           Type.Spec.(tuple args @-> ret)

    let signatures_of_subs prog =
      Term.enum sub_t prog |>
      Seq.map ~f:(fun s -> Sub.name s, signature_of_sub s) |>
      Seq.to_list

    let vars_of_prog prog =
      (object inherit [Var.Set.t] Term.visitor
        method! enter_var v vs = Set.add vs v
      end)#run prog Var.Set.empty |> Set.to_sequence

    let invoke_subroutine_signature =
      "invoke-subroutine", Type.Spec.(one int // all any @-> any)


    let finish_imports =
      Machine.Local.update state ~f:(fun s -> {
            s with program = Lisp.Program.finish_imports s.program;
          })


    let run =
      finish_imports >>= fun () ->
      Machine.get () >>= fun proj ->
      Machine.Local.get state >>= fun s ->
      Env.all >>= fun evars ->
      let pvars = vars_of_prog (Project.program proj) in
      let vars = Seq.append evars pvars in
      let t = Project.target proj in
      let externals =
        invoke_subroutine_signature ::
        signatures_of_subs (Project.program proj) |>
        List.map ~f:(fun (n,s) -> n,s t) in
      let typeenv =
        Lisp.Program.Type.infer ~externals vars s.program in
      Lisp.Program.Type.errors typeenv |>
      Machine.List.iter ~f:(fun s ->
          Machine.Observation.make Type.notify_error s) >>= fun () ->
      Machine.Local.put state {s with typeenv}
  end


  let typecheck = Typechecker.run
  let types = Machine.Local.get state >>| fun s -> s.typeenv

  let collect_externals s =
    Lisp.Program.get s.program Lisp.Program.Items.func |>
    List.fold ~init:String.Map.empty  ~f:(fun toload def ->
        let names = Attribute.Set.get External.t (Lisp.Def.attributes def) in
        Set.fold names ~init:toload ~f:(fun toload name ->
            Map.add_multi toload ~key:name ~data:def)) |>
    Map.to_sequence


  let collect_globals target s =
    let open Lisp.Attributes in
    let default_width = Theory.Target.bits target in
    let add attr def vars =
      let vars' =
        Set.to_list @@ Attribute.Set.get attr (Lisp.Def.attributes def) in
      Set.union vars @@
      Var.Set.of_list @@
      List.map ~f:(var_of_lisp_var default_width) vars' in
    Lisp.Program.get s.program Lisp.Program.Items.func |>
    List.fold ~init:Var.Set.empty  ~f:(fun vars def ->
        add Variables.global def vars |>
        add Variables.static def) |>
    Set.to_sequence


  let link_global var = match Var.typ var with
    | Imm m ->
      Value.zero m >>= Env.set var
    | _ -> Machine.return ()

  let link_globals () =
    Machine.gets Project.target >>= fun target ->
    Machine.Local.get state >>| collect_globals target >>=
    Machine.Seq.iter ~f:link_global

  let find_sub prog name =
    Term.enum sub_t prog |>
    Seq.find ~f:(fun s -> String.equal (Sub.name s) name) |> function
    | None -> [],None,None,None
    | Some sub ->
      let tid = Some (Term.tid sub) in
      let addr = Term.get_attr sub address in
      match Term.get_attr sub C.proto with
      | None -> [],None,tid,addr
      | Some proto ->
        let args = Term.enum arg_t sub |> Seq.to_list in
        let args,ret = match proto.C.Type.Proto.return with
          | `Void -> args,None
          | _ -> List.(take args (List.length args - 1), last args) in
        args,ret,tid,addr


  let link_feature (name,_defs) =
    Machine.get () >>= fun proj ->
    Machine.Local.get state >>= fun s ->
    let args,ret,tid,addr = find_sub (Project.program proj) name in
    let name = KB.Name.read name in
    Lisp.Resolve.extern Lisp.Check.arg
      s.program
      Lisp.Program.Items.func name args |> function
    | None -> Machine.return ()
    | Some (Error _) when Option.is_none tid -> Machine.return ()
    | Some (Error err) -> Machine.raise (Unresolved (name,err))
    | Some (Ok (fn,bs)) ->
      let bs,frame_size = Vars.make_frame s.width bs in
      let module Code(Machine : Machine) = struct
        open Machine.Syntax
        module Eval = Bap_primus_interpreter.Make(Machine)
        module Value = Bap_primus_value.Make(Machine)
        module Interp = Interpreter(Machine)

        let failf ppf = Format.ksprintf
            (fun msg -> fun () -> Machine.raise (Runtime_error msg)) ppf

        let eval_args = Machine.List.map bs ~f:(fun (var,arg) ->
            Eval.exp (Arg.rhs arg) >>| fun w -> (var ,w))

        let size_of_reg r = match Var.typ r with
          | Imm x -> x
          | _ -> assert false

        let eval_ret r = match ret with
          | None -> Machine.return ()
          | Some v -> match Arg.rhs v with
            | Bil.Var reg -> Eval.set reg r
            | Bil.(Cast (LOW, rsize, Var reg)) ->
              let vsize = size_of_reg reg in
              Eval.get reg >>= fun lhs ->
              Eval.extract ~hi:(vsize-1) ~lo:rsize lhs >>= fun high ->
              Eval.concat high r >>= fun r ->
              Eval.set reg r
            | e -> failf "an unsupported return semantics: %a" Exp.pps e ()

        let exec =
          eval_args >>= fun bs ->
          let args = List.rev_map ~f:snd bs in
          Value.b0 >>= fun init ->
          let name = KB.Name.show name in
          Interp.eval_advices Advice.Before init name args >>= fun _ ->
          Machine.Local.update state ~f:(Vars.push_frame bs) >>= fun () ->
          Interp.notify_when true Trace.call_entered name args >>= fun () ->
          Interp.notify_when true Trace.lisp_call_entered name args >>= fun () ->
          Interp.eval_exp (Lisp.Def.Func.body fn) >>= fun r ->
          Machine.Local.update state ~f:(Vars.pop frame_size) >>= fun () ->
          Interp.notify_when true Trace.lisp_call_returned name args ~rval:r >>= fun () ->
          Interp.notify_when true Trace.call_returned name args ~rval:r >>= fun () ->
          Interp.eval_advices Advice.After r name args >>= fun r ->
          eval_ret r
      end in
      Linker.link ?addr ?tid ~name:(KB.Name.unqualified name) (module Code)

  let link_features () =
    Machine.Local.get state >>| collect_externals >>=
    Machine.Seq.iter ~f:link_feature

  let copy_primitives ~src ~dst =
    Lisp.Program.get src primitive |>
    List.fold ~init:dst ~f:(fun dst p ->
        Lisp.Program.add dst primitive p)

  let link_program program =
    Machine.Local.get state >>= fun s ->
    let s = {s with program = Lisp.Program.merge s.program program} in
    Machine.Local.put state s >>= fun () ->
    link_globals () >>=
    link_features

  let program = Machine.Local.get state >>| fun s -> s.program

  let link_primitive p =
    Machine.Local.update state ~f:(fun s -> {
          s with
          program = Lisp.Program.add s.program primitive p
        })

  let define ?types ?docs ?package name body =
    Machine.gets Project.target >>= fun arch ->
    let types = Option.map types ~f:(fun t -> t arch) in
    Lisp.Def.Closure.create ?types ?docs ?package name body |>
    link_primitive

  let signal ?params ?(doc="undocumented") obs proj =
    Machine.gets Project.target >>= fun arch ->
    let specialize = List.map ~f:(fun p -> p arch) in
    let name = Bap_primus_observation.name obs in
    let default_types = Lisp.Type.signature [] ~rest:Any Any in
    let types = match params with
      | None ->
        default_types
      | Some (`All t) ->
        Lisp.Type.signature [] ~rest:(t arch) Any
      | Some (`Tuple ts) ->
        Lisp.Type.signature (specialize ts) Any
      | Some (`Gen (ts,t)) ->
        Lisp.Type.signature (specialize ts) ~rest:(t arch) Any in
    let name = KB.Name.read name in
    let r = Lisp.Def.Signal.create ~types ~docs:doc name in
    Machine.Observation.subscribe obs (fun x ->
        proj x >>= Self.eval_signal name) >>= fun sub ->
    Machine.Local.update state ~f:(fun s -> {
          s with program = Lisp.Program.add s.program signal r;
                 signals = Map.add_exn s.signals (KB.Name.show name) sub;
        })


  (* this is a deprecated interface for the backward compatibility,
     we can't efficiently translate a list of primitives to the code
     existential, thus we are forced to unpack the packed library of
     primitives, then find one primitive and repack it, and repeat
     this process every time we need an existential.
  *)
  let link_primitives (module Library : Lisp.Def.Primitives) =
    let module Unpacked = Library(Machine) in
    Unpacked.defs () |> Machine.List.iter ~f:(fun def ->
        let module Packed(M : Machine) = struct
          module Unpacked = Library(M)
          let run =
            Unpacked.defs () |> List.find ~f:(fun d ->
                KB.Name.equal (Lisp.Def.name d) (Lisp.Def.name def)) |> function
            | Some code -> (Lisp.Def.Primitive.body code)
            | _ -> assert false
        end in
        Lisp.Def.Closure.of_primitive def
          (module Packed : Lisp.Def.Closure) |>
        link_primitive)

  let eval_method name = Self.eval_signal (KB.Name.read name)
  let eval_fun name = Self.eval_lisp (KB.Name.read name)

  let optimize () =
    Machine.Local.get state >>= fun s ->
    let known_methods =
      Lisp.Program.get s.program meth |>
      List.fold ~init:String.Set.empty ~f:(fun mets met ->
          Set.add mets (KB.Name.show (Lisp.Def.name met))) in
    let useless_subscriptions =
      Map.fold s.signals ~init:[] ~f:(fun ~key:name ~data:sub subs ->
          if Set.mem known_methods name then subs
          else sub::subs) in
    Machine.List.iter useless_subscriptions ~f:Machine.Observation.cancel


  let refine ctxt =
    Machine.Local.update state ~f:(fun s -> {
          s with program =
                   Lisp.Program.with_context s.program
                     (Lisp.Context.merge
                        ctxt
                        (Lisp.Program.context s.program))
        })

end

module Doc = struct
  module type Element = sig
    type t
    val pp : formatter -> t -> unit
  end

  open Bap_knowledge

  module Info = Bap_primus_info

  module Category = String
  module Name = Knowledge.Name
  module Descr = String

  type index = (string * (Name.t * string) list) list

  let normalize xs =
    Map.of_alist_reduce (module Name) xs ~f:(fun x y -> match x,y with
        | "", y -> y
        | x, "" -> x
        | x,y when String.equal x y -> x
        | x,y -> sprintf "%s\nOR\n%s" x y) |>
    Map.to_alist

  let describe prog item =
    Lisp.Program.get prog item |> List.map ~f:(fun x ->
        let name = Name.create (KB.Name.show (Lisp.Def.name x)) in
        let info = Info.create ~desc:(Lisp.Def.docs x) name in
        name,Info.desc info) |> normalize

  let index p = Lisp.Program.Items.[
      "Macros", describe p macro;
      "Substitutions", describe p subst;
      "Constants", describe p const;
      "Functions", describe p func;
      "Methods", describe p meth;
      "Parameters", describe p para;
      "Primitives", describe p primitive;
      "Signals", describe p signal;
    ]

  module Make(Machine : Machine) = struct
    open Machine.Syntax
    let generate_index : index Machine.t =
      Machine.Local.get state >>| fun s ->
      index s.program
  end
end

let primitive = lisp_primitive
module Semantics = Bap_primus_lisp_semantics
module Unit = Semantics.Unit
module Attribute = Lisp.Attribute
