open Core_kernel.Std
open Bap.Std
open Format
open Bap_c.Std
open Bap_primus_types
open Bap_primus_sexp


module Lisp = struct
  module Attribute = Bap_primus_lisp_attribute
  module Def = Bap_primus_lisp_def
  module Var = Bap_primus_lisp_var
  module Parse = Bap_primus_lisp_parse
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

module Pos = Bap_primus_pos


type exn += Runtime_error of string
type exn += Unresolved of string * Lisp.Resolve.resolution

type bindings = (Var.t * value) list [@@deriving sexp_of]
type state = {
  program : Lisp.Program.t;
  width : int;
  env : bindings;
  cur : Id.t;
}

let inspect {env} = sexp_of_bindings env

let width_of_ctxt proj =
  Size.in_bits (Arch.addr_size (Project.arch proj))

let state = Bap_primus_state.declare ~inspect
    ~name:"lisp-env"
    ~uuid:"0360697d-febe-4982-a528-152ada72bf4a"
    (fun proj -> {
         env = [];
         cur = Id.null;
         program = Lisp.Program.empty;
         width = width_of_ctxt proj;
       })


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

module Locals(Machine : Machine) = struct
  open Machine.Syntax
  include Errors(Machine)

  let of_lisp {data={exp;typ}} =
    Machine.Local.get state >>= fun {width} ->
    match typ with
    | Type t -> Machine.return (Var.create exp (Type.Imm t))
    | _ -> Machine.return (Var.create exp (Type.Imm width))

  let bindings =
    Machine.List.map ~f:(fun (v,x) -> of_lisp v >>| fun v -> v,x)

  let rec update xs x ~f = match xs with
    | [] -> []
    | (x',w) :: xs when Var.(x' = x) -> (x,f w) :: xs
    | xw :: xs -> xw :: update xs x ~f

  let replace xs x w = update xs x ~f:(fun _ -> w)

  let push v w s = {s with env = (v,w)::s.env}
  let pop n ({env} as s) = {s with env = List.drop env n}
end


let () = Exn.add_printer (function
    | Runtime_error msg -> Some ("primus runtime error - " ^ msg)
    | Unresolved (name,res) ->
      let msg = asprintf "unable to resolve function %s, because %a"
          name Lisp.Resolve.pp_resolution res in
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


module Trace = Bap_primus_linker.Trace


module Interpreter(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)
  module Eval = Bap_primus_interpreter.Make(Machine)
  module Env = Bap_primus_env.Make(Machine)
  module Mem = Bap_primus_memory.Make(Machine)
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


  let is_external_call name def =
    match Attribute.Set.get (Lisp.Def.attributes def) External.t with
    | None -> false
    | Some names -> List.mem ~equal:String.equal names name

  let notify_when cond obs name args =
    if cond
    then Machine.Observation.make obs (name,args)
    else Machine.return ()

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
      Vars.bindings bs >>= fun bs ->
      Eval.const Word.b0 >>= fun init ->
      notify_when is_external Trace.call_entered name args >>= fun () ->
      eval_advices Advice.Before init name args >>= fun _ ->
      Machine.Local.put state {s with env = bs @ s.env} >>= fun () ->
      eval_exp (Lisp.Def.Func.body fn) >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop (List.length bs)) >>= fun () ->
      notify_when is_external Trace.call_returned name (args @ [r]) >>= fun () ->
      eval_advices Advice.After r name args

  and eval_advices stage init primary args =
    Machine.Local.get state >>= fun {program} ->
    Lisp.Program.get program func |>
    Machine.List.fold ~init ~f:(fun r def ->
        let name = Lisp.Def.name def in
        match Attribute.Set.get (Lisp.Def.attributes def) Advice.t with
        | None -> Machine.return r
        | Some adv ->
          if Set.mem (Advice.targets adv stage) primary
          then match stage with
            | After ->  eval_lisp name (args @ [r])
            | Before -> eval_lisp name args
          else Machine.return r)

  and eval_primitive name args =
    Machine.Local.get state >>= fun {program} ->
    match Lisp.Resolve.primitive program primitive name () with
    | None -> failf "unresolved primitive %s" name ()
    | Some (Error _) -> failf "conflicting primitive %s" name ()
    | Some (Ok (code,())) ->
      let module Body = (val (Lisp.Def.Closure.body code)) in
      let module Code = Body(Machine) in
      Eval.const Word.b0 >>= fun init ->
      eval_advices Advice.Before init name args >>= fun _ ->
      Code.run args >>= fun r ->
      eval_advices Advice.After r name args

  and eval_exp exp  =
    let int v t =
      let width = match t with
        | Type t -> Machine.return t
        | Symbol -> Machine.return Lisp.Type.symbol_size
        | Any | Name _ -> width () in
      width >>= fun width ->
      Eval.const (Word.of_int64 ~width v) in
    let sym v = Value.Symbol.to_value v.data in
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
    and rep c e =
      eval c >>= function {value} as r ->
        if Word.is_zero value then Machine.return r
        else eval e >>= fun _ -> rep c e
    and ite c e1 e2 =
      eval c >>= fun {value=w} ->
      if Word.is_zero w then eval e2 else eval e1
    and let_ v e1 e2 =
      eval e1 >>= fun w ->
      Vars.of_lisp v >>= fun v ->
      Machine.Local.update state ~f:(Vars.push v w) >>=  fun () ->
      eval e2 >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop 1) >>= fun () ->
      Machine.return r
    and lookup v =
      Machine.Local.get state >>= fun {env} ->
      Vars.of_lisp v >>= fun v ->
      match List.Assoc.find ~equal:Var.equal env v with
      | Some w -> Machine.return w
      | None -> Eval.get v
    and app n args =
      Machine.List.map args ~f:eval >>= fun args -> match n with
      | Static _ -> assert false
      | Dynamic "invoke-subroutine" -> eval_sub args
      | Dynamic n -> eval_lisp n args
    and seq es =
      let rec loop = function
        | [] -> Eval.const Word.b0
        | e :: [] -> eval e
        | e :: es -> eval e >>= fun _ -> loop es in
      loop es
    and set v w =
      Machine.Local.get state >>= fun s ->
      Vars.of_lisp v >>= fun v ->
      if List.Assoc.mem ~equal:Var.equal s.env v
      then
        Machine.Local.put state {s with env = Vars.replace s.env v w}
        >>= fun () -> Machine.return w
      else
        Eval.set v w >>= fun () ->
        Machine.return w
    and msg fmt es =
      let buf = Buffer.create 64 in
      let ppf = formatter_of_buffer buf in
      let pp_exp e =
        Machine.catch
          (eval e >>| fun {value=x} -> fprintf ppf "%a" Word.pp x)
          (fun err ->
             fprintf ppf "<%s>" (Exn.to_string err);
             Machine.return ()) in
      Machine.List.iter fmt ~f:(function
          | Lit s -> Machine.return (pp_print_string ppf s)
          | Pos n -> match List.nth es n with
            | None -> failf "bad positional argument $%d" n ()
            | Some e -> pp_exp e) >>= fun () ->
      pp_print_flush ppf ();
      Machine.Observation.make new_message (Buffer.contents buf) >>= fun () ->
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
          Vars.bindings bs >>= fun bs ->
          Machine.Local.put state {s with env = bs @ s.env} >>= fun () ->
          eval_exp (Lisp.Def.Meth.body met) >>= fun _ ->
          Machine.Local.update state ~f:(Vars.pop (List.length bs)))

end


module Make(Machine : Machine) = struct
  open Machine.Syntax
  module Self = Interpreter(Machine)
  module Linker = Bap_primus_linker.Make(Machine)
  module Eval = Bap_primus_interpreter.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)
  module Vars = Locals(Machine)
  include Errors(Machine)

  let collect_externals s =
    Lisp.Program.get s.program Lisp.Program.Items.func |>
    List.fold ~init:String.Map.empty  ~f:(fun toload def ->
        match Attribute.Set.get (Lisp.Def.attributes def) External.t with
        | Some names ->
          List.fold names ~init:toload ~f:(fun toload name ->
              Map.add_multi toload ~key:name ~data:def)
        | _ -> toload) |>
    Map.to_sequence


  let find_sub prog name =
    Term.enum sub_t prog |>
    Seq.find ~f:(fun s -> Sub.name s = name) |> function
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
    Lisp.Resolve.extern Lisp.Check.arg
      s.program
      Lisp.Program.Items.func name args |> function
    | None -> Machine.return ()
    | Some (Error _) when tid = None -> Machine.return ()
    | Some (Error err) -> Machine.raise (Unresolved (name,err))
    | Some (Ok (fn,bs)) ->
      Vars.bindings bs >>= fun bs ->
      let module Code(Machine : Machine) = struct
        open Machine.Syntax
        module Eval = Bap_primus_interpreter.Make(Machine)
        module Interp = Interpreter(Machine)

        let failf ppf = Format.ksprintf
            (fun msg -> fun () -> Machine.raise (Runtime_error msg)) ppf

        let eval_args = Machine.List.map bs ~f:(fun (var,arg) ->
            let open Bil.Types in
            match Arg.rhs arg with
            | Var v -> Eval.get v >>| fun w -> (var,w)
            | Load (_,BinOp(op, Var sp, Int off),e,s) ->
              Eval.get sp >>= fun sp ->
              Eval.const off >>= fun off ->
              Eval.binop op sp off >>= fun addr ->
              Eval.load addr e s >>| fun w -> (var,w)
            | _ -> failf "unsupported argument passing semantics" ())

        let eval_ret r = match ret with
          | None -> Machine.return ()
          | Some v -> match Arg.rhs v with
            | Bil.Var reg -> Eval.set reg r
            | e -> failf "unknown return semantics: %a" Exp.pps e ()

        let exec =
          eval_args >>= fun bs ->
          let args = List.map ~f:snd bs in
          Eval.const Word.b0 >>= fun init ->
          Interp.eval_advices Advice.Before init name args >>= fun _ ->
          Machine.Local.update state
            ~f:(fun s -> {s with env = bs @ s.env}) >>= fun () ->
          Machine.Observation.make Trace.call_entered (name,args) >>= fun () ->
          Interp.eval_exp (Lisp.Def.Func.body fn) >>= fun r ->
          Machine.Local.update state ~f:(Vars.pop (List.length bs)) >>= fun () ->
          Machine.Observation.make Trace.call_returned (name,args @ [r]) >>= fun () ->
          Interp.eval_advices Advice.After r name args >>= fun r ->
          eval_ret r
      end in
      Linker.link ?addr ?tid ~name (module Code)

  let link_features () =
    Machine.Local.get state >>| collect_externals >>=
    Machine.Seq.iter ~f:link_feature

  let init_env proj = (object
    inherit [(Var.t * value) Machine.t list] Term.visitor
    method! enter_term _ t env =
      match Term.get_attr t address with
      | None -> env
      | Some addr ->
        let binding =
          let typ = Type.imm (Word.bitwidth addr) in
          Value.of_word addr >>| fun addr ->
          Var.create (Term.name t) typ, addr in
        binding :: env
  end)#run proj [] |> Machine.List.all

  let link_program program =
    Machine.get () >>= fun proj ->
    init_env (Project.program proj) >>= fun env ->
    Machine.Local.put state {
      program; env;
      cur = Id.null;
      width = width_of_ctxt proj;
    } >>= fun () ->
    link_features ()

  let program = Machine.Local.get state >>| fun s -> s.program

  let link_primitive p =
    Machine.Local.update state ~f:(fun s -> {
          s with
          program = Lisp.Program.add s.program primitive p
        })

  let define ?types ?docs name body =
    Machine.gets Project.arch >>= fun arch ->
    let types = Option.map types ~f:(fun t -> t arch) in
    Lisp.Def.Closure.create ?types ?docs name body |>
    link_primitive

  let signal ?params:_ ?doc:_ obs proj =
    let name = Bap_primus_observation.name obs in
    Machine.Observation.observe obs (fun x ->
        proj x >>= Self.eval_signal name)

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
                Lisp.Def.name d = Lisp.Def.name def) |> function
            | Some code -> (Lisp.Def.Primitive.body code)
            | _ -> assert false
        end in
        Lisp.Def.Closure.of_primitive def
          (module Packed : Lisp.Def.Closure) |>
        link_primitive)
end

let init ?log:_ ?paths:_ _features  =
  failwith "Lisp library no longer requires initialization"

type primitives = Lisp.Def.primitives
module type Primitives = Lisp.Def.Primitives
module Primitive = Lisp.Def.Primitive
module type Closure = Lisp.Def.Closure
type closure = Lisp.Def.closure
type program = Lisp.Program.t
module Load = Lisp.Parse
module Type = struct
  include Lisp.Program.Type
  type t = arch -> Lisp.Type.t
  type signature = arch -> Lisp.Type.signature

  type parameters = [
    | `All of t
    | `Gen of t list * t
    | `Tuple of t list
  ]


  module Spec = struct
    let any _ = Lisp.Type.any
    let var s _ = Lisp.Type.var s
    let sym _ = Lisp.Type.sym
    let word n _ = Lisp.Type.word n
    let int arch =
      Lisp.Type.word (Size.in_bits (Arch.addr_size arch))
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
