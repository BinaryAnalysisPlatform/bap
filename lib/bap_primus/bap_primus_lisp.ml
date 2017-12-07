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
end

open Bap_primus_lisp_types
open Bap_primus_lisp_attributes
open Lisp.Program.Items

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
    ~uuid:"fc4b3719-f32c-4d0f-ad63-6167ab00b7f9"
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
    | Any -> Machine.return (Var.create exp (Type.Imm width))
    | Type t -> Machine.return (Var.create exp (Type.Imm t))

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



let message,new_message =
  Bap_primus_observation.provide
    ~inspect:sexp_of_string "lisp-message"

module Trace = struct
  open Sexp
  module Observation = Bap_primus_observation
  let sexp_of_value {value=x} =
    let v = Format.asprintf "%a" Word.pp_hex x in
    Atom v
  let sexp_of_binding (_,x) = sexp_of_value x

  let sexp_of_enter (def,bs) =
    List (Atom (Lisp.Def.name def) :: List.map bs ~f:sexp_of_binding)

  let sexp_of_leave (call,result) =
    List (Atom "#result-of" ::
          sexp_of_enter call ::
          [sexp_of_value result])

  let enter,entered =
    Observation.provide ~inspect:sexp_of_enter "lisp-call"

  let leave,left =
    Observation.provide ~inspect:sexp_of_leave "lisp-return"

end


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
    Machine.Local.get state >>= fun s ->
    Lisp.Resolve.defun Lisp.Check.value s.program func name args |>
    function
    | None -> eval_primitive name args
    | Some (Error resolution) ->
      Machine.raise (Unresolved (name,resolution))
    | Some (Ok (fn,bs)) ->
      Vars.bindings bs >>= fun bs ->
      Eval.const Word.b0 >>= fun init ->
      Machine.Observation.make Bap_primus_linker.will_exec (`symbol name) >>= fun () ->
      eval_advices Advice.Before init name args >>= fun _ ->
      Machine.Local.put state {s with env = bs @ s.env} >>= fun () ->
      Machine.Observation.make Trace.entered (fn,bs) >>= fun () ->
      eval_exp (Lisp.Def.Func.body fn) >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop (List.length bs)) >>= fun () ->
      Machine.Observation.make Trace.left ((fn,bs),r) >>= fun () ->
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
    | Some (Error err) -> failf "conflicting primitive %s" name ()
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
        | Any -> width ()
        | Type t -> Machine.return t in
      width >>= fun width ->
      Eval.const (Word.of_int64 ~width v) in
    let rec eval = function
      | {data=Int {data={exp;typ}}} -> int exp typ
      | {data=Var v} -> lookup v
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
      Machine.Local.get state >>= fun {env; width} ->
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

end


module Make(Machine : Machine) = struct
  open Machine.Syntax
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


  let link_feature (name,defs) =
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
          Machine.get () >>= fun ctxt ->
          eval_args >>= fun bs ->
          let args = List.map ~f:snd bs in
          Eval.const Word.b0 >>= fun init ->
          Interp.eval_advices Advice.Before init name args >>= fun _ ->
          Machine.Local.update state
            ~f:(fun s -> {s with env = bs @ s.env}) >>= fun () ->
          Machine.Observation.make Trace.entered (fn,bs) >>= fun () ->
          Interp.eval_exp (Lisp.Def.Func.body fn) >>= fun r ->
          Machine.Local.update state ~f:(Vars.pop (List.length bs)) >>= fun () ->
          Machine.Observation.make Trace.left ((fn,bs),r) >>= fun () ->
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


  let link_primitive p =
    Machine.Local.update state ~f:(fun s -> {
          s with
          program = Lisp.Program.add s.program primitive p
        })

  let define ?docs name body =
    Lisp.Def.Closure.create ?docs name body |>
    link_primitive

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

let init ?(log=std_formatter) ?(paths=[]) features  =
  failwith "Lisp library no longer requires initialization"

type primitives = Lisp.Def.primitives
module type Primitives = Lisp.Def.Primitives
module Primitive = Lisp.Def.Primitive
module type Closure = Lisp.Def.Closure
type closure = Lisp.Def.closure
type program = Lisp.Program.t
module Load = Lisp.Parse
