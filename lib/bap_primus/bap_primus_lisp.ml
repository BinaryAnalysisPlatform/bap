open Core_kernel.Std
open Bap.Std
open Format
open Bap_c.Std
open Bap_primus_types
open Bap_primus_sexp

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Def = Bap_primus_lisp_def
module Parse = Bap_primus_lisp_parse
module Machine = Bap_primus_lisp_machine
module State = Bap_primus_state
module Context = Bap_primus_lisp_context

open Bap_primus_lisp_attributes


(* for external usage *)

type library = {
  mutable paths : string list;
  mutable features : string list;
  mutable log : formatter;
  mutable initialized : bool;
}


(* expose the primitive construction to OCaml users *)
module Primitive = struct
  type 'a t = 'a Def.primitive Def.t
  let create = Def.Primitive.create
end

module type Primitives = functor (Machine : Machine) ->  sig
  val defs : unit -> value Machine.t Primitive.t list
end

type primitives = (module Primitives)



type exn += Runtime_error of string
type exn += Link_error of string


type bindings = (var * value) list [@@deriving sexp_of]

type state = {
  primitives : primitives list;
  program : program;
  width : int;
  env : bindings;
  contexts : Context.t;
  paths : string list;
}

let inspect {env} = sexp_of_bindings env

let width_of_ctxt proj =
  Size.in_bits (Arch.addr_size (Project.arch proj))


let () = Exn.add_printer (function
    | Runtime_error msg -> Some ("primus runtime error - " ^ msg)
    | Link_error msg -> Some ("primus linker error - " ^ msg)
    | _ -> None)

let empty_program = {
  modules = String.Set.empty;
  advices = [];
  defs    = [];
  macros  = [];
  substs  = [];
}

let state = Bap_primus_state.declare ~inspect
    ~name:"lisp-env"
    ~uuid:"fc4b3719-f32c-4d0f-ad63-6167ab00b7f9"
    (fun proj -> {
         env = [];
         primitives = [];
         program = empty_program;
         paths = [Filename.current_dir_name];
         width = width_of_ctxt proj;
         contexts = Contexts.of_project proj;
       })

module Trace = struct
  module Observation = Bap_primus_observation
  let sexp_of_value {value=x} =
    let v = Format.asprintf "%a" Word.pp_hex x in
    Atom v
  let sexp_of_binding (_,x) = sexp_of_value x

  let sexp_of_enter (def,bs) =
    List (Atom (Def.name def) :: List.map bs ~f:sexp_of_binding)

  let sexp_of_leave (call,result) =
    List (Atom "#result-of" ::
               sexp_of_enter call ::
               [sexp_of_value result])

  let enter,entered =
    Observation.provide ~inspect:sexp_of_enter "lisp-call"

  let leave,left =
    Observation.provide ~inspect:sexp_of_leave "lisp-return"

end

module Locals(Machine : Machine) = struct
  open Machine.Syntax

  let rec update xs x ~f = match xs with
    | [] -> []
    | (x',w) :: xs when x' = x -> (x,f w) :: xs
    | xw :: xs -> xw :: update xs x ~f

  let replace xs x w = update xs x ~f:(fun _ -> w)

  let push v w s = {s with env = (v,w)::s.env}
  let pop n ({env} as s) = {s with env = List.drop env n}
end


module Make(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)
  module Eval = Bap_primus_interpreter.Make(Machine)
  module Vars = Locals(Machine)
  module Value = Bap_primus_value.Make(Machine)


  let error kind = Format.ksprintf
      (fun msg -> fun () -> Machine.raise (kind msg))

  let failf fmt = error (fun m -> Runtime_error m) fmt
  let linkerf fmt = error (fun m -> Link_error m) fmt

  let collect_externals s =
    List.fold ~init:String.Map.empty s.program.defs ~f:(fun toload def ->
        match Univ_map.find def.meta.attrs External.t with
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
    fprintf library.log "linking %s@\n" name;
    Machine.get () >>= fun proj ->
    Machine.Local.get state >>= fun s ->
    let arch = Project.arch proj in
    let args,ret,tid,addr = find_sub (Project.program proj) name in
    match Resolve.extern s.contexts defs name arch args with
    | res,None when tid = None -> Machine.return ()
    | res,None -> Machine.raise (Resolve.Failed (name,s.contexts,res))
    | _,Some (fn,bs) ->
      let module Code(Machine : Machine) = struct
        open Machine.Syntax
        module Eval = Bap_primus_interpreter.Make(Machine)
        module Lisp = Lisp(Machine)

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
            | _ -> failf "unsupported argument passing sematics" ())

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
          Lisp.eval_advices `before init name args >>= fun _ ->
          Machine.Local.update state
            ~f:(fun s -> {s with env = bs @ s.env}) >>= fun () ->
          Machine.Observation.make Trace.entered (fn,bs) >>= fun () ->
          Lisp.eval_exp fn.code.body >>= fun r ->
          Machine.Local.update state ~f:(Vars.pop (List.length bs)) >>= fun () ->
          Machine.Observation.make Trace.left ((fn,bs),r) >>= fun () ->
          Lisp.eval_advices `after r name args >>= fun r ->
          eval_ret r
      end in
      Linker.link ?addr ?tid ~name (module Code)

  let link_features () =
    Machine.Local.get state >>| collect_externals >>=
    Machine.Seq.iter ~f:link_feature

  let load_feature name =
    fprintf library.log "loading feature %s@\n" name;
    Machine.Local.update state ~f:(fun s -> {
          s with program = Load.feature
                     ~paths:s.paths
                     s.program
                     s.contexts name
        })


  let load_features fs =
    Machine.List.iter fs ~f:load_feature

  let add_directory path =
    fprintf library.log "adding path %s@\n" path;
    Machine.Local.update state ~f:(fun s -> {
          s with paths = s.paths @ [path]
        })

  let init_env proj = (object
    inherit [(var * value) Machine.t list] Term.visitor
    method! enter_term _ t env =
      match Term.get_attr t address with
      | None -> env
      | Some addr ->
        let binding =
          Value.of_word addr >>| fun addr ->
          {data = Term.name t; typ = Word}, addr in
        binding :: env
  end)#run proj [] |> Machine.List.all


  let init () =
    fprintf library.log "initializing lisp library@\n";
    Machine.List.iter library.paths ~f:add_directory >>= fun () ->
    load_features library.features >>=
    link_features >>= fun () ->
    fprintf library.log "the lisp machine is ready@\n";
    Machine.return ()

  let link_primitives p =
    Machine.Local.update state ~f:(fun s ->
        {s with primitives = p :: s.primitives})
end



let library = {
  paths = [];
  features = [];
  log = err_formatter;
  initialized = false;
}



module Make = Machine.Make

let init ?(log=std_formatter) ?(paths=[]) features  =
  if library.initialized
  then invalid_argf "Lisp library is already initialized" ();
  library.initialized <- true;
  library.paths <- paths;
  library.features <- features;
  library.log <- log;
  Bap_primus_main.add_component (module Make)
