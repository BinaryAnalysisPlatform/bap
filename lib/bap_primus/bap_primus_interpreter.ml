open Core_kernel.Std
open Bap.Std
open Bap_c.Std

open Format
open Bap_primus_types

module Observation = Bap_primus_observation
module State = Bap_primus_state

open Bap_primus_sexp

let enter_term, term_entered =
  Observation.provide ~inspect:sexp_of_tid "enter-term"
let leave_term, term_left =
  Observation.provide ~inspect:sexp_of_tid "leave-term"
let enter_pos,pos_entered =
  Observation.provide ~inspect:Pos.sexp_of_t "enter-pos"
let leave_pos,pos_left =
  Observation.provide ~inspect:Pos.sexp_of_t "leave-pos"
let sexp_of_term term = sexp_of_tid (Term.tid term)
let enter_sub,sub_entered =
  Observation.provide ~inspect:sexp_of_term "enter-sub"
let enter_arg,arg_entered =
  Observation.provide ~inspect:sexp_of_term "enter-arg"
let enter_blk,blk_entered =
  Observation.provide ~inspect:sexp_of_term "enter-blk"
let enter_phi,phi_entered =
  Observation.provide ~inspect:sexp_of_term "enter-phi"
let enter_def,def_entered =
  Observation.provide ~inspect:sexp_of_term "enter-def"
let enter_jmp,jmp_entered =
  Observation.provide ~inspect:sexp_of_term "enter-jmp"
let enter_top,top_entered =
  Observation.provide ~inspect:sexp_of_term "enter-top"
let leave_sub,sub_left =
  Observation.provide ~inspect:sexp_of_term "leave-sub"
let leave_arg,arg_left =
  Observation.provide ~inspect:sexp_of_term "leave-arg"
let leave_blk,blk_left =
  Observation.provide ~inspect:sexp_of_term "leave-blk"
let leave_phi,phi_left =
  Observation.provide ~inspect:sexp_of_term "leave-phi"
let leave_def,def_left =
  Observation.provide ~inspect:sexp_of_term "leave-def"
let leave_jmp,jmp_left =
  Observation.provide ~inspect:sexp_of_term "leave-jmp"
let leave_top,top_left =
  Observation.provide ~inspect:sexp_of_term "leave-top"

let enter_exp,exp_entered =
  Observation.provide ~inspect:sexp_of_exp "enter-exp"

let leave_exp,exp_left =
  Observation.provide ~inspect:sexp_of_exp "leave-exp"

let pc_change,pc_changed =
  Observation.provide ~inspect:sexp_of_word "pc-changed"

let halting,will_halt =
  Observation.provide ~inspect:sexp_of_unit "halting"

let sexp_of_insn insn = Sexp.Atom (Insn.to_string insn)

let loading,on_loading =
  Observation.provide ~inspect:sexp_of_value "loading"

let loaded,on_loaded =
  Observation.provide ~inspect:sexp_of_values "loaded"

let storing,on_storing =
  Observation.provide ~inspect:sexp_of_value "storing"

let stored,on_stored =
  Observation.provide ~inspect:sexp_of_values "stored"

let reading,on_reading =
  Observation.provide ~inspect:sexp_of_var "reading"

let read,on_read =
  Observation.provide ~inspect:sexp_of_binding "read"

let writing,on_writing =
  Observation.provide ~inspect:sexp_of_var "writing"

let written,on_written =
  Observation.provide ~inspect:sexp_of_binding "written"

let undefined,on_undefined =
  Observation.provide ~inspect:sexp_of_value "undefined"


let results r op = Sexp.List [op; sexp_of_value r]

let sexp_of_binop ((op,x,y),r) = results r @@ sexps [
    Bil.string_of_binop op;
    string_of_value x;
    string_of_value y;
  ]


let sexp_of_unop ((op,x),r) = results r @@ sexps [
    Bil.string_of_unop op;
    string_of_value x;
  ]

let sexp_of_cast ((t,s,v),r) = results r @@ sexps [
    Bil.string_of_cast t;
    string_of_int s;
    string_of_value v;
  ]

let sexp_of_extract ((hi,lo,x),r) = results r @@ sexps [
    string_of_int hi;
    string_of_int lo;
    string_of_value x;
  ]

let sexp_of_concat ((x,y),r) = results r @@ sexps [
    string_of_value x;
    string_of_value y;
  ]

let binop,on_binop =
  Observation.provide ~inspect:sexp_of_binop "binop"

let unop,on_unop =
  Observation.provide ~inspect:sexp_of_unop "unop"

let cast,on_cast =
  Observation.provide ~inspect:sexp_of_cast "cast"

let extract,on_extract =
  Observation.provide ~inspect:sexp_of_extract "extract"

let concat,on_concat =
  Observation.provide ~inspect:sexp_of_concat "concat"

let const,on_const =
  Observation.provide ~inspect:sexp_of_value "const"

let sexp_of_name = function
  | `symbol name -> Sexp.Atom name
  | `tid tid -> Sexp.Atom (Tid.name tid)
  | `addr addr -> Sexp.Atom (Addr.string_of_value addr)

type state = {
  addr : addr;
  curr : pos;
}

let sexp_of_state {curr} =
  Pos.sexp_of_t curr

let null proj =
  let size = Arch.addr_size (Project.arch proj) in
  Addr.zero (Size.in_bits size)

let state = Bap_primus_machine.State.declare
    ~uuid:"14a17161-173b-46da-9e95-7819104cc220"
    ~name:"interpreter"
    ~inspect:sexp_of_state
    (fun proj  ->
       let prog = Project.program proj in
       Pos.{
         addr = null proj;
         curr = Top {me=prog; up=Nil};
       })


type exn += Halt
type exn += Runtime_error of string

let () =
  Exn.add_printer (function
      | Runtime_error msg ->
        Some (sprintf "Bap_primus runtime error: %s" msg)
      | Halt -> Some "Halt"
      | _ -> None)


module Make (Machine : Machine) = struct
  open Machine.Syntax

  module Eval = Eval.Make(Machine)
  module Memory = Bap_primus_memory.Make(Machine)
  module Env = Bap_primus_env.Make(Machine)
  module Linker = Bap_primus_linker.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)

  type 'a m = 'a Machine.t

  let (!!) = Machine.Observation.make

  let failf fmt = Format.ksprintf (fun msg ->
      fun () -> Machine.raise (Runtime_error msg)) fmt


  let value = Value.of_word

  let word_of_type = function
    | Type.Mem _ -> Word.b0
    | Type.Imm t -> Word.zero t

  let undefined t =
    value (word_of_type t) >>= fun r ->
    !!on_undefined r >>| fun () -> r

  let set v x =
    !!on_writing v >>= fun () ->
    Env.set v x >>= fun () ->
    !!on_written (v,x)

  let get v =
    !!on_reading v >>= fun () ->
    Env.get v >>= fun r ->
    !!on_read (v,r) >>| fun () -> r

  let binop op x y =
    try
      value (Bil.Apply.binop op x.value y.value) >>= fun r ->
      !!on_binop ((op,x,y),r) >>| fun () -> r
    with Division_by_zero -> failf "Division by zero" ()


  let unop op x =
    value (Bil.Apply.unop op x.value) >>= fun r ->
    !!on_unop ((op,x),r) >>| fun () -> r

  let cast t s x =
    value (Bil.Apply.cast t s x.value) >>= fun r ->
    !!on_cast ((t,s,x),r) >>| fun () -> r

  let concat x y =
    value (Word.concat x.value y.value) >>= fun r ->
    !!on_concat ((x,y),r) >>| fun () -> r

  let extract ~hi ~lo x =
    value (Word.extract_exn ~hi ~lo x.value) >>= fun r ->
    !!on_extract ((hi,lo,x),r) >>| fun () -> r

  let const c =
    value c >>= fun r ->
    !!on_const r >>| fun () -> r

  let rec eval_exp x =
    let eval = function
      | Bil.Load (Bil.Var _, a,_,`r8) -> eval_load a
      | Bil.Store (m,a,x,_,`r8) -> eval_store m a x
      | Bil.BinOp (op, x, y) -> eval_binop op x y
      | Bil.UnOp (op,x) -> eval_unop op x
      | Bil.Var v -> eval_var v
      | Bil.Int c -> eval_int c
      | Bil.Cast (t,s,x) -> eval_cast t s x
      | Bil.Unknown (x,typ) -> eval_unknown x typ
      | Bil.Extract (hi,lo,x) -> eval_extract hi lo x
      | Bil.Concat (x,y) -> eval_concat x y
      | exp ->
        invalid_argf "precondition failed: denormalized exp: %s"
          (Exp.to_string exp) () in
    !!exp_entered x >>= fun () ->
    eval x >>= fun r ->
    !!exp_left x >>| fun () -> r
  and eval_load a =
    eval_exp a >>= fun a ->
    !!on_loading a >>= fun () ->
    Memory.load a.value >>= value >>= fun r ->
    !!on_loaded (a,r) >>| fun () -> r
  and eval_store m a x =
    eval_exp m >>= fun _ ->
    eval_exp a >>= fun a ->
    eval_exp x >>= fun x ->
    !!on_storing a >>= fun () ->
    Memory.store a.value x.value >>= fun () ->
    !!on_stored (a,x) >>| fun () -> a
  and eval_binop op x y =
    eval_exp x >>= fun x ->
    eval_exp y >>= fun y ->
    binop op x y
  and eval_unop op x =
    eval_exp x >>= fun x ->
    unop op x
  and eval_var = get
  and eval_int = const
  and eval_cast t s x =
    eval_exp x >>= fun x ->
    cast t s x
  and eval_unknown x t = undefined t
  and eval_extract hi lo x =
    eval_exp x >>= fun x ->
    extract ~hi ~lo x
  and eval_concat x y =
    eval_exp x >>= fun x ->
    eval_exp y >>= fun y ->
    concat x y

  let eval_exp x = eval_exp (Exp.simpl (Exp.normalize x))

  let mem =
    Machine.get () >>| fun proj ->
    let (module Target) = target_of_arch (Project.arch proj) in
    Target.CPU.mem

  let load a e s =
    mem >>= fun m ->
    eval_exp Bil.(Load (var m, int a.value,e,s))

  let store a x e s =
    mem >>= fun m ->
    Machine.ignore_m @@
    eval_exp Bil.(Store (var m,int a.value,int x.value,e,s))


  let update_pc t =
    match Term.get_attr t address with
    | None -> Machine.return ()
    | Some addr ->
      Machine.Local.get state >>= fun s ->
      Machine.Local.put state {s with addr} >>= fun () ->
      if Addr.(s.addr <> addr)
      then !!pc_changed addr
      else Machine.return ()

  let enter cls curr t =
    !!pos_entered curr >>= fun () ->
    !!term_entered (Term.tid t) >>= fun () ->
    Term.switch cls t
      ~program:(!!top_entered)
      ~sub:(!!sub_entered)
      ~arg:(!!arg_entered)
      ~blk:(!!blk_entered)
      ~phi:(!!phi_entered)
      ~def:(!!def_entered)
      ~jmp:(!!jmp_entered)

  let leave cls curr t =
    Term.switch cls t
      ~program:(!!top_left)
      ~sub:(!!sub_left)
      ~arg:(!!arg_left)
      ~blk:(!!blk_left)
      ~phi:(!!phi_left)
      ~def:(!!def_left)
      ~jmp:(!!jmp_left) >>= fun () ->
    !!term_left (Term.tid t) >>= fun () ->
    !!pos_left curr


  let term return cls f t =
    Machine.Local.get state >>= fun s ->
    match Pos.next s.curr cls t with
    | Error err -> Machine.raise err
    | Ok curr ->
      update_pc t >>= fun () ->
      Machine.Local.update state (fun s -> {s with curr}) >>= fun () ->
      enter cls curr t >>= fun () ->
      Machine.catch (f t)
        (fun exn -> leave cls curr t >>= fun () -> Machine.raise exn)
      >>= fun r ->
      leave cls curr t >>= fun () ->
      return r

  let normal = Machine.return

  let halt =
    !!will_halt () >>= fun () -> Machine.raise Halt

  let (:=) v x  = eval_exp x >>= set v
  let def t = Def.lhs t := Def.rhs t
  let def = term normal def_t def

  let label : label -> _ = function
    | Direct t -> Linker.exec (`tid t)
    | Indirect x ->
      eval_exp x >>= fun {value} ->
      Linker.exec (`addr value)

  let call c =
    label (Call.target c) >>= fun () ->
    match Call.return c with
    | Some t -> label t
    | None -> failf "a non-return call returned" ()

  let goto c = label c
  let ret l = label l
  let interrupt n r = failf "not implemented" ()
  let jump t = match Jmp.kind t with
    | Call c -> call c
    | Goto l -> goto l
    | Ret l -> ret l
    | Int (n,r) -> interrupt n r

  let jmp t = eval_exp (Jmp.cond t) >>| fun {value} -> Word.is_one value
  let jmp = term normal jmp_t jmp


  let blk t =
    (* todo add the phi nodes, or think at least.. *)
    Machine.Seq.iter (Term.enum def_t t) ~f:def >>= fun () ->
    Machine.Seq.find (Term.enum jmp_t t) ~f:jmp

  let finish = function
    | None -> Machine.return ()
    | Some code -> jump code

  let blk = term finish blk_t blk

  let arg_def t = match Arg.intent t with
    | None | Some In -> Arg.lhs t := Arg.rhs t
    | _ -> Machine.return ()

  let arg_def = term normal arg_t arg_def

  let arg_use t = match Arg.intent t with
    | None | Some Out -> Arg.lhs t := Arg.rhs t
    | _ -> Machine.return ()

  let arg_use = term normal arg_t arg_use

  let eval_entry = function
    | None -> Machine.return ()
    | Some t -> blk t

  let sub t =
    let iter f = Machine.Seq.iter (Term.enum arg_t t) ~f in
    iter arg_def >>= fun () ->
    eval_entry (Term.first blk_t t) >>= fun () ->
    iter arg_use

  let sub = term normal sub_t sub

  let pos = Machine.Local.get state >>| fun {curr} -> curr

  let pc = Machine.Local.get state >>| fun {addr} -> addr
end

module Init(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)

  let is_linked name t = [
    Linker.is_linked (`tid (Term.tid t));
    Linker.is_linked (`symbol (name t));
  ] |> Machine.List.all >>| List.exists ~f:ident >>= function
    | true -> Machine.return true
    | false -> match Term.get_attr t address with
      | None -> Machine.return false
      | Some x -> Linker.is_linked (`addr x)


  let link name code t m =
    m >>= fun () -> is_linked name t >>= function
    | true -> Machine.return ()
    | false ->
      Linker.link
        ~name:(name t)
        ~tid:(Term.tid t)
        ?addr:(Term.get_attr t address)
        code

  let linker = object
    inherit [unit Machine.t] Term.visitor
    method! enter_blk t =
      let module Code(Machine : Machine) = struct
        module Interp = Make(Machine)
        let exec = Interp.blk t
      end in
      link Term.name (module Code) t
    method! enter_sub t =
      let module Code(Machine : Machine) = struct
        module Interp = Make(Machine)
        let exec = Interp.sub t
      end in
      link Sub.name (module Code) t
  end

  let run () =
    Machine.get () >>= fun proj ->
    linker#run (Project.program proj) (Machine.return ())
end
