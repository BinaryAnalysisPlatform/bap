open Core_kernel.Std
open Bap.Std
open Bap_c.Std

open Format
open Bap_primus_types

module Context = Bap_primus_context
module Observation = Bap_primus_observation
module State = Bap_primus_state

open Bap_primus_sexp

let sexp_of_level = Bap_primus_context.sexp_of_level

let enter_term, term_entered =
  Observation.provide ~inspect:sexp_of_tid "enter-term"
let leave_term, term_left =
  Observation.provide ~inspect:sexp_of_tid "leave-term"

let enter_level,level_entered =
  Observation.provide ~inspect:sexp_of_level "enter-level"

let leave_level, level_left =
  Observation.provide ~inspect:sexp_of_level "leave-level"

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

let pc_change,pc_changed =
  Observation.provide ~inspect:sexp_of_word "pc-changed"

let sexp_of_insn insn = Sexp.Atom (Insn.to_string insn)

let exec_insn,insn_entered =
  Observation.provide ~inspect:sexp_of_word "exec-insn"


let sexp_of_name = function
  | `symbol name -> Sexp.Atom name
  | `tid tid -> Sexp.Atom (Tid.name tid)
  | `addr addr -> Sexp.Atom (Addr.string_of_value addr)

let sexp_of_call name = Sexp.List [
    Sexp.Atom "call";
    sexp_of_name name;
  ]

let call,calling =
  Observation.provide ~inspect:sexp_of_call "call"


type state = {
  curr : Context.level;
}

let sexp_of_state {curr} =
  Context.sexp_of_level curr

let collect_blks prog =
  (object(self)
    inherit [tid Addr.Map.t] Term.visitor
    method! enter_blk term addrs =
      match Term.get_attr term address with
      | None -> addrs
      | Some addr -> Map.add addrs ~key:addr ~data:(Term.tid term)
  end)#run prog Addr.Map.empty

let null ctxt =
  let size = Arch.addr_size (Project.arch ctxt#project) in
  Addr.zero (Size.in_bits size)

let state = Bap_primus_machine.State.declare
    ~uuid:"14a17161-173b-46da-9e95-7819104cc220"
    ~name:"interpreter"
    ~inspect:sexp_of_state
    Context.Level.(fun proj  ->
        let prog = Project.program proj in
        {
          curr = Top {me=prog; up=Nil};
        })

module Make (Machine : Machine) = struct
  open Machine.Syntax

  module Eval = Eval.Make(Machine)
  module Memory = Bap_primus_memory.Make(Machine)
  module Env = Bap_primus_env.Make(Machine)
  module Linker = Bap_primus_linker.Make(Machine)

  type 'a m = 'a Machine.t
  type error += Runtime_error of string

  let (!!) = Machine.Observation.make

  let () =
    Bap_primus_error.add_printer (function
        | Runtime_error msg ->
          Some (sprintf "Bap_primus runtime error: %s" msg)
        | _ -> None)


  let failf fmt = Format.ksprintf (fun msg ->
      fun () -> Machine.fail (Runtime_error msg)) fmt

  let sema = object
    inherit [word,word] Eval.t
    method value_of_word = Machine.return
    method word_of_value x = Machine.return (Some x)
    method undefined = failf "undefined value" ()
    method storage_of_value x = Machine.return (Some x)
    method lookup = Env.get 
    method update = Env.set 

    method load base addr =
      let addr = Addr.(base + addr) in
      Memory.load addr

    method store base addr data =
      let addr = Addr.(base + addr) in
      Memory.save addr data >>= fun () ->
      Machine.return base
  end

  let term cls f t =
    Machine.Local.get state >>= fun {curr} -> 
    match Context.Level.next curr cls t with
    | Error err -> Machine.fail err
    | Ok curr ->
      !!level_entered curr >>= fun () -> 
      Machine.Local.put state {curr} >>= fun () -> 
      !!term_entered (Term.tid t) >>= fun () ->
      Term.switch cls t 
        ~program:(!!top_entered)
        ~sub:(!!sub_entered)
        ~arg:(!!arg_entered)
        ~blk:(!!blk_entered)
        ~phi:(!!phi_entered)
        ~def:(!!def_entered)
        ~jmp:(!!jmp_entered) >>= fun () -> 
      f t >>= fun r -> 
      Term.switch cls t 
        ~program:(!!top_left)
        ~sub:(!!sub_left)
        ~arg:(!!arg_left)
        ~blk:(!!blk_left)
        ~phi:(!!phi_left)
        ~def:(!!def_left)
        ~jmp:(!!jmp_left) >>= fun () -> 
      !!term_left (Term.tid t) >>= fun () -> 
      !!level_left curr >>= fun () -> 
      Machine.return r


  let exp = sema#eval_exp
  let (:=) v x  = exp x >>= sema#update v 
  let def t = Def.lhs t := Def.rhs t
  let def = term def_t def 

  let label : label -> _ = function
    | Direct t -> Linker.exec (`tid t)
    | Indirect x -> 
      exp x >>= fun x -> 
      Linker.exec (`addr x)

  let call c = label (Call.target c)
  let goto c = label c
  let ret l = label l
  let interrupt n r = failf "not implemented" ()
  let jump t = match Jmp.kind t with
    | Call c -> call c
    | Goto l -> goto l
    | Ret l -> ret l
    | Int (n,r) -> interrupt n r

  let jmp t = exp (Jmp.cond t) >>| Word.is_zero
  let jmp = term jmp_t jmp


  let blk t =
    (* todo add the phi nodes, or think at least.. *)
    Machine.Seq.iter (Term.enum def_t t) ~f:def >>= fun () ->
    Machine.Seq.find (Term.enum jmp_t t) ~f:jmp >>= function
    | None -> Machine.return ()
    | Some t -> jump t

  let blk = term blk_t blk 

  let arg_def t = match Arg.intent t with
    | None | Some In -> Arg.lhs t := Arg.rhs t
    | _ -> Machine.return ()

  let arg_def = term arg_t arg_def

  let arg_use t = match Arg.intent t with
    | None | Some Out -> Arg.lhs t := Arg.rhs t
    | _ -> Machine.return ()

  let arg_use = term arg_t arg_use

  let eval_entry = function
    | None -> Machine.return ()
    | Some t -> blk t

  let sub t =
    let iter f = Machine.Seq.iter (Term.enum arg_t t) ~f in
    iter arg_def >>= fun () ->
    eval_entry (Term.first blk_t t) >>= fun () ->
    iter arg_use

  let sub = term sub_t sub

  let pos = Machine.Local.get state >>| fun {curr} -> curr

  let halt = failf "machine halted" ()
end

module Main(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)
  module Interp = Make(Machine)

  let linker = object 
    inherit [unit Machine.t] Term.visitor
    method! enter_blk t m = m >>= fun () -> Interp.blk t
    method! enter_sub t m = m >>= fun () -> Interp.sub t
  end

  let init () =
    Machine.get () >>= fun proj -> 
    linker#run (Project.program proj) (Machine.return ())
end

let () = Bap_primus_machine.add_component (module Main)
