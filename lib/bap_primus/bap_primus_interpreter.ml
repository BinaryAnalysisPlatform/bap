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

let pc_change,pc_changed =
  Observation.provide ~inspect:sexp_of_word "pc-changed"

let halting,will_halt =
  Observation.provide ~inspect:sexp_of_unit "halting"

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

  type 'a m = 'a Machine.t

  let (!!) = Machine.Observation.make

  let failf fmt = Format.ksprintf (fun msg ->
      fun () -> Machine.raise (Runtime_error msg)) fmt

  let undefined = Word.of_int 0 ~width:0

  let sema = object
    inherit [word,word] Eval.t
    method value_of_word = Machine.return
    method word_of_value x = Machine.return (Some x)
    method undefined = Machine.return undefined
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

  let update_pc t = 
    match Term.get_attr t address with
    | None -> Machine.return ()
    | Some addr ->
      Machine.Local.get state >>= fun s -> 
      Machine.Local.put state {s with addr} >>= fun () -> 
      if Addr.(s.addr <> addr) 
      then !!pc_changed addr
      else Machine.return ()

  let term cls f t =
    Machine.Local.get state >>= fun s -> 
    match Pos.next s.curr cls t with
    | Error err -> Machine.raise err
    | Ok curr ->
      update_pc t >>= fun () ->
      Machine.Local.update state (fun s -> {s with curr}) >>= fun () -> 
      !!pos_entered curr >>= fun () -> 
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
      !!pos_left curr >>= fun () ->
      Machine.return r

  let halt = 
    !!will_halt () >>= fun () -> Machine.raise Halt
  let exp = sema#eval_exp
  let (:=) v x  = exp x >>= sema#update v 
  let def t = Def.lhs t := Def.rhs t
  let def = term def_t def 

  let label : label -> _ = function
    | Direct t -> Linker.exec (`tid t)
    | Indirect x -> 
      exp x >>= fun x -> 
      Linker.exec (`addr x)

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

  let jmp t = exp (Jmp.cond t) >>| Word.is_one
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
end

module Init(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)

  let is_linked name t = [
    Linker.is_linked (`tid (Term.tid t));
    Linker.is_linked (`symbol (name t));
  ] |> Machine.List.all >>| (fun xs -> List.mem xs true) >>= function
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

