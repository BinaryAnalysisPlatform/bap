open Core_kernel
open Bap.Std
open Bap_c.Std

open Format
open Bap_primus_types

module Primus = struct
  module Env = Bap_primus_env
  module Linker = Bap_primus_linker
  module Machine = Bap_primus_machine
  module Memory = Bap_primus_memory
  module Observation = Bap_primus_observation
  module State = Bap_primus_state
  module Value = Bap_primus_value
end

open Primus

module Time = struct
  include Int
  let clocks = ident
  let of_clocks = of_int
end

open Bap_primus_sexp

let clock,tick =
  Observation.provide ~inspect:Time.sexp_of_t "clock"
    ~desc:"Occurs on each clock (operation) of the interpreter."

let memory_switch,switching_memory =
  let inspect = Primus.Memory.Descriptor.sexp_of_t in
  Observation.provide ~inspect "memory-switch"
    ~desc:"Occurs when the memory bank is switched."

let enter_term, term_entered =
  Observation.provide ~inspect:sexp_of_tid "enter-term"
    ~desc:"Occurs before the given term."

let leave_term, term_left =
  Observation.provide ~inspect:sexp_of_tid "leave-term"
    ~desc:"Occurs after the given term is evaluated."

let enter_pos,pos_entered =
  Observation.provide ~inspect:Pos.sexp_of_t "enter-pos"
    ~desc:"Occurs before the given position is evaluated."

let leave_pos,pos_left =
  Observation.provide ~inspect:Pos.sexp_of_t "leave-pos"
    ~desc:"Occurs after the given position is evaluated."

let sexp_of_term term = sexp_of_tid (Term.tid term)

let enter_sub,sub_entered =
  Observation.provide ~inspect:sexp_of_term "enter-sub"
    ~desc:"Occurs before the given term is evaluated."

let enter_arg,arg_entered =
  Observation.provide ~inspect:sexp_of_term "enter-arg"
    ~desc:"Occurs before the given term is evaluated."

let enter_blk,blk_entered =
  Observation.provide ~inspect:sexp_of_term "enter-blk"
    ~desc:"Occurs before the given term is evaluated."

let enter_phi,phi_entered =
  Observation.provide ~inspect:sexp_of_term "enter-phi"
    ~desc:"Occurs before the given term is evaluated."

let enter_def,def_entered =
  Observation.provide ~inspect:sexp_of_term "enter-def"
    ~desc:"Occurs before the given term is evaluated."

let enter_jmp,jmp_entered =
  Observation.provide ~inspect:sexp_of_term "enter-jmp"
    ~desc:"Occurs before the given term is evaluated."

let enter_top,top_entered =
  Observation.provide ~inspect:sexp_of_term "enter-top"
    ~desc:"Occurs before the given term is evaluated."

let leave_sub,sub_left =
  Observation.provide ~inspect:sexp_of_term "leave-sub"
    ~desc:"Occurs after the given term is evaluated."

let leave_arg,arg_left =
  Observation.provide ~inspect:sexp_of_term "leave-arg"
    ~desc:"Occurs after the given term is evaluated."

let leave_blk,blk_left =
  Observation.provide ~inspect:sexp_of_term "leave-blk"
    ~desc:"Occurs after the given term is evaluated."

let leave_phi,phi_left =
  Observation.provide ~inspect:sexp_of_term "leave-phi"
    ~desc:"Occurs after the given term is evaluated."

let leave_def,def_left =
  Observation.provide ~inspect:sexp_of_term "leave-def"
    ~desc:"Occurs after the given term is evaluated."

let leave_jmp,jmp_left =
  Observation.provide ~inspect:sexp_of_term "leave-jmp"
    ~desc:"Occurs after the given term is evaluated."

let leave_top,top_left =
  Observation.provide ~inspect:sexp_of_term "leave-top"
    ~desc:"Occurs after the given term is evaluated."

let enter_exp,exp_entered =
  Observation.provide ~inspect:sexp_of_exp "enter-exp"
    ~desc:"Occurs before the given exp is evaluated."

let leave_exp,exp_left =
  Observation.provide ~inspect:sexp_of_exp "leave-exp"
    ~desc:"Occurs after the given exp is evaluated."

let pc_change,pc_changed =
  Observation.provide ~inspect:sexp_of_word "pc-changed"
    ~desc:"Occurs after the program counter is changed."

let halting,will_halt =
  Observation.provide ~inspect:sexp_of_unit "halting"
    ~desc:"Occurs before the halt operation is posted."


let division_by_zero,will_divide_by_zero =
  Observation.provide ~inspect:sexp_of_unit "division-by-zero"
    ~desc:"Occurs before the division by zero happens."

let cfi_violation,cfi_will_diverge =
  Observation.provide ~inspect:sexp_of_word "cfi-violation"
    ~desc:"Occurs before the control flow violation is commited."

let segfault, will_segfault =
  Observation.provide ~inspect:sexp_of_word "segfault"
    ~desc:"Occurs just before the segmentation fault."

let pagefault,page_fail =
  Observation.provide ~inspect:sexp_of_word "pagefault"
    ~desc:"Occurs on pagefault."

let interrupt,will_interrupt =
  Observation.provide ~inspect:sexp_of_int "interrupt"
    ~desc:"Occurs before an interrupt."

let sexp_of_insn insn = Sexp.Atom (Insn.to_string insn)

let loading,on_loading =
  Observation.provide ~inspect:sexp_of_value "loading"
    ~desc:"Occurs just before the given address is dereferenced."

let loaded,on_loaded =
  Observation.provide ~inspect:sexp_of_values "loaded"
    ~desc:"Occurs after the given address is read."

let storing,on_storing =
  Observation.provide ~inspect:sexp_of_value "storing"
    ~desc:"Occurs before the value is stored at the address."

let stored,on_stored =
  Observation.provide ~inspect:sexp_of_values "stored"
    ~desc:"Occurs after the value is stored at the address."

let reading,on_reading =
  Observation.provide ~inspect:sexp_of_var "reading"
    ~desc:"Occurs before the given variable is read."

let read,on_read =
  Observation.provide ~inspect:sexp_of_binding "read"
    ~desc:"Occurs after the given variable is evaluated to the value."

let writing,on_writing =
  Observation.provide ~inspect:sexp_of_var "writing"
    ~desc:"Occurs before the value is assigned to the variable."

let written,on_written =
  Observation.provide ~inspect:sexp_of_binding "written"
    ~desc:"Occurs after the value is assigned to the variable."

let undefined,on_undefined =
  Observation.provide ~inspect:sexp_of_value "undefined"
    ~desc:"Occurs when an undefined value is created."

let jumping,will_jump =
  Observation.provide ~inspect:sexp_of_values "jumping"
    ~desc:"Occurs before the jump to the given destination."

let eval_cond,on_cond =
  Observation.provide ~inspect:sexp_of_value "eval-cond"
    ~desc:"Occurs when the jump condition is evaluated."

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

let sexp_of_ite ((cond, yes, no), r) = results r @@ sexps [
    string_of_value cond;
    string_of_value yes;
    string_of_value no;
  ]

let binop,on_binop =
  Observation.provide ~inspect:sexp_of_binop "binop"
    ~desc:"Occurs on each binary operation."

let unop,on_unop =
  Observation.provide ~inspect:sexp_of_unop "unop"
    ~desc:"Occurs on each unary operation."

let cast,on_cast =
  Observation.provide ~inspect:sexp_of_cast "cast"
    ~desc:"Occurs on each cast."

let extract,on_extract =
  Observation.provide ~inspect:sexp_of_extract "extract"
    ~desc:"Occurs on each extract operation."

let concat,on_concat =
  Observation.provide ~inspect:sexp_of_concat "concat"
    ~desc:"Occurs on each concat operation."

let const,on_const =
  Observation.provide ~inspect:sexp_of_value "const"
    ~desc:"Occurs on each constant."

let ite, on_ite =
  Observation.provide ~inspect:sexp_of_ite "ite"
    ~desc:"Occurs on each ite expression."


let sexp_of_name = function
  | `symbol name -> Sexp.Atom name
  | `tid tid -> Sexp.Atom (Tid.name tid)
  | `addr addr -> Sexp.Atom (Addr.string_of_value addr)

type scope = {
  stack : (var * value) list;
  bound : int Var.Map.t;
}

type state = {
  time : Time.t;
  addr : addr;
  curr : pos;
  lets : scope; (* lexical context *)
  prompts : tid list;
}

let sexp_of_state {curr} =
  Pos.sexp_of_t curr

let null proj =
  let size = Arch.addr_size (Project.arch proj) in
  Addr.zero (Size.in_bits size)

let empty_scope = {
  stack = [];
  bound = Var.Map.empty;
}

let state = Primus.Machine.State.declare
    ~uuid:"14a17161-173b-46da-9e95-7819104cc220"
    ~name:"interpreter"
    ~inspect:sexp_of_state
    (fun proj  ->
       let prog = Project.program proj in
       Pos.{
         time = Time.zero;
         addr = null proj;
         curr = Top {me=prog; up=Nil};
         lets = empty_scope;
         prompts = [];
       })

type exn += Halt
type exn += Division_by_zero
type exn += Segmentation_fault of addr
type exn += Cfi_violation of addr
type exn += Runtime_error of string


let () =
  Exn.add_printer (function
      | Runtime_error msg ->
        Some (sprintf "Primus interpreter runtime error: %s" msg)
      | Halt -> Some "Halt"
      | Segmentation_fault x ->
        Some (asprintf "Segmentation fault at %a" Addr.pp_hex x)
      | Division_by_zero -> Some "Division by zero"
      | Cfi_violation where ->
        Some (asprintf "CFI violation at %a" Addr.pp_hex where)
      | _ -> None)


let division_by_zero_handler = "__primus_division_by_zero_handler"
let pagefault_handler =  "__primus_pagefault_handler"
let cfi_violation_handler = "__primus_cfi_violation_handler"

module Make (Machine : Machine) = struct
  open Machine.Syntax

  module Eval = Eval.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Code = Linker.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  type 'a m = 'a Machine.t

  let (!!) = Machine.Observation.make

  let post = Machine.Observation.post

  let failf fmt = Format.ksprintf (fun msg ->
      fun () -> Machine.raise (Runtime_error msg)) fmt


  let value = Value.of_word

  let word_of_type = function
    | Type.Imm t -> Word.zero t
    | Type.Mem _ | Type.Unk -> Word.b0

  let undefined t =
    value (word_of_type t) >>= fun r ->
    !!on_undefined r >>| fun () -> r

  let get v =
    !!on_reading v >>= fun () ->
    Env.get v >>= fun r ->
    post on_read ~f:(fun k -> k (v,r)) >>| fun () -> r

  let call_when_provided name =
    Code.is_linked (`symbol name) >>= fun provided ->
    if provided then Code.exec (`symbol name) >>| fun () -> true
    else Machine.return false

  let tick =
    Machine.Local.get state >>= fun s ->
    !!tick s.time >>= fun () ->
    Machine.Local.put state {
      s with time = Time.succ s.time;
    }


  let unop op x =
    value (Bil.Apply.unop op x.value) >>= fun r ->
    tick >>= fun () ->
    post on_unop ~f:(fun k -> k ((op,x),r)) >>| fun () -> r

  let cast t s x =
    value (Bil.Apply.cast t s x.value) >>= fun r ->
    post on_cast ~f:(fun k -> k ((t,s,x),r)) >>| fun () -> r

  let concat x y =
    value (Word.concat x.value y.value) >>= fun r ->
    post on_concat ~f:(fun k -> k ((x,y),r)) >>| fun () -> r

  let extract ~hi ~lo x =
    value (Word.extract_exn ~hi ~lo x.value) >>= fun r ->
    post on_extract ~f:(fun k -> k ((hi,lo,x),r)) >>| fun () -> r


  let coerce s x =
    if Value.bitwidth x <> s
    then extract ~hi:(s-1) ~lo:0 x
    else Machine.return x

  let binop op x y = match op with
    | Bil.DIVIDE | Bil.SDIVIDE
    | Bil.MOD | Bil.SMOD
      when Word.is_zero y.value ->
      !!will_divide_by_zero () >>= fun () ->
      call_when_provided division_by_zero_handler >>= fun called ->
      if called
      then undefined (Type.Imm (Word.bitwidth x.value))
      else Machine.raise Division_by_zero
    | _ ->
      let s = max (Value.bitwidth x) (Value.bitwidth y) in
      coerce s x >>= fun x ->
      coerce s y >>= fun y ->
      value (Bil.Apply.binop op x.value y.value) >>= fun r ->
      tick >>= fun () ->
      post on_binop ~f:(fun k -> k ((op,x,y),r)) >>| fun () -> r

  let const c =
    value c >>= fun r ->
    !!on_const r >>| fun () -> r

  let set v x =
    !!on_writing v >>= fun () ->
    let x = match Var.typ v with
      | Unk | Mem _ -> Machine.return x
      | Imm m ->
        if Value.bitwidth x <> m
        then extract ~hi:(m-1) ~lo:0 x
        else Machine.return x in
    x >>= fun x ->
    Env.set v x >>= fun () ->
    post on_written ~f:(fun k -> k (v,x))

  let trapped_memory_access access =
    Machine.catch access (function
        | Bap_primus_memory.Pagefault a ->
          !!page_fail a >>= fun () ->
          call_when_provided pagefault_handler >>= fun trapped ->
          if trapped then access
          else
            !!will_segfault a >>= fun () ->
            Machine.raise (Segmentation_fault a)
        | exn -> Machine.raise exn)

  let load_byte a =
    !!on_loading a >>= fun () ->
    trapped_memory_access (Memory.get a.value) >>= fun r ->
    tick >>= fun () ->
    post on_loaded ~f:(fun k -> k (a,r)) >>| fun () -> r

  let store_byte a x =
    !!on_storing a >>= fun () ->
    trapped_memory_access (Memory.set a.value x) >>= fun () ->
    tick >>= fun () ->
    post on_stored ~f:(fun k -> k (a,x))

  let ite cond yes no =
    value (if Word.is_one cond.value then yes.value else no.value) >>= fun r ->
    post on_ite ~f:(fun k -> k ((cond, yes, no), r)) >>| fun () -> r


  let get_lexical scope v =
    List.Assoc.find_exn scope ~equal:Var.equal v

  let update_lexical f =
    Machine.Local.update state ~f:(fun s -> {
          s with lets = f s.lets
        })

  let push_lexical v x = update_lexical @@ fun s -> {
      stack = (v,x) :: s.stack;
      bound = Map.update s.bound v ~f:(function
          | None -> 1
          | Some n -> n + 1)
    }

  let pop_lexical v = update_lexical @@ fun s -> {
      stack = List.tl_exn s.stack;
      bound = Map.change s.bound v ~f:(function
          | None -> None
          | Some 1 -> None
          | Some n -> Some (n-1))
    }

  let memory typ name = match typ with
    | Type.Mem (ks,vs) ->
      let ks = Size.in_bits ks and vs = Size.in_bits vs in
      Primus.Memory.Descriptor.create ks vs name
    | _ as t  -> failwithf "type error - load from %s:%a"  name Type.pps t ()

  let rec memory_of_storage : exp -> _ = function
    | Var v -> memory (Var.typ v) (Var.name v)
    | Unknown (_,typ) -> memory typ "unknown"
    | Store (s,_,_,_,_)
    | Ite (_,s,_)
    | Let (_,_,s) -> memory_of_storage s
    | x -> failwithf "expression `%a' is no a storage" Exp.pps x ()


  let switch_memory m =
    let m = memory_of_storage m in
    Memory.memory >>= fun m' ->
    if Primus.Memory.Descriptor.equal m m'
    then Machine.return ()
    else
      !!switching_memory m >>= fun () ->
      Memory.switch m


  let time =
    Machine.Local.get state >>| fun s -> s.time


  let rec eval_exp x =
    let eval = function
      | Bil.Load (m,a,_,_) -> eval_load m a
      | Bil.Store (m,a,x,_,_) -> eval_store m a x
      | Bil.BinOp (op, x, y) -> eval_binop op x y
      | Bil.UnOp (op,x) -> eval_unop op x
      | Bil.Var v -> eval_var v
      | Bil.Int c -> eval_int c
      | Bil.Cast (t,s,x) -> eval_cast t s x
      | Bil.Unknown (x,typ) -> eval_unknown x typ
      | Bil.Extract (hi,lo,x) -> eval_extract hi lo x
      | Bil.Concat (x,y) -> eval_concat x y
      | Bil.Ite (cond, yes, no) -> eval_ite cond yes no
      | Bil.Let (v,x,y) -> eval_let v x y in
    !!exp_entered x >>= fun () ->
    eval x >>= fun r ->
    !!exp_left x >>| fun () -> r
  and eval_let v x y =
    eval_exp x >>= fun x ->
    push_lexical v x >>= fun () ->
    eval_exp y >>= fun r ->
    pop_lexical v >>| fun () ->  r
  and eval_ite cond yes no =
    eval_exp yes >>= fun yes ->
    eval_exp no >>= fun no ->
    eval_exp cond >>= fun cond ->
    ite cond yes no
  and eval_load m a =
    eval_exp a >>= fun a ->
    switch_memory m >>= fun () ->
    eval_storage m >>= fun () ->
    load_byte a
  and eval_store m a x =
    eval_exp a >>= fun a ->
    eval_exp x >>= fun x ->
    switch_memory m >>= fun () ->
    eval_storage m >>= fun () ->
    store_byte a x >>| fun () -> a
  and eval_binop op x y =
    eval_exp x >>= fun x ->
    eval_exp y >>= fun y ->
    binop op x y
  and eval_unop op x =
    eval_exp x >>= fun x ->
    unop op x
  and eval_var v =
    Machine.Local.get state >>= fun {lets} ->
    if Map.mem lets.bound v
    then Machine.return (get_lexical lets.stack v)
    else get v
  and eval_int = const
  and eval_cast t s x =
    eval_exp x >>= fun x ->
    cast t s x
  and eval_unknown _x t = undefined t
  and eval_extract hi lo x =
    eval_exp x >>= fun x ->
    extract ~hi ~lo x
  and eval_concat x y =
    eval_exp x >>= fun x ->
    eval_exp y >>= fun y ->
    concat x y
  and eval_storage = function
    | Var _ -> Machine.return ()
    | mem -> Machine.void (eval_exp mem)

  let eval_exp x = eval_exp (Exp.simpl (Exp.normalize x))

  let exp = eval_exp

  let mem =
    Machine.get () >>| fun proj ->
    let (module Target) = target_of_arch (Project.arch proj) in
    Target.CPU.mem

  let succ x =
    Value.one (Value.bitwidth x) >>= fun one ->
    binop PLUS x one

  let rec do_load a e s =
    load_byte a >>= fun v ->
    if s = 8 then Machine.return v
    else succ a >>= fun a ->
      do_load a e (s - 8) >>= fun u -> match e with
      | LittleEndian -> concat u v
      | BigEndian -> concat v u

  let load a e s = do_load a e (Size.in_bits s)

  let rec do_store a x s hd tl =
    cast hd 8 x >>= fun b ->
    store_byte a b >>= fun () ->
    if s = 8 then Machine.return ()
    else succ a >>= fun a ->
      cast tl (s - 8) x >>= fun x ->
      do_store a x (s - 8) hd tl

  let store a x e s =
    let open Bil.Types in
    let s = Size.in_bits s in
    let x =
      if Value.bitwidth x <> s
      then extract ~hi:(s-1) ~lo:0 x
      else Machine.return x in
    x >>= fun x ->
    match e with
    | LittleEndian -> do_store a x s LOW HIGH
    | BigEndian    -> do_store a x s HIGH LOW

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


  let branch cnd yes no =
    Value.zero (Value.bitwidth cnd) >>= fun zero ->
    binop Bil.EQ cnd zero >>= fun is_zero ->
    !!on_cond is_zero >>= fun () ->
    if Value.is_one is_zero then no else yes

  let rec repeat cnd body =
    cnd >>= fun x ->
    Value.zero (Value.bitwidth x) >>= fun zero ->
    binop Bil.EQ x zero >>= fun stop ->
    !!on_cond stop >>= fun () ->
    if Value.is_one stop
    then Machine.return stop
    else body >>= fun _ -> repeat cnd body

  let term return cls f x t =
    Machine.Local.get state >>= fun s ->
    match Pos.next s.curr cls t with
    | Error err -> Machine.raise err
    | Ok curr ->
      update_pc t >>= fun () ->
      Machine.Local.update state (fun s -> {s with curr}) >>= fun () ->
      enter cls curr t >>= fun () ->
      f x t >>= fun r ->
      leave cls curr t >>= fun () ->
      return r

  let normal = Machine.return

  let halt =
    !!will_halt () >>= fun () -> Machine.raise Halt

  let (:=) v x  = eval_exp x >>= set v
  let def () t  = Def.lhs t := Def.rhs t
  let def = term normal def_t def ()

  let will_jump_to_tid cond dst =
    Code.resolve_addr (`tid dst) >>= function
    | None -> Machine.return ()
    | Some addr ->
      Value.of_word addr >>= fun addr ->
      tick >>= fun () ->
      post will_jump ~f:(fun k -> k (cond,addr))

  let exec_to_prompt dst =
    Machine.Local.get state >>= function
    | {prompts = p :: _} when Tid.equal p dst ->
      Machine.return ()
    | _ -> Code.exec (`tid dst)

  let label cond : label -> _ = function
    | Direct dst ->
      will_jump_to_tid cond dst >>= fun () ->
      exec_to_prompt dst
    | Indirect x ->
      eval_exp x >>= fun ({value} as dst) ->
      tick >>= fun () ->
      post will_jump ~f:(fun k -> k (cond,dst)) >>= fun () ->
      Code.resolve_tid (`addr value) >>= function
      | None -> Code.exec (`addr value)
      | Some dst -> exec_to_prompt dst


  let resolve_return call = match Call.return call with
    | None -> Machine.return None
    | Some (Direct dst) -> Machine.return (Some dst)
    | Some (Indirect dst) ->
      eval_exp dst >>= fun {value=dst} ->
      Code.resolve_tid (`addr dst)

  let push_prompt dst = match dst with
    | None -> Machine.return ()
    | Some dst ->
      Machine.Local.update state ~f:(fun s ->
          {s with prompts = dst :: s.prompts})

  let pop_prompt =
    Machine.Local.update state ~f:(function
        | {prompts=[]} as s -> s
        | {prompts=_::prompts} as s -> {s with prompts})

  let trap_cfi_violation callsite =
    !!cfi_will_diverge callsite >>= fun () ->
    call_when_provided cfi_violation_handler >>= function
    | true -> Machine.return ()
    | false -> Machine.raise (Cfi_violation callsite)

  let call cond call =
    Machine.Local.get state >>= fun {addr=callsite} ->
    resolve_return call >>= fun ret ->
    push_prompt ret >>= fun () ->
    label cond (Call.target call) >>= fun () ->
    Machine.Local.get state >>= function
    | {prompts=[]} -> Machine.return ()
    | {prompts=p::_} -> match ret with
      | None -> Machine.return ()
      | Some p' when Tid.(p <> p') ->
        trap_cfi_violation callsite >>= fun () ->
        pop_prompt >>= fun () ->
        label cond (Direct p')
      | Some p ->
        pop_prompt >>= fun () ->
        label cond (Direct p)

  let goto cond c = label cond c
  let interrupt n  = !!will_interrupt n

  let jump cond t = match Jmp.kind t with
    | Ret dst -> label cond dst
    | Call c -> call cond c
    | Goto l -> goto cond l
    | Int (n,r) ->
      interrupt n >>= fun () ->
      Code.exec (`tid r)

  type constr =
    | Init
    | Conj of Value.t
    | Disj of Value.t * Jmp.t

  let jmp constr t =
    eval_exp (Jmp.cond t) >>= fun v ->
    match constr, Word.is_one v.value with
    | Init,true ->
      !!on_cond v >>= fun () ->
      Machine.return (Disj (v,t))
    | Init,false ->
      !!on_cond v >>= fun () ->
      unop Bil.NOT v >>| fun u ->
      Conj u
    | Conj u,false ->
      binop Bil.AND u v >>= !!on_cond >>= fun () ->
      unop Bil.NOT v >>= fun v ->
      binop Bil.AND u v >>| fun w ->
      Conj w
    | Conj u,true ->
      binop Bil.AND u v >>= fun w ->
      !!on_cond w >>| fun () ->
      Disj (w,t)
    | Disj (u,t),_ ->
      unop Bil.NOT u >>= fun p ->
      binop Bil.AND p v >>= !!on_cond >>= fun () ->
      binop Bil.OR u v >>| fun w ->
      Disj (w,t)

  let jmp = term normal jmp_t jmp

  let blk () t =
    Machine.Seq.iter (Term.enum def_t t) ~f:def >>= fun () ->
    Machine.Seq.fold ~init:Init (Term.enum jmp_t t) ~f:jmp

  let finish = function
    | Init | Conj _ -> Machine.return ()  (* return from sub *)
    | Disj (cond,code) -> jump cond code

  let blk : blk term -> unit m = term finish blk_t blk ()

  let arg_def () t = match Arg.intent t with
    | None | Some (In|Both) -> Arg.lhs t := Arg.rhs t
    | _ -> Machine.return ()

  let arg_def = term normal arg_t arg_def ()

  let arg_use () t = match Arg.intent t with
    | Some Out -> Arg.lhs t := Arg.rhs t
    | _ -> Machine.return ()

  let arg_use = term normal arg_t arg_use ()

  let is_out_intent x = match Arg.intent x with
    | Some Out -> true
    | _ -> false

  let get_arg t = exp (Arg.rhs t)
  let get_args ~input sub =
    Term.enum arg_t sub |>
    Seq.filter ~f:(fun x -> match input with
        | true -> not @@ is_out_intent x
        | false -> is_out_intent x) |>
    Machine.Seq.map ~f:get_arg

  let iter_args t f = Machine.Seq.iter (Term.enum arg_t t) ~f

  let sub () t = match Term.first blk_t t with
    | None -> Machine.return ()
    | Some entry ->
      let name = Sub.name t in
      iter_args t arg_def >>= fun () ->
      get_args ~input:true t >>| Seq.to_list_rev >>= fun inputs ->
      post Linker.Trace.call_entered ~f:(fun k ->
          k (name,List.rev inputs)) >>= fun () ->
      blk entry >>= fun () ->
      iter_args t arg_use >>= fun () ->
      post Linker.Trace.call_returned ~f:(fun k ->
          get_args ~input:false t >>| Seq.to_list >>= fun rets ->
          let args = List.rev_append inputs rets in
          k (name,args))

  let assume x = !!on_cond x

  let sub = term normal sub_t sub ()
  let pos = Machine.Local.get state >>| fun {curr} -> curr
  let pc = Machine.Local.get state >>| fun {addr} -> addr
end

module LinkBinaryProgram(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Linker.Make(Machine)

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


  let init () =
    Machine.get () >>= fun proj ->
    linker#run (Project.program proj) (Machine.return ())
end
