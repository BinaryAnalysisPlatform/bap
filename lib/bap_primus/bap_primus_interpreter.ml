open Core_kernel.Std
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


open Bap_primus_sexp

let switch_memory,memory_switched =
  Observation.provide ~inspect:(sexp_of_option sexp_of_var) "switch-memory"

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

let division_by_zero,will_divide_by_zero =
  Observation.provide ~inspect:sexp_of_unit "division-by-zero"

let segfault, will_segfault =
  Observation.provide ~inspect:sexp_of_word "segfault"

let pagefault,page_fail =
  Observation.provide ~inspect:sexp_of_word "pagefault"

let interrupt,will_interrupt =
  Observation.provide ~inspect:sexp_of_int "interrupt"

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

let jumping,will_jump =
  Observation.provide ~inspect:sexp_of_values "jumping"

let eval_cond,on_cond =
  Observation.provide ~inspect:sexp_of_value "eval-cond"


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

let ite, on_ite =
  Observation.provide ~inspect:sexp_of_ite "ite"

let sexp_of_name = function
  | `symbol name -> Sexp.Atom name
  | `tid tid -> Sexp.Atom (Tid.name tid)
  | `addr addr -> Sexp.Atom (Addr.string_of_value addr)

type scope = {
  stack : (var * value) list;
  bound : int Var.Map.t;
}

type state = {
  addr : addr;
  curr : pos;
  lets : scope; (* lexical context *)
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
         addr = null proj;
         curr = Top {me=prog; up=Nil};
         lets = empty_scope;
       })

type exn += Halt
type exn += Division_by_zero
type exn += Segmentation_fault of addr
type exn += Runtime_error of string

let () =
  Exn.add_printer (function
      | Runtime_error msg ->
        Some (sprintf "Bap_primus runtime error: %s" msg)
      | Halt -> Some "Halt"
      | Segmentation_fault x ->
        Some (asprintf "Segmentation fault at %a" Addr.pp_hex x)
      | Division_by_zero -> Some "Division by zero"
      | _ -> None)


let division_by_zero_handler = "__primus_division_by_zero_handler"
let pagefault_handler =  "__primus_pagefault_handler"


module Make (Machine : Machine) = struct
  open Machine.Syntax

  module Eval = Eval.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Code = Linker.Make(Machine)
  module Value = Primus.Value.Make(Machine)

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



  let call_when_provided name =
    Code.is_linked (`symbol name) >>= fun provided ->
    if provided then Code.exec (`symbol name) >>| fun () -> true
    else Machine.return false

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
      value (Bil.Apply.binop op x.value y.value) >>= fun r ->
      !!on_binop ((op,x,y),r) >>| fun () -> r

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
    !!on_loaded (a,r) >>| fun () -> r

  let store_byte a x =
    !!on_storing a >>= fun () ->
    trapped_memory_access (Memory.set a.value x) >>= fun () ->
    !!on_stored (a,x)

  let ite cond yes no =
    value (if Word.is_one cond.value then yes.value else no.value) >>= fun r ->
    !!on_ite ((cond, yes, no), r) >>| fun () -> r


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
    | Type.Imm _ as t -> failwithf "type error - load from %s:%a"
                           name Type.pps t ()
    | Type.Mem (ks,vs) ->
      let ks = Size.in_bits ks and vs = Size.in_bits vs in
      Primus.Memory.Descriptor.create ks vs name

  let rec memory_of_storage : exp -> _ = function
    | Var v -> memory (Var.typ v) (Var.name v)
    | Unknown (_,typ) -> memory typ "unknown"
    | Store (s,_,_,_,_)
    | Ite (_,s,_)
    | Let (_,_,s) -> memory_of_storage s
    | x -> failwithf "expression `%a' is no a storage" Exp.pps x ()


  let switch_memory m = Memory.switch (memory_of_storage m)


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


  let term ?(cleanup=Machine.return ()) return cls f t =
    Machine.Local.get state >>= fun s ->
    match Pos.next s.curr cls t with
    | Error err -> Machine.raise err
    | Ok curr ->
      update_pc t >>= fun () ->
      Machine.Local.update state (fun s -> {s with curr}) >>= fun () ->
      enter cls curr t >>= fun () ->
      Machine.catch (f t)
        (fun exn ->
           leave cls curr t >>= fun () ->
           cleanup >>= fun () ->
           Machine.raise exn) >>= fun r ->
      leave cls curr t >>= fun () ->
      cleanup >>= fun () ->
      return r

  let normal = Machine.return

  let halt =
    !!will_halt () >>= fun () -> Machine.raise Halt

  let (:=) v x  = eval_exp x >>= set v
  let def t = Def.lhs t := Def.rhs t
  let def = term normal def_t def

  let will_jump_to_tid cond dst =
    Code.resolve_addr (`tid dst) >>= function
    | None -> Machine.return ()
    | Some addr ->
      Value.of_word addr >>= fun addr ->
      !!will_jump (cond,addr)

  let label cond : label -> _ = function
    | Direct t ->
      will_jump_to_tid cond t >>= fun () ->
      Code.exec (`tid t)
    | Indirect x ->
      eval_exp x >>= fun ({value} as dst) ->
      !!will_jump (cond,dst) >>= fun () ->
      Code.exec (`addr value)

  let call cond c =
    label cond (Call.target c) >>= fun () ->
    match Call.return c with
    | Some t -> label cond t
    | None -> failf "a non-return call returned" ()

  let goto cond c = label cond c
  let interrupt n  = !!will_interrupt n

  let jump cond t = match Jmp.kind t with
    | Ret _ -> Machine.return () (* return from sub *)
    | Call c -> call cond c
    | Goto l -> goto cond l
    | Int (n,r) ->
      interrupt n >>= fun () ->
      Code.exec (`tid r)

  let jmp t = eval_exp (Jmp.cond t) >>= fun ({value} as cond) ->
    !!on_cond cond >>| fun () ->
    Option.some_if (Word.is_one value) (cond,t)
  let jmp = term normal jmp_t jmp

  let blk t =
    Machine.Seq.iter (Term.enum def_t t) ~f:def >>= fun () ->
    Machine.Seq.find_map (Term.enum jmp_t t) ~f:jmp

  let finish = function
    | None -> Machine.return ()  (* return from sub *)
    | Some (cond,code) -> jump cond code

  let blk = term finish blk_t blk

  let arg_def t = match Arg.intent t with
    | None | Some (In|Both) -> Arg.lhs t := Arg.rhs t
    | _ -> Machine.return ()

  let arg_def = term normal arg_t arg_def

  let arg_use t = match Arg.intent t with
    | None | Some (Out|Both) -> Arg.lhs t := Arg.rhs t
    | _ -> Machine.return ()

  let arg_use = term normal arg_t arg_use

  let get_arg t = Env.get (Arg.lhs t)
  let get_args ~input sub =
    Term.enum arg_t sub |>
    Seq.filter ~f:(fun x -> not input || Arg.intent x <> Some Out) |>
    Machine.Seq.map ~f:get_arg

  let iter_args t f = Machine.Seq.iter (Term.enum arg_t t) ~f

  let sub t = match Term.first blk_t t with
    | None -> Machine.return ()
    | Some entry ->
      let name = Sub.name t in
      iter_args t arg_def >>= fun () ->
      get_args ~input:true t >>| Seq.to_list >>= fun args ->
      !!Linker.Trace.call_entered (name,args) >>= fun () ->
      blk entry >>= fun () ->
      iter_args t arg_use >>= fun () ->
      get_args ~input:false t >>| Seq.to_list >>= fun args ->
      !!Linker.Trace.call_returned (name,args)


  let sub = term normal sub_t sub
  let pos = Machine.Local.get state >>| fun {curr} -> curr
  let pc = Machine.Local.get state >>| fun {addr} -> addr
end

module Init(Machine : Machine) = struct
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


  let run () =
    Machine.get () >>= fun proj ->
    linker#run (Project.program proj) (Machine.return ())
end
