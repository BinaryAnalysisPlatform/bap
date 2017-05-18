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


let variable_access,variable_will_be_looked_up =
  Observation.provide ~inspect:(fun v ->
      Sexp.Atom (Var.name v)) "variable-access"

let variable_read,variable_was_read =
  Observation.provide ~inspect:sexp_of_binding "variable-read"

let variable_written,variable_was_written =
  Observation.provide ~inspect:sexp_of_binding "variable-written"

let address_access,address_will_be_read =
  Observation.provide ~inspect:sexp_of_word "address-access"

let address_read,address_was_read =
  Observation.provide ~inspect:sexp_of_move "address-read"

let address_written,address_was_written =
  Observation.provide ~inspect:sexp_of_move
    "address-written"

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
  next : tid option;
  curr : Context.level;
  blks : tid Addr.Map.t;
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
         next = None;
         curr = Top {me=prog; up=Nil};
         blks = collect_blks prog
       })

module Make (Machine : Machine) = struct
  open Machine.Syntax

  module Eval = Eval.Make(Machine)
  module Memory = Bap_primus_memory.Make(Machine)
  module Linker = Bap_primus_linker.Make(Machine)
  module Env = Bap_primus_env.Make(Machine)

  type 'a r = 'a Machine.t

  let make_observation = Machine.Observation.make

  type error += Runtime_error of string

  let () =
    Bap_primus_error.add_printer (function
        | Runtime_error msg ->
          Some (sprintf "Bap_primus runtime error: %s" msg)
        | _ -> None)

  let observe_term cls t = Term.switch cls t
      ~program:(make_observation top_entered)
      ~sub:(make_observation sub_entered)


  let failf fmt = Format.ksprintf (fun msg ->
      fun () -> Machine.fail (Runtime_error msg)) fmt

  class base_sema = object
    inherit [word,word] Eval.t
    method value_of_word = Machine.return
    method word_of_value x = Machine.return (Some x)
    method undefined = failf "undefined value" ()
    method storage_of_value x = Machine.return (Some x)

    method lookup var =
      make_observation variable_will_be_looked_up var >>= fun () ->
      Env.get var >>= fun r ->
      make_observation variable_was_read (var,r) >>= fun () ->
      Machine.return r

    method update var r =
      Env.set var r >>= fun () ->
      make_observation variable_was_written (var,r)

    method load base addr =
      let addr = Addr.(base + addr) in
      make_observation address_will_be_read addr >>= fun () ->
      Memory.load addr >>= fun r ->
      make_observation address_was_read (addr,r) >>= fun () ->
      Machine.return r

    method store base addr data =
      let addr = Addr.(base + addr) in
      Memory.save addr data >>= fun () ->
      make_observation address_was_written (addr,data) >>= fun () ->
      Machine.return base
  end

  let resolve_addr addr = failf "not implemented" ()

  let sema = new base_sema

  let skip = Machine.return ()
  let halt = Machine.return ()
  let exp = sema#eval_exp

  let set_next next = Machine.Local.update state (fun s ->
    {s with next = next})
  let def_next x = set_next (Some x)
  let undef_next = set_next None

  let label l = function
    | Direct tid -> Machine.Local.update state (fun s ->
      {s with next = Some tid})
    | Indirect e -> exp e >>= resolve_addr

  let call c = failf "not implemented" ()
  let goto c = failf "not implemented" ()
  let ret l = failf "not implemented" ()
  let interrupt n r = failf "not implemented" ()


  let phi t = failf "not implemented" ()
  let def t = exp (Def.rhs t) >>= sema#update (Def.lhs t)

  let jmp t =
    exp (Jmp.cond t) >>= fun c ->
    if Word.is_zero c then skip
    else match Jmp.kind t with
      | Call c -> call c
      | Goto l -> goto l
      | Ret l -> ret l
      | Int (n,r) -> interrupt n r

  let fst cls t = Option.map ~f:Term.tid (Term.first cls t)
  let goto_first_of terms =
    List.find_map terms ~f:ident |> function
    | None -> skip
    | Some tid -> def_next tid

  let blk t =
    let fst cls = fst cls t in
    goto_first_of [fst phi_t; fst def_t; fst jmp_t]

  let arg t =
    exp (Arg.rhs t) >>= sema#update (Arg.lhs t)

  let sub t =
    let fst cls = fst cls t in
    goto_first_of [fst arg_t; fst blk_t]

  let prog t = goto_first_of [fst sub_t t]


  let next_term cls p t =
    Term.next cls p (Term.tid t)

  let next_tid cls p t =
    next_term cls p t |> Option.map ~f:Term.tid


  open Context.Level

  let no_transition = failf "no transition" ()


  module Trans = struct
    let (-->) cls step p tid  = match Term.find cls p tid with
      | None -> None
      | Some t -> Some (step t)

    let fall = [
      jmp_t --> jmp;
    ]

    let goto = [
      blk_t --> blk;
    ]

    let call = [
      sub_t --> sub;
    ]

    let top = [
      sub_t --> sub;
    ]

    let sub = [
      arg_t --> arg;
      blk_t --> blk;
    ]

    let arg = [
      arg_t --> arg;
      blk_t --> blk;
    ]

    let blk = [
      phi_t --> phi;
      def_t --> def;
      jmp_t --> jmp;
    ]

    let phi = [
      phi_t --> phi;
      def_t --> def;
      jmp_t --> jmp;
    ]

    let def = [
      def_t --> def;
      jmp_t --> jmp;
    ]


    let get trans p t =
      List.find_map trans ~f:(fun f -> f p t)

    let run trans p t : unit Machine.t =
      get trans p t |> function
      | None -> no_transition
      | Some x -> x

    let apply t x =
      List.find_map t ~f:(fun f -> f x) |> function
      | None -> no_transition
      | Some x -> x

  end



  let rec eval m =
    m >>= fun () -> Machine.Local.get state >>= function
    | {next=None} -> halt
    | {next=Some tid} -> eval @@ compute_tid tid
  and compute_tid tid =
    let open Trans in
    Machine.Local.get state >>= fun s ->
    match s.curr with
    | Top {me} -> run top me tid
    | Sub {me} -> run sub me tid
    | Arg {up={me=sub}} -> run arg sub tid
    | Blk {me} -> run blk me tid
    | Phi {up={me=blk}} -> run phi blk tid
    | Def {up={me=blk}} -> run def blk tid
    | Jmp {up={me=blk; up={me=sub; up={me=top}}}} -> apply [
        get fall blk;
        get goto sub;
        get call top;
      ] tid

  let eval f t = eval (f t)

  let run cls t = Term.switch cls t
      ~program:(eval prog)
      ~sub:(eval sub)
      ~arg:(eval arg)
      ~blk:(eval blk)
      ~phi:(eval phi)
      ~def:(eval def)
      ~jmp:(eval jmp)

  class type s = Semantics(Machine).t

  let sema : s = object
    inherit base_sema
    method run : 'p 't. ('p,'t) cls -> 't term -> unit Machine.t =
      fun cls t -> run cls t
  end


  (* let next_of_level level = *)
  (*   let open Context.Level in match level with *)
  (*   | Top _ -> None *)
  (*   | Sub _ -> None *)
  (*   | Arg {me; up={me=sub}} -> next_tid arg_t sub me *)
  (*   | Blk _ *)
  (*   | Phi _ *)
  (*   | Def _ *)
  (*   | Jmp _ -> assert false *)


  (* let set_next_term next = *)
  (*   Machine.update (fun ctxt -> *)
  (*       ctxt#set_next (Option.map ~f:Term.tid next)) *)

  (* class ['e] t  = object(self) *)
  (*   inherit ['e,word,word] Eval.t *)

  (*   method value_of_word = Machine.return *)
  (*   method word_of_value x = Machine.return (Some x) *)

  (*   method lookup var = *)
  (*     make_observation variable_will_be_looked_up var >>= fun () -> *)
  (*     Env.get var >>= fun r -> *)
  (*     make_observation variable_was_read (var,r) >>= fun () -> *)
  (*     Machine.return r *)

  (*   method update var r = *)
  (*     Env.set var r >>= fun () -> *)
  (*     make_observation variable_was_written (var,r) *)

  (*   method load base addr = *)
  (*     let addr = Addr.(base + addr) in *)
  (*     make_observation address_will_be_read addr >>= fun () -> *)
  (*     Memory.load addr >>= fun r -> *)
  (*     make_observation address_was_read (addr,r) >>= fun () -> *)
  (*     super#eval_int r *)

  (*   method store base addr data = *)
  (*     let addr = Addr.(base + addr) in *)
  (*     super#store mem addr data >>= fun r -> *)
  (*     Memory.save addr data >>= fun () -> *)
  (*     make_observation address_was_written (addr,data) >>= fun () -> *)
  (*     Machine.return r *)

  (*   method enter_term cls t = *)
  (*     super#enter_term cls t >>= fun () -> *)
  (*     Machine.Local.get state >>= fun s -> *)
  (*     make_observation term_entered (Term.tid t) >>= fun () -> *)
  (*     match Context.Level.next s.curr cls t with *)
  (*     | Error err -> Machine.fail err *)
  (*     | Ok next -> *)
  (*       Machine.Local.put state {s with curr=next} >>= fun () -> *)
  (*       make_observation level_left s.curr >>= fun () -> *)
  (*       make_observation level_entered next >>= fun () -> *)
  (*       Term.switch cls t *)
  (*         ~program:(make_observation top_entered) *)
  (*         ~sub:(make_observation sub_entered) *)
  (*         ~arg:(make_observation arg_entered) *)
  (*         ~blk:(make_observation blk_entered) *)
  (*         ~phi:(make_observation phi_entered) *)
  (*         ~def:(make_observation def_entered) *)
  (*         ~jmp:(make_observation jmp_entered) *)



  (*   method eval_sub sub = match Term.first arg_t sub with *)
  (*     | None -> set_next_term (Term.first blk_t sub) *)
  (*     | arg -> set_next_term arg *)


  (*   method leave_term cls t = *)
  (*     super#leave_term cls t >>= fun () -> *)
  (*     make_observation term_left (Term.tid t) >>= fun () -> *)
  (*     Term.switch cls t *)
  (*       ~program:(make_observation top_left) *)
  (*       ~sub:(make_observation sub_left) *)
  (*       ~arg:(make_observation arg_left) *)
  (*       ~blk:(make_observation blk_left) *)
  (*       ~phi:(make_observation phi_left) *)
  (*       ~def:(make_observation def_left) *)
  (*       ~jmp:(make_observation jmp_left) *)





  (*   method empty : Bil.storage = object(self) *)
  (*     method load _ = None *)
  (*     method save _ _ = self *)
  (*   end *)

  (*   method private eval_jmp_target exp = *)
  (*     super#eval_exp exp >>| Bil.Result.value >>= function *)
  (*     | Bil.Bot -> failf "undefined jump destination" () *)
  (*     | Bil.Mem _ -> failf "type error in a jump" () *)
  (*     | Bil.Imm dst -> Machine.return dst *)

  (*   method eval_indirect exp = *)
  (*     let open Context.Level in *)
  (*     self#eval_jmp_target exp >>= fun dst -> *)
  (*     Machine.Local.get state >>= fun {blks} -> *)
  (*     Linker.is_linked (`addr dst) >>= fun is_linked -> *)
  (*     match Map.find blks dst with *)
  (*     | Some tid when not is_linked -> self#eval_direct tid *)
  (*     | _ -> *)
  (*       make_observation calling (`addr dst) >>= fun () -> *)
  (*       Linker.exec (`addr dst) self (\* in case of a tail-call *\) *)

  (*   method private eval_indirect_call exp = *)
  (*     self#eval_jmp_target exp >>= fun dst -> *)
  (*     make_observation calling (`addr dst) >>= fun () -> *)
  (*     Linker.exec (`addr dst) self *)

  (*   method eval_call call = *)
  (*     match Call.target call with *)
  (*     | Indirect exp -> self#eval_indirect_call exp *)
  (*     | Direct tid -> *)
  (*       make_observation calling (`tid tid) >>= fun () -> *)
  (*       Linker.exec (`tid tid) self *)
  (* end *)
end
