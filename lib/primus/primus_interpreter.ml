open Core_kernel.Std
open Bap.Std
open Bap_c.Std

open Primus_types

module Context = Primus_context
module Observation = Primus_observation
module State = Primus_state

open Primus_sexp

let sexp_of_level = Primus_context.sexp_of_level

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
  pc : addr;
}

let sexp_of_state {pc} = Sexp.List [
    Sexp.List [Sexp.Atom "program-counter"; sexp_of_word pc];
]

let sexp_of_state {pc} = [%sexp {pc : word = pc}]

module Make (Machine : Machine) = struct
  open Machine.Syntax

  module Expi = Expi.Make(Machine)
  module Biri = Biri.Make(Machine)
  module Memory = Primus_memory.Make(Machine)
  module Linker = Primus_linker.Make(Machine)
  module Env = Primus_env.Make(Machine)
  module Lisp_machine = Primus_lisp.Machine(Machine)

  type 'a r = (Bil.result,'a) Machine.t
  type 'a u = (unit,'a) Machine.t


  let make_observation = Machine.Observation.make


  let observe_term cls t = Term.switch cls t
      ~program:(make_observation top_entered)
      ~sub:(make_observation sub_entered)

  type error += Runtime_error of string

  let () =
    Primus_error.add_printer (function
        | Runtime_error msg ->
          Some (sprintf "Primus runtime error: %s" msg)
        | _ -> None)

  class ['e] t  =
    object(self)
      inherit ['e] Biri.t as super
      constraint 'e = #Context.t

      method! enter_term cls t =
        super#enter_term cls t >>= fun () ->
        Machine.get () >>= fun ctxt ->
        make_observation term_entered (Term.tid t) >>= fun () ->
        match Context.Level.next ctxt#level cls t with
        | Error err -> Machine.fail err
        | Ok next ->
          Machine.put (ctxt#with_level next) >>= fun () ->
          make_observation level_left ctxt#level >>= fun () ->
          make_observation level_entered next >>= fun () ->
          Term.switch cls t
            ~program:(make_observation top_entered)
            ~sub:(make_observation sub_entered)
            ~arg:(make_observation arg_entered)
            ~blk:(make_observation blk_entered)
            ~phi:(make_observation phi_entered)
            ~def:(make_observation def_entered)
            ~jmp:(make_observation jmp_entered)


      method! leave_term cls t =
        super#leave_term cls t >>= fun () ->
        make_observation term_left (Term.tid t) >>= fun () ->
        Term.switch cls t
          ~program:(make_observation top_left)
          ~sub:(make_observation sub_left)
          ~arg:(make_observation arg_left)
          ~blk:(make_observation blk_left)
          ~phi:(make_observation phi_left)
          ~def:(make_observation def_left)
          ~jmp:(make_observation jmp_left)

      method! lookup var =
        make_observation variable_will_be_looked_up var >>= fun () ->
        match Var.typ var with
        | Type.Mem _ ->
          Machine.get () >>= fun ctxt ->
          let (ctxt,v) = ctxt#create_storage self#empty in
          Machine.put ctxt >>= fun () -> Machine.return v
        | _ ->
          Env.get var >>= fun r ->
          make_observation variable_was_read (var,r) >>= fun () ->
          Machine.return r

      method! update var r =
        super#update var r >>= fun () ->
        make_observation variable_was_written (var,r)

      method! load _ addr =
        make_observation address_will_be_read addr >>= fun () ->
        Memory.load addr >>= fun r ->
        make_observation address_was_read (addr,r) >>= fun () ->
        super#eval_int r

      method! store mem addr data =
        super#store mem addr data >>= fun r ->
        Memory.save addr data >>= fun () ->
        make_observation address_was_written (addr,data) >>= fun () ->
        Machine.return r

      method! empty : Bil.storage = object(self)
        method load _ = None
        method save _ _ = self
      end

      method! eval_indirect exp =
        super#eval_exp exp >>| Bil.Result.value >>= function
        | Bil.Imm dst -> Linker.exec (`addr dst)
        | _ -> Machine.return ()

      method private eval_lisp_return arg r =
        match Arg.rhs arg with
        | Bil.Var v ->
          self#eval_int r >>= fun r ->
          self#update v r
        | _ -> Machine.return ()

      method private set_next_block call =
        match Call.return  call with
        | None -> Machine.return ()
        | Some dst -> super#eval_ret dst

      method! eval_call call =
        match Call.target call with
        | Indirect exp -> self#eval_indirect exp
        | Direct tid ->
          make_observation calling (`tid tid) >>= fun () ->
          Machine.get () >>= fun ctxt ->
          match Term.find sub_t ctxt#program tid with
          | None -> super#eval_call call
          | Some sub ->
            let name = Sub.name sub in
            Lisp_machine.is_bound name >>= function
            | false ->
              Linker.exec (`symbol name)
            | true -> match Term.get_attr sub C.proto with
              | None ->
                (* todo: issue a warning or failure *)
                Linker.exec (`symbol name)
              | Some proto ->
                let args = proto.C.Type.Proto.args in
                let arity = List.length args in
                Seq.take (Term.enum arg_t sub) arity |>
                Machine.Seq.map ~f:(fun arg ->
                    self#eval_arg arg >>= fun () ->
                    self#lookup (Arg.lhs arg) >>= fun r ->
                    match Bil.Result.value r with
                    | Bil.Imm w -> Machine.return w
                    | v ->
                      Machine.fail (Runtime_error "unbound arg")) >>= fun bs ->
                self#set_next_block call >>= fun () ->
                Lisp_machine.run self name (Seq.to_list bs) >>= fun r ->
                match proto with
                | {C.Type.Proto.return=`Void} ->
                  Machine.return ()
                | _ -> match Term.last arg_t sub with
                  | None ->
                    (* issue a warning? or assertion? *)
                    Machine.return ()
                  | Some arg ->
                    self#eval_lisp_return arg r
    end
end
