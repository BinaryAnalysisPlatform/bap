open Core_kernel.Std
open Bap.Std
open Monads.Std


class context ?main p = object(self : 's)
  inherit Expi.context
  val next : tid option = None
  val curr = Term.tid p
  method main = match main with
    | Some main -> Some main
    | None -> Term.first sub_t p
  method program : program term = p
  method enter_term : 't 'p . ('p,'t) cls -> 't term -> 's =
    fun _ t -> {< curr = Term.tid t >}
  method leave_term : 't 'p . ('p,'t) cls -> 't term -> 's = fun _ _ -> self
  method set_next t = {< next = t >}
  method next = next
  method curr = curr
end


module type S = sig

  type ('a,'e) state
  type 'a u = (unit,'a) state
  type 'a r = (Bil.result,'a) state

  module Expi : Expi.S with type ('a,'e) state = ('a,'e) state

  class ['a] t : object
    constraint 'a = #context
    inherit ['a] Expi.t

    method enter_term : 't 'p . ('p,'t) cls -> 't term -> 'a u

    method eval : 't 'p. ('p,'t) cls -> 't term -> 'a u

    method leave_term : 't 'p . ('p,'t) cls -> 't term -> 'a u

    method eval_sub : sub term -> 'a u
    method eval_blk : blk term -> 'a u
    method eval_arg : arg term -> 'a u
    method eval_def : def term -> 'a u
    method eval_phi : phi term -> 'a u
    method eval_jmp : jmp term -> 'a u

    method eval_goto : label -> 'a u
    method eval_call : call -> 'a u
    method eval_ret  : label -> 'a u
    method eval_exn  : int -> tid -> 'a u

    method eval_direct : tid -> 'a u
    method eval_indirect : exp -> 'a u
  end
end

module Make (SM : Monad.State.S2) = struct
  open SM.Syntax

  type ('a,'e) state = ('a,'e) SM.t
  type 'a u = (unit,'a) state
  type 'a r = (Bil.Result.t,'a) state

  module Expi = Expi.Make(SM)

  type 'a s =
    | Empty
    | First of 'a term
    | Ready of ('a term * 'a term)

  let bad_phi phi =
    Phi.select_or_unknown phi (Tid.create ())

  let eval_args scope sub f : 'a u =
    Term.enum arg_t sub |> Seq.fold ~init:(SM.return ())
      ~f:(fun m a -> m >>= fun () ->
           match scope, Arg.intent a with
           | _, None
           | `enter, (Some In  |Some Both)
           | `leave, (Some Out |Some Both) -> f a
           | _ -> SM.return ())

  let expect_bool w =
    let w = Word.bitwidth w in
    Type_error.bad_type ~exp:bool_t ~got:(Type.Imm w)

  let update_context f =
    SM.get () >>= fun c -> SM.put (f c)

  let set_next term : #context u =
    update_context (fun c -> match term with
        | None -> c#set_next None
        | Some term -> c#set_next (Some (Term.tid term)))

  (** [eval_terms f cls c c' blk] eval terms of block [blk] by applying
      function [f] to each term of class [cls].

      The following invariants are preserved:

      1. if term [t] is not next, then [f] is not applied and [t] is skipped
      2. if [f] is applied to [t], then [next] is set to the successor
         of [t], just before the application, where
         successor of t is
         - next element in sequence if [t] is not the last one
         - [t'] if [t] is last and [c] is not [None], and [t'] is the
           first element of non empty sequence of terms of class [c]
         - [t'] if [t] is last and [c] is not [None] and sequence of terms
           of class [c] is empty and [t'] is first element of non empty
           sequence of term of class [c']
         - None otherwise
  *)
  let eval_terms eval c1 c2 c3 blk : #context u =
    let set_next next = match next with
      | Some thing as next -> set_next next
      | None -> match c2 with
        | None -> SM.return ()
        | Some c2 -> match Seq.hd (Term.enum c2 blk) with
          | Some t -> set_next (Some t)
          | None -> match c3 with
            | None -> SM.return ()
            | Some c3 -> set_next (Seq.hd (Term.enum c3 blk)) in
    let step t1 next =
      SM.get () >>= fun c -> match c#next with
      | None -> SM.return ()
      | Some tid when Tid.(Term.tid t1 <> tid) -> SM.return ()
      | _ -> set_next next >>= fun () -> eval c1 t1 in
    let terms = Term.enum c1 blk in
    set_next (Seq.hd terms) >>= fun () ->
    Seq.fold terms ~init:(SM.return Empty)
      ~f:(fun m t -> m >>= function
        | Empty -> SM.return (First t)
        | First t1 -> set_next (Some t1) >>| fun () -> Ready (t1,t)
        | Ready (t1,t2) ->
          step t1 (Some t2) >>= fun () ->
          SM.return (Ready (t2,t))) >>= function
    | Empty -> SM.return ()
    | First t -> step t None
    | Ready (t1,t2) ->
      step t1 (Some t2) >>= fun () ->
      step t2 None

  class ['a] t = object(self)
    constraint 'a = #context
    inherit ['a] Expi.t

    method private do_enter_term : 't 'p. ('p,'t) cls -> 't term -> 'a u = fun cls t ->
      update_context (fun (c : 'a) -> c#enter_term cls t) >>= fun () ->
      self#enter_term cls t

    method private do_leave_term : 't 'p. ('p,'t) cls -> 't term -> 'a u = fun cls t ->
      update_context (fun (c : 'a) -> c#leave_term cls t) >>= fun () ->
      self#leave_term cls t

    method enter_term : 't 'p. ('p,'t) cls -> 't term -> 'a u = fun _ _ ->
      SM.return ()

    method leave_term : 't 'p. ('p,'t) cls -> 't term -> 'a u = fun _ _ ->
      SM.return ()

    method eval : 't 'p. ('p,'t) cls -> 't term -> 'a u = fun cls t ->
      self#do_enter_term cls t >>= fun () ->
      Term.switch cls t
        ~program:(fun p -> SM.get () >>= fun ctxt ->
                   match ctxt#main with
                   | None -> SM.return ()
                   | Some main -> self#eval sub_t main)
        ~sub:self#eval_sub
        ~arg:self#eval_arg
        ~blk:self#eval_blk
        ~phi:self#eval_phi
        ~def:self#eval_def
        ~jmp:self#eval_jmp >>= fun () ->
      self#leave_term cls t

    method eval_sub sub : 'a u =
      set_next (Term.first blk_t sub) >>= fun () ->
      eval_args `enter sub (self#eval arg_t) >>= fun () ->
      self#eval_fun sub >>= fun () ->
      eval_args `leave sub (self#eval arg_t)

    method private eval_fun sub : 'a u =
      SM.get () >>= fun c -> match c#next with
      | None -> SM.return ()
      | Some p -> match Term.find blk_t sub p with
        | Some blk ->
          self#eval blk_t blk >>= fun () ->
          self#eval_fun sub
        | None -> match Term.find sub_t c#program p with
          | None -> SM.return ()
          | Some calee ->
            self#eval sub_t calee >>= fun () ->
            self#eval_fun sub

    method eval_blk (blk : blk term) : 'a u =
      set_next None >>= fun () ->
      eval_terms self#eval phi_t (Some def_t) (Some jmp_t) blk >>= fun () ->
      eval_terms self#eval def_t (Some jmp_t)  None        blk >>= fun () ->
      eval_terms self#eval jmp_t None  None                blk

    method eval_arg arg : 'a u =
      self#eval_exp (Arg.rhs arg) >>=
      self#update (Arg.lhs arg)

    method eval_def def : 'a u =
      self#eval_exp (Def.rhs def) >>=
      self#update (Def.lhs def)

    method eval_phi phi : 'a u =
      self#do_enter_term phi_t phi >>= fun () ->
      SM.get () >>= fun c ->
      self#eval_exp (bad_phi phi) >>= fun v ->
      self#update (Phi.lhs phi) v >>= fun () ->
      self#leave_term phi_t phi

    method eval_goto dst  : 'a u = self#eval_label dst

    method eval_call call : 'a u = self#eval_label (Call.target call)

    method eval_ret  dst  : 'a u = self#eval_label dst

    method eval_exn num ret : 'a u = set_next None

    method eval_direct tid : 'a u =
      update_context (fun c -> c#set_next (Some tid))

    method eval_indirect exp : 'a u = set_next None

    method private eval_label = function
      | Direct tid -> self#eval_direct tid
      | Indirect exp -> self#eval_indirect exp

    method eval_jmp jmp =
      let leave () = self#leave_term jmp_t jmp in
      let stop _ = set_next None >>= leave in
      let finish s = s >>= leave in
      let expect_bool r = self#type_error (expect_bool r) in
      self#do_enter_term jmp_t jmp >>= fun () ->
      self#eval_exp (Jmp.cond jmp) >>| Bil.Result.value >>= function
      | Bil.Bot -> stop ()
      | Bil.Mem _ -> self#type_error Type_error.bad_imm >>= stop
      | Bil.Imm r when Word.(r = b0) -> SM.return ()
      | Bil.Imm r when Word.(r <> b1) -> expect_bool r >>= stop
      | Bil.Imm r -> finish @@ match Jmp.kind jmp with
        | Goto dst -> self#eval_goto dst
        | Call dst -> self#eval_call dst
        | Ret  dst -> self#eval_ret  dst
        | Int (num,ret) -> self#eval_exn num ret
  end
end

include Make(Monad.State)
