open Core_kernel
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Bap_main

(* Algorithm.

   For each system, just when we start, we create the init machine
   that will be used to fork all other machines and re-execute the
   path N times, where N is the number of linearly independent paths
   that are reachable from the entry point. The init machine spawns
   a new worker for each model in its queue. We start with an initial
   empty model that starts the pioneer machine.

   Each worker (including the pioneer) runs from the entry point and
   builds the path formula. With each value we associate a
   formula, which we optimize on the fly. We designate certain values
   as inputs and keep them symbolic. When we hit the branch that we
   didn't see before and that leads to the desitantion which we
   neither saw nor plan to see, we take the formula associated with
   that branch condition and switch back to the init machine, and push
   its negation to the queue.

   When a worker finishes, the control is passed back to the init
   machine that picks the first actual formula and finds a satisfying
   model for it, then spawns a new worker process, which works as above.

   To identify which formula is actual, we associate with each
   formula the branch destination that will be taken if the formula
   is satisfiable. A formula is actual if this destination wasn't
   visited by a worker before. An unresolved destination is treated
   as unvisited, therefore it is always actual.
*)

let cutoff = Extension.Configuration.parameter
    Extension.Type.(int =? 1)
    "cutoff-level"

type memory = {
  bank : Primus.Memory.Descriptor.t;
  ptr : Word.t;
  len : int;
} [@@deriving compare, equal]

let memories = Primus.Machine.State.declare
    ~uuid:"15dbb89a-3bfb-421c-8b19-16605425c3f5"
    ~name:"symbolic-memories" @@ fun _ ->
  String.Map.empty

module Bank = Primus.Memory.Descriptor
module Id = Monad.State.Multi.Id
let debug_msg,post_msg = Primus.Observation.provide "executor-debug"
    ~inspect:sexp_of_string

module Debug(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  let msg fmt = Format.kasprintf (fun msg ->
      Machine.current () >>= fun id ->
      let msg = Format.asprintf "%a: %s" Id.pp id msg in
      Machine.Observation.make post_msg msg) fmt
end


module Input : sig
  type t
  val ptr : Primus.Memory.Descriptor.t -> addr -> t
  val var : var -> t
  val to_symbol : t -> string
  val size : t -> int

  module Make(Machine : Primus.Machine.S) : sig
    val set : t -> Primus.Value.t -> unit Machine.t
  end
  include Base.Comparable.S with type t := t
end = struct
  type t =
    | Var of var
    | Ptr of {
        bank : Primus.Memory.Descriptor.t;
        addr : addr;
      } [@@deriving compare, sexp_of]

  let ptr bank addr = Ptr {bank; addr}
  let var p = Var p

  let to_symbol = function
    | Var v -> Format.asprintf "%a" Var.pp v
    | Ptr {bank;addr} ->
      Format.asprintf "%s[%a]"
        (Primus.Memory.Descriptor.name bank)
        Addr.pp_hex addr

  let size = function
    | Ptr {bank} -> Primus.Memory.Descriptor.data_size bank
    | Var v -> match Var.typ v with
      | Imm m -> m
      | _ -> 1

  module Make(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Mems = Primus.Memory.Make(Machine)
    module Env = Primus.Env.Make(Machine)
    module Lisp = Primus.Lisp.Make(Machine)

    let allocate bank ptr len =
      let width = Bank.data_size bank in
      let seed = Hashtbl.hash (Bank.name bank) in
      let generator = Primus.Generator.Random.lcg ~width seed in
      Mems.allocate ~generator ptr len

    let allocate_if_not bank addr =
      Mems.is_writable addr >>= function
      | true -> Machine.return ()
      | false ->
        let name = Primus.Memory.Descriptor.name bank in
        Machine.Global.get memories >>= fun memories ->
        match Map.find memories name with
        | None -> Lisp.failf "unknown symbolic memory %s" name ()
        | Some {ptr; len} -> allocate bank ptr len

    let set input value = match input with
      | Ptr {bank; addr} ->
        Mems.memory >>= fun current ->
        Mems.switch bank >>= fun () ->
        allocate_if_not bank addr >>= fun () ->
        Mems.set addr value >>= fun () ->
        Mems.switch current
      | Var v ->
        Env.set v value
  end

  include Base.Comparable.Make(struct
      type nonrec t = t [@@deriving compare, sexp_of]
    end)
end

module Formula : sig
  type t
  type formula = t
  type model
  type value

  val word : Word.t -> t
  val var : string -> int -> t

  val binop : binop -> t -> t -> t
  val unop : unop -> t -> t
  val extract : int -> int -> t -> t
  val concat : t -> t -> t
  val ite : t -> t -> t -> t
  val cast : cast -> int -> t -> t

  val solve : ?constraints:t list -> ?refute:bool -> t -> model option


  module Model : sig
    type t = model
    val get : model -> Input.t -> value option
  end

  module Value : sig
    type t = value
    val to_word : value -> word
    val to_formula : value -> formula
    val to_string : value -> string
  end

  val to_string : t -> string
end = struct
  module Bitv = Z3.BitVector
  module Bool = Z3.Boolean
  module Expr = Z3.Expr
  module Sort = Z3.Sort

  type t = Expr.expr
  type formula = t
  type value = t

  let ctxt = Z3.mk_context [
      "model", "true";
    ]

  let to_string = Expr.to_string

  let coerce_shift op ctxt x y =
    let sx = Bitv.get_size (Expr.get_sort x)
    and sy = Bitv.get_size (Expr.get_sort y) in
    if sx = sy then op ctxt x y
    else
      let y = Bitv.mk_zero_ext ctxt (sx-sy) y in
      op ctxt x y


  let z3_of_binop : binop -> _ = function
    | PLUS -> Bitv.mk_add
    | MINUS -> Bitv.mk_sub
    | TIMES -> Bitv.mk_mul
    | DIVIDE -> Bitv.mk_udiv
    | SDIVIDE -> Bitv.mk_sdiv
    | MOD -> Bitv.mk_srem
    | SMOD -> Bitv.mk_urem
    | LSHIFT -> coerce_shift Bitv.mk_shl
    | RSHIFT -> coerce_shift Bitv.mk_lshr
    | ARSHIFT -> coerce_shift Bitv.mk_ashr
    | AND -> Bitv.mk_and
    | OR -> Bitv.mk_or
    | XOR -> Bitv.mk_xor
    | LT -> Bitv.mk_ult
    | LE -> Bitv.mk_ule
    | SLT -> Bitv.mk_slt
    | SLE -> Bitv.mk_sle
    | EQ -> fun ctxt x y ->
      Bitv.mk_not ctxt @@
      Bitv.mk_redor ctxt @@
      Bitv.mk_xor ctxt x y
    | NEQ -> fun ctxt x y ->
      Bitv.mk_redor ctxt @@
      Bitv.mk_xor ctxt x y

  let do_simpl x = Expr.simplify x None

  let bit0 = Expr.mk_numeral_int ctxt 0 (Bitv.mk_sort ctxt 1)
  let bit1 = Expr.mk_numeral_int ctxt 1 (Bitv.mk_sort ctxt 1)

  let is_bool = Bool.is_bool

  let bit_of_bool x =
    if Bool.is_bool x
    then do_simpl @@ Bool.mk_ite ctxt x bit1 bit0
    else x

  let bool_of_bit x =
    do_simpl @@ Bool.mk_eq ctxt x bit1

  let z3_of_unop : unop -> _ = function
    | NEG -> Bitv.mk_neg
    | NOT -> Bitv.mk_not

  let coerce_to_bit x =
    if is_bool x then bit_of_bool x else x

  let coerce_to_bool x =
    if is_bool x then x else bool_of_bit x


  let simpl x = Expr.simplify x None |>
                coerce_to_bit

  let binop op x y = simpl (z3_of_binop op ctxt x y)
  let unop op x = simpl (z3_of_unop op ctxt x)

  let word x =
    let s = Bitv.mk_sort ctxt (Word.bitwidth x) in
    let x = Word.to_bitvec x in
    if Bitvec.fits_int x
    then Expr.mk_numeral_int ctxt (Bitvec.to_int x) s
    else
      let x = Bitvec.to_bigint x in
      Expr.mk_numeral_string ctxt (Z.to_string x) s

  let extract hi lo x =
    let xs = Bitv.get_size (Expr.get_sort x)
    and ns = hi-lo+1 in
    simpl @@
    if ns > xs
    then if lo = 0
      then Bitv.mk_zero_ext ctxt (ns-xs) x
      else
        Bitv.mk_extract ctxt hi lo @@
        Bitv.mk_zero_ext ctxt (ns-xs) x
    else
      Bitv.mk_extract ctxt hi lo x

  let concat x y = simpl @@ Bitv.mk_concat ctxt x y

  let ite c x y = simpl @@ Bool.mk_ite ctxt (coerce_to_bool c) x y

  let cast (c:cast) s x =
    let old = Bitv.get_size (Expr.get_sort x) in
    simpl @@ match c with
    | SIGNED -> if old < s
      then Bitv.mk_sign_ext ctxt (s-old) x
      else extract (s-1) 0 x
    | UNSIGNED -> if old < s
      then Bitv.mk_zero_ext ctxt (s-old) x
      else extract (s-1) 0 x
    | HIGH -> extract (old-1) (old-s) x
    | LOW -> extract (s-1) 0 x

  let var name size =
    Expr.mk_const_s ctxt name (Bitv.mk_sort ctxt size)

  type model = {
    model : Z3.Model.model;
  }

  let solve ?(constraints=[]) ?(refute=false) expr =
    let solver = Z3.Solver.mk_simple_solver ctxt in
    let constraints = List.map constraints ~f:bool_of_bit in
    let lhs = Expr.mk_numeral_int ctxt 0 (Expr.get_sort expr) in
    let formula = Bool.mk_eq ctxt lhs expr in
    let formula = if refute then formula
      else Bool.mk_not ctxt formula in
    match Z3.Solver.check solver (formula::constraints) with
    | UNSATISFIABLE | UNKNOWN -> None
    | SATISFIABLE -> match Z3.Solver.get_model solver with
      | None -> None
      | Some model -> Some {model}

  module Model = struct
    type t = model
    let get {model} input =
      let var = var (Input.to_symbol input) (Input.size input) in
      match Z3.Model.eval model var false with
      | Some x when Expr.is_numeral x ->  Some x
      | _ -> None

  end

  module Value = struct
    type t = value
    let to_formula = ident
    let to_bigint x =
      let s = Expr.to_string x in
      let len = String.length s in
      if String.length s > 2 && Char.(s.[0] = '#')
      then
        let base = match s.[1] with
          | 'b' -> 2
          | 'x' -> 16
          | _ -> failwithf "Not a literal value %s" s () in
        Z.of_substring_base base s ~pos:2 ~len:(len-2)
      else Z.of_string s

    let to_word x =
      let s = Bitv.get_size (Expr.get_sort x)
      and z = to_bigint x in
      Word.create Bitvec.(bigint z mod modulus s) s

    let to_string x = Expr.to_string x
  end
end

type executor = {
  inputs : Set.M(Input).t;
  constraints : Formula.t list;
  formulae : Formula.t Primus.Value.Id.Map.t;
}

let executor = Primus.Machine.State.declare
    ~uuid:"e21aa0fe-bc37-48e3-b398-ce7e764843c8"
    ~name:"symbolic-executor-formulae" @@ fun _ -> {
    formulae = Primus.Value.Id.Map.empty;
    inputs = Set.empty (module Input);
    constraints = [];
  }

let sexp_of_formula x = Sexp.Atom (Formula.to_string x)

let new_formula,on_formula = Primus.Observation.provide "new-formula"
    ~package:"bap"
    ~inspect:(fun (id,x) -> Sexp.List [
        Primus.Value.sexp_of_t id;
        sexp_of_formula x;
      ])

module Executor(Machine : Primus.Machine.S) : sig
  val formula : Primus.Value.t -> Formula.t option Machine.t
  val inputs : Input.t seq Machine.t
  val init : unit -> unit Machine.t
  val set_input : Input.t -> Primus.Value.t -> unit Machine.t
  val constraints : Formula.t list Machine.t
  val add_constraint : Primus.Value.t -> unit Machine.t
end = struct
  open Machine.Syntax

  module Value = Primus.Value.Make(Machine)
  module Mems = Primus.Memory.Make(Machine)
  module Debug = Debug(Machine)

  let id = Primus.Value.id

  let formula s x = Map.find s.formulae (id x)
  let word x = Formula.word @@ Primus.Value.to_word x
  let to_formula x = function
    | None -> word x
    | Some x -> x

  let lift1 x r f =
    Machine.Local.get executor >>= fun s ->
    match formula s x with
    | None -> Machine.return ()
    | Some x ->
      Machine.Local.put executor {
        s with
        formulae = Map.add_exn s.formulae (id r) (f x)
      }

  let lift2 x y r f =
    Machine.Local.get executor >>= fun s ->
    match formula s x, formula s y with
    | None,None -> Machine.return ()
    | fx,fy ->
      let x = to_formula x fx
      and y = to_formula y fy in
      Machine.Local.put executor {
        s with
        formulae = Map.add_exn s.formulae (id r) (f x y)
      }

  let lift3 x y z r f =
    Machine.Local.get executor >>= fun s ->
    match formula s x, formula s y, formula s z with
    | None,None,None -> Machine.return ()
    | fx,fy,fz ->
      let x = to_formula x fx
      and y = to_formula y fy
      and z = to_formula z fz in
      Machine.Local.put executor {
        s with
        formulae = Map.add_exn s.formulae (id r) (f x y z)
      }

  let on_binop ((op,x,y),z) = lift2 x y z @@ Formula.binop op
  let on_unop ((op,x),z) = lift1 x z @@ Formula.unop op
  let on_extract ((hi,lo,x),z) = lift1 x z @@ Formula.extract hi lo
  let on_concat ((x,y),z) = lift2 x y z @@ Formula.concat
  let on_ite ((c,x,y),z) = lift3 c x y z @@ Formula.ite
  let on_cast ((c,w,x),z) = lift1 x z @@ Formula.cast c w

  let set_input origin x =
    let id = Primus.Value.id x in
    let size = Input.size origin  in
    let x = Formula.var (Input.to_symbol origin) size in
    Machine.Local.update executor ~f:(fun s -> {
          s with
          inputs = Set.add s.inputs origin;
          formulae = Map.set s.formulae id x;
        })

  let on_memory_input (p,x) =
    Mems.memory >>= fun desc ->
    set_input (Input.ptr desc p) x

  let on_env_input (v,x) = set_input (Input.var v) x


  let inputs =
    Machine.Local.get executor >>| fun s ->
    Set.to_sequence s.inputs

  let add_constraint x =
    let refute = Value.is_zero x in
    Machine.Local.get executor >>= fun s ->
    match formula s x with
    | None -> Machine.return ()
    | Some x ->
      let x = if refute then Formula.unop NOT x else x in
      Machine.Local.put executor {
        s with constraints = x :: s.constraints;
      }

  let constraints =
    Machine.Local.get executor >>| fun s -> s.constraints

  let formula v =
    Machine.Local.get executor >>| fun s ->
    formula s v

  let init () = Machine.sequence Primus.Interpreter.[
      binop >>> on_binop;
      unop >>> on_unop;
      extract >>> on_extract;
      concat >>> on_concat;
      ite >>> on_ite;
      cast >>> on_cast;
      Primus.Memory.generated >>> on_memory_input;
      Primus.Env.generated >>> on_env_input;
      eval_cond >>> add_constraint;
    ]
end

type edge = {
  blk : blk term;
  src : jmp term;
  dst : Jmp.dst;
}

type task = {
  parent : task option;
  inputs : Input.t Seq.t;
  refute : bool;
  rhs : Formula.t;
  edge : edge option;
  hash : int;
}

type master = {
  self : Id.t option;
  worklist : task list;
  forks : int Map.M(Tid).t;
  dests : int Map.M(Tid).t;
  ctxts : int Map.M(Int).t;
}

type worker = {
  ctxt : int;                   (* the hash of the trace *)
  task : task option;
}

let master = Primus.Machine.State.declare
    ~uuid:"8d2b41d4-4852-40e9-917a-33f1f5af01a1"
    ~name:"symbolic-executor-master" @@ fun _ -> {
    self = None;
    worklist = [];
    forks = Tid.Map.empty;      (* queued or finished forks *)
    dests = Tid.Map.empty;      (* queued of finished dests *)
    ctxts = Int.Map.empty;      (* evaluated contexts *)
  }

let worker = Primus.Machine.State.declare
    ~uuid:"92f3042c-ba8f-465a-9d57-4c1b9ec7c186"
    ~name:"symbolic-executor-worker" @@ fun _ -> {
    ctxt = Hashtbl.hash 0;
    task = None
  }

let pp_dst ppf dst = match Jmp.resolve dst with
  | First tid -> Format.fprintf ppf "%a" Tid.pp tid
  | Second _ -> Format.fprintf ppf "unk"

let pp_edge ppf {blk; src; dst} =
  Format.fprintf ppf "%a(%a) -> %as"
    Tid.pp (Term.tid blk)
    Tid.pp (Term.tid src) pp_dst dst

let pp_edge_option ppf = function
  | None -> Format.fprintf ppf "sporadic"
  | Some edge -> pp_edge ppf edge

let pp_task ppf {rhs; refute; edge; hash} =
  Format.fprintf ppf "%08x: %a s.t.%s:@\n%s"
    hash
    pp_edge_option edge
    (if refute then " not" else "")
    (Formula.to_string rhs)

let forker ctxt : Primus.component =
  let cutoff = Extension.Configuration.get ctxt cutoff in
  let module Forker(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    module Executor = Executor(Machine)
    module Eval = Primus.Interpreter.Make(Machine)
    module Value = Primus.Value.Make(Machine)
    module Visited = Bap_primus_track_visited.Set.Make(Machine)
    module Debug = Debug(Machine)

    let is_conditional jmp = match Jmp.cond jmp with
      | Bil.Int _ -> false
      | _ -> true

    (* is Some (blk,jmp) when the current position is a branch point *)
    let branch_point = function
      | Primus.Pos.Jmp {up={me=blk}; me=jmp}
        when Term.length jmp_t blk > 1 &&
             is_conditional jmp ->
        Some (blk,jmp)
      | _ -> None


    let other_dst ~taken blk jmp =
      let is_other = if taken
        then Fn.non (Term.same jmp)
        else Term.same jmp in
      Term.enum jmp_t blk |>
      Seq.find_map ~f:(fun jmp ->
          if not (is_other jmp) then None
          else match Jmp.alt jmp with
            | None -> Jmp.dst jmp
            | dst -> dst) |> function
      | Some dst -> dst
      | None ->
        failwithf "Broken branch %a at:@\n%a"
          Jmp.pps jmp Blk.pps blk ()

    let count table key = match Map.find table key with
      | None -> 0
      | Some x -> x

    let incr table key = Map.update table key ~f:(function
        | None -> 1
        | Some n -> n + 1)

    let edge_knowledge visited master {src;dst} =
      count master.forks (Term.tid src) +
      match Jmp.resolve dst with
      | Second _ -> 0
      | First tid ->
        count master.dests tid +
        if Set.mem visited tid then 1 else 0

    let obtained_knowledge visited master hash edge =
      count master.ctxts hash + match edge with
      | None -> 0
      | Some e -> edge_knowledge visited master e

    let compute_edge taken pos = match branch_point pos with
      | None -> None
      | Some (blk,src) ->
        let dst = other_dst ~taken blk src in
        Some {blk;src;dst}

    let push_task task s =
      let s = {
        s with ctxts = incr s.ctxts task.hash;
               worklist = task :: s.worklist;
      } in
      match task.edge with
      | None -> s
      | Some {src; dst} -> {
          s with
          forks = incr s.forks (Term.tid src);
          dests = match Jmp.resolve dst with
            | First dst -> incr s.dests dst
            | Second _ -> s.dests
        }

    let on_cond cnd =
      let taken = Word.is_one (Primus.Value.to_word cnd) in
      Eval.pos >>= fun pos ->
      let edge = compute_edge taken pos in
      Visited.all >>= fun visited ->
      Machine.Global.get master >>= fun s ->
      Machine.Local.get worker >>= fun {ctxt=hash; task=parent} ->
      let known = obtained_knowledge visited s hash edge in
      if known > cutoff
      then Machine.return ()
      else
        Executor.formula cnd >>= function
        | None -> Machine.return ()
        | Some rhs ->
          Executor.inputs >>= fun inputs ->
          let task = {refute = taken; inputs; rhs; edge; hash; parent} in
          Machine.Global.update master ~f:(push_task task)

    let pop_task () =
      let rec pop visited s = function
        | [] -> None
        | t :: ts ->
          if obtained_knowledge visited s t.hash t.edge > cutoff
          then pop visited s ts
          else Some (t, ts) in
      Machine.Global.get master >>= fun s ->
      Visited.all >>= fun visited ->
      match pop visited s s.worklist with
      | None -> Machine.Global.put master {
          s with worklist=[]
        } >>| fun () -> None
      | Some (t,ts) -> Machine.Global.put master {
          s with worklist = ts
        } >>| fun () ->
        Some t

    let assert_parents task =
      let rec collect asserts = function
        | None -> asserts
        | Some {rhs; parent} -> collect (rhs::asserts) parent in
      collect [] task

    let exec_task ({inputs; refute; rhs} as task) =
      let constraints = assert_parents task.parent in
      match Formula.solve ~constraints ~refute rhs with
      | None ->
        Eval.halt >>|
        never_returns
      | Some model ->
        Machine.Local.update worker ~f:(fun t ->
            {t with task = Some task}) >>= fun () ->
        let string_of_input = Input.to_symbol in
        let module Input = Input.Make(Machine) in
        Machine.Seq.iter inputs ~f:(fun input ->
            match Formula.Model.get model input with
            | None -> Machine.return ()
            | Some value ->
              Debug.msg "%s = %s"
                (string_of_input input)
                (Formula.Value.to_string value) >>= fun () ->
              Value.of_word (Formula.Value.to_word value) >>= fun value ->
              Input.set input value)

    let rec run_master system =
      Machine.Global.get master >>= fun s ->
      match s.self with
      | None ->
        Machine.current () >>= fun master_pid ->
        Debug.msg "starting the master process with pid %a"
          Id.pp master_pid >>= fun () ->
        Machine.Global.put master {
          s with self = Some master_pid
        } >>= fun () ->
        Machine.fork () >>= fun () ->
        Machine.current () >>= fun client_pid ->
        Debug.msg "the first fork returns at %a"
          Id.pp client_pid >>= fun () ->
        if Id.equal master_pid client_pid
        then run_master system
        else Machine.return ()
      | Some master_pid ->
        pop_task () >>= function
        | None ->
          Debug.msg "Worklist is empty, finishing" >>= fun () ->
          Eval.halt >>| never_returns
        | Some t  ->
          Debug.msg "We have some tasks" >>= fun () ->
          Machine.fork () >>= fun () ->
          Machine.current () >>= fun client_pid ->
          if Id.equal master_pid client_pid
          then
            Debug.msg "Client is finished, master is resumed" >>= fun () ->
            run_master system
          else
            Debug.msg "Forked a new machine" >>= fun () ->
            exec_task t

    let init () = Machine.sequence Primus.Interpreter.[
        eval_cond >>> on_cond;
        Primus.System.start >>> run_master;
      ]
  end in (module Forker)


let sexp_of_assert_failure (name,model,formula,inputs) =
  Sexp.List (
    Atom name ::
    sexp_of_formula formula ::
    List.filter_map inputs ~f:(fun input ->
        match Formula.Model.get model input with
        | None -> None
        | Some value -> Option.some @@ Sexp.List [
            Sexp.Atom (Input.to_symbol input);
            Sexp.Atom (Formula.Value.to_string value)
          ]))

let assert_failure,failed_assertion =
  Primus.Observation.provide "assert-failure"
    ~inspect:sexp_of_assert_failure
    ~desc:"occurs when a symbolic executor assertion doesn't hold"

let to_word x = Primus.Value.to_word x
let to_int x = to_word x |> Word.to_int_exn

module SymbolicPrimitives(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Lisp = Primus.Lisp.Make(Machine)
  module Mems = Primus.Memory.Make(Machine)
  module Val = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Executor = Executor(Machine)
  module Closure = Primus.Lisp.Closure.Make(Machine)
  module Debug = Debug(Machine)

  let addr_size arch_addr_size = function
    | [addr_size; _data_size] -> to_int addr_size
    | [] | [_] -> arch_addr_size
    | _ -> failwith "symbolic-memory: expects 3 to 5 arguments"

  let data_size = function
    | [_addr_size; data_size] -> to_int data_size
    | _ -> 8

  let rec set_inputs bank data_size lower upper =
    if Word.(lower > upper)
    then Machine.return ()
    else
      Val.zero data_size >>=
      Executor.set_input (Input.ptr bank lower) >>= fun () ->
      set_inputs bank data_size (Word.succ lower) upper

  let register_memory m =
    let name = Bank.name m.bank in
    Machine.Global.update memories ~f:(fun mem ->
        Map.update mem name ~f:(function
            | None -> m
            | Some m' when equal_memory m m' -> m
            | _ ->
              invalid_argf "symbolic-memory: %s was previously \
                            defined with different parameters"
                name ()))

  let with_memory {bank} perform =
    Mems.memory >>= fun current ->
    Mems.switch bank >>= fun () ->
    perform >>= fun x ->
    Mems.switch current >>| fun () ->
    x

  let open_bank id =
    Machine.Global.get memories >>= fun mems ->
    Val.Symbol.of_value id >>= fun name ->
    match Map.find mems name with
    | None ->
      Lisp.failf "unknown memory %s" name ()
    | Some bank -> Machine.return bank

  let read id ptr =
    open_bank id >>= fun bank ->
    with_memory bank (Mems.get (to_word ptr))

  let write id ptr data =
    open_bank id >>= fun bank ->
    with_memory bank (Mems.set (to_word ptr) data) >>| fun () ->
    ptr

  let width default_width = function
    | [] -> default_width
    | s :: _ -> to_int s

  let value width = function
    | [_; x] -> Machine.return x
    | _ -> Val.of_int ~width 0

  let arch_addr_size =
    Machine.gets Project.arch >>|
    Arch.addr_size >>|
    Size.in_bits

  let create_value var rest =
    arch_addr_size >>= fun default_width ->
    let width = width default_width rest in
    Val.Symbol.of_value var >>= fun name ->
    let var = Var.create ("R_" ^ name) (Imm width) in
    Env.has var >>= function
    | true -> Env.get var
    | false ->
      value width rest >>= fun x ->
      Executor.set_input (Input.var var) x >>= fun () ->
      Env.set var x >>| fun () ->
      x

  let create_memory id lower upper rest =
    arch_addr_size >>= fun arch_addr_size ->
    Val.Symbol.of_value id >>= fun name ->
    Machine.Global.get memories >>= fun memories ->
    if Map.mem memories name then Machine.return id
    else
      let data_size = data_size rest in
      let bank = Bank.create name
          ~addr_size:(addr_size arch_addr_size rest)
          ~data_size in
      let lower = to_word lower and upper = to_word upper in
      let len = match Word.(to_int (upper - lower)) with
        | Error _ ->
          invalid_arg "symbolic-memory: the region is too large"
        | Ok diff -> diff + 1 in
      let memory = {bank; ptr=lower; len} in
      register_memory memory >>= fun () ->
      let width = Bank.data_size bank in
      let seed = Hashtbl.hash (Bank.name bank) in
      let generator = Primus.Generator.Random.lcg ~width seed in
      with_memory memory (Mems.allocate ~generator lower len) >>= fun () ->
      set_inputs bank data_size lower upper >>| fun () ->
      id

  let assume assumptions =
    Machine.List.iter assumptions ~f:Executor.add_constraint
    >>= fun () ->
    Val.b1


  let assert_ name assertions =
    Val.Symbol.of_value name >>= fun name ->
    Machine.List.iter assertions ~f:(fun assertion ->
        Executor.constraints >>= fun constraints ->
        Executor.formula assertion >>= fun x ->
        let x = match x with
          | None -> Formula.word (Primus.Value.to_word assertion)
          | Some x -> x in
        Machine.List.iter constraints ~f:(fun constr ->
            Debug.msg "s.t. %s" (Formula.to_string constr)) >>= fun () ->
        Machine.Observation.post failed_assertion ~f:(fun report ->
            match Formula.solve ~constraints ~refute:true x with
            | None ->
              Debug.msg "it holds!" >>= fun () ->
              Machine.return ()
            | Some model ->
              Debug.msg "it doesn't hold!" >>= fun () ->
              Executor.inputs >>| Seq.to_list >>= fun inputs ->
              report (name,model,x,inputs))) >>= fun () ->
    Val.b1

  let run args =
    Closure.name >>= fun name -> match name,args with
    | "symbolic-value", id :: rest -> create_value id rest
    | "symbolic-memory", (id :: lower :: upper :: rest) ->
      create_memory id lower upper rest
    | "symbolic-memory-read", [id; ptr] -> read id ptr
    | "symbolic-memory-write", [id; ptr; data] -> write id ptr data
    | "symbolic-assume", args -> assume args
    | "symbolic-assert", name :: args -> assert_ name args
    | _ -> Lisp.failf "%s: invalid number of arguments" name ()
end

module Primitives(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let def name types docs =
    Lisp.define name (module SymbolicPrimitives) ~types ~docs

  let init () = Machine.sequence [
      def "symbolic-assume" (all bool @-> bool)
        "Assumes the specified list of constraints.";
      def "symbolic-assert" (one sym // all bool @-> bool)
        "(assert NAME C1 ... CM) verifies the specified
         list of assertions C1 and posts the assert-failure observation
         for each condition that doesn't hold.";
      def "symbolic-value" (one sym // all int @-> a)
        "(symbolic-value NAME &rest) creates a new symbolic value.
        Accepts up to two optional arguments. The first argument
        denotes the bitwidth of the created value and the second is
        the initial value.";
      def "symbolic-memory" (tuple [sym; int; int] // all int @-> sym)
        "(symbolic-memory NAME LOWER UPPER &rest) creates a new
        symbolic memory region with the given NAME and LOWER and UPPER
        bounds. The symbolic memory could be read and written with
        the SYMBOLIC-MEMORY-READ and SYMBOLIC-MEMORY-WRITE primitives.
        Optionally accepts one or two more arguments. If two optional
        arguments are provided, then the first one stands for the
        address bus size of the symbolic memory and the second for the
        data bus size, all in bits. If only one is provided then it is
        interpreted as the data bus size and the address size defaults
        to the architecture-specific address size (e.g., 64 bits for
        amd64). If none are provided then the address size is
        architecture-specific and the data size defaults to 8 bits.";
      def "symbolic-memory-read" (tuple [sym; int] @-> byte)
        "(symbolic-memory-read MEM POS) reads the value of the
         symbolic memory MEM at the specified position POS.";
      def "symbolic-memory-write" (tuple [sym; int; byte] @-> byte)
        "(symbolic-memory-write MEM POS X) writes X to the symbolic
         memory MEM at the specified position POS."
    ]
end

let () = Extension.declare  @@ fun ctxt ->
  Primus.Components.register_generic "symbolic-computer"
    (module Executor) ~package:"bap"
    ~desc:"Computes a symbolic formula for each Primus value.";
  Primus.Components.register_generic "symbolic-path-explorer"
    (forker ctxt) ~package:"bap"
    ~desc:"Computes a path constraint for each branch and forks a \
           new machine if the constraint is satisfiable.";
  Primus.Components.register_generic "symbolic-lisp-primitives"
    ~package:"bap"
    ~desc:"Provides assume and assert primitives."
    (module Primitives);
  Ok ()
