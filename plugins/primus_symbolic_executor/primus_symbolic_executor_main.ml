open Core_kernel
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Bap_main
include Self()

(* Algorithm.

   For each system, just when we start it, we create the master
   machine that will be used to fork all other machines and re-execute
   the path N times, where N is bounded by cutoff times the number of
   linearly independent paths starting fromt the entry point. The master
   machine spawns a new worker for each task in its queue. Each task
   denotes a set of constraints that should be satisfied under the
   given set of inputs. The first worker machine starts with an empty
   set of constraints and inputs.

   Each worker (including the pioneer) runs from the entry point and
   builds the path formula. With each value we associate a formula,
   which we optimize on the fly. We designate certain values as inputs
   and keep them symbolic. When we hit the branch that we didn't see
   before and that leads to the desitantion which we neither saw nor
   plan to see, we take the formula associated with that branch
   condition and switch back to the init machine, and push its
   negation to the queue.

   When a worker finishes, the control is passed back to the init
   machine that picks the first actual formula and finds a satisfying
   model for it, then spawns a new worker process, which works as
   above.

   To identify which formula is actual, we associate with each formula
   the branch destination that will be taken if the formula is
   satisfiable. A formula is actual if this destination wasn't visited
   by a worker before. An unresolved destination is treated as
   unvisited, therefore it is always actual.  *)

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

module SMT : sig
  type formula
  type expr
  type value
  type model

  val formula : ?refute:bool -> expr -> formula
  val inverse : formula -> formula

  val word : Word.t -> expr
  val var : string -> int -> expr

  val binop : binop -> expr -> expr -> expr
  val unop : unop -> expr -> expr
  val extract : int -> int -> expr -> expr
  val concat : expr -> expr -> expr
  val ite : expr -> expr -> expr -> expr
  val cast : cast -> int -> expr -> expr

  val check : formula list -> model option

  module Model : sig
    type t = model
    val get : model -> Input.t -> value option
  end

  module Value : sig
    type t = value
    val to_word : value -> word
    val to_formula : value -> formula
    val to_string : value -> string
    include Base.Comparable.S with type t := value
  end

  module Formula : Base.Comparable.S with type t = formula

  val pp_expr : Format.formatter -> expr -> unit
  val pp_formula : Format.formatter -> formula -> unit
  val pp_value : Format.formatter -> value -> unit
end = struct
  module Bitv = Z3.BitVector
  module Bool = Z3.Boolean
  module Expr = Z3.Expr
  module Sort = Z3.Sort

  type expr = Expr.expr
  type formula = expr
  type value = expr

  let ctxt = Z3.mk_context [
      "model", "true";
    ]

  let to_string = Expr.to_string
  let pp_expr ppf expr = Format.fprintf ppf "%s" (to_string expr)
  let pp_formula = pp_expr
  let pp_value = pp_expr

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

  let inverse x = do_simpl@@Bool.mk_not ctxt x

  let formula ?(refute=false) expr =
    let lhs = Expr.mk_numeral_int ctxt 0 (Expr.get_sort expr) in
    let formula = Bool.mk_eq ctxt lhs expr in
    if refute then do_simpl@@formula else inverse formula


  module Model = struct
    type t = model
    let get {model} input =
      let var = var (Input.to_symbol input) (Input.size input) in
      match Z3.Model.eval model var false with
      | Some x when Expr.is_numeral x -> Some x
      | _ -> None
  end

  let solver = Z3.Solver.mk_simple_solver ctxt

  let check assertions =
    Z3.Solver.push solver;
    Z3.Solver.add solver assertions;
    let result = match Z3.Solver.check solver [] with
      | UNSATISFIABLE | UNKNOWN -> None
      | SATISFIABLE -> match Z3.Solver.get_model solver with
        | None -> None
        | Some model -> Some {model} in
    Z3.Solver.pop solver 1;
    result

  module Formula = struct
    type t = formula
    include Base.Comparable.Make(struct
        type t = formula
        let compare = Expr.compare
        let sexp_of_t x = Sexp.Atom (to_string x)
      end)
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

    include (Formula : Base.Comparable.S with type t := value)
  end
end

type executor = {
  inputs : Set.M(Input).t;
  values : SMT.expr Primus.Value.Id.Map.t;
}

let executor = Primus.Machine.State.declare
    ~uuid:"e21aa0fe-bc37-48e3-b398-ce7e764843c8"
    ~name:"symbolic-executor-formulae" @@ fun _ -> {
    values = Primus.Value.Id.Map.empty;
    inputs = Set.empty (module Input);
  }



module Context : sig
  type scope

  module Scope : sig
    type t = scope
    val declare : string -> scope
    val user : scope
    val path : scope

    include Base.Comparable.S with type t := t
  end

  module Make(Machine : Primus.Machine.S) : sig
    val add : scope -> SMT.formula -> unit Machine.t
    val get : scope -> Set.M(SMT.Formula).t Machine.t
  end
end = struct

  type scope = string

  type context = {
    assertions : Set.M(SMT.Formula).t Map.M(String).t
  }

  let context = Primus.Machine.State.declare
      ~uuid:"9538b1fa-b6c0-464a-b11e-22377a10b0b6"
      ~name:"symbolic-context" @@ fun _ -> {
      assertions = Map.empty (module String);
    }


  module Scope = struct
    let scopes = String.Hash_set.create ()

    let declare scope =
      if Hash_set.mem scopes scope
      then invalid_argf "the scope named %s \
                         is already declared" scope ();
      scope

    let user = declare "user"
    let path = declare "path"
    include (String : Base.Comparable.S with type t = scope)
  end

  module Make(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    let add scope x = Machine.Local.update context ~f:(fun s -> {
          assertions = Map.update s.assertions scope ~f:(function
              | None -> Set.singleton (module SMT.Formula) x
              | Some xs -> Set.add xs x)
        })

    let get scope =
      Machine.Local.get context >>| fun s ->
      match Map.find s.assertions scope with
      | None -> Set.empty (module SMT.Formula)
      | Some xs -> xs
  end
end

module Executor(Machine : Primus.Machine.S) : sig
  val value : Primus.Value.t -> SMT.expr option Machine.t
  val inputs : Input.t seq Machine.t
  val init : unit -> unit Machine.t
  val set_input : Input.t -> Primus.Value.t -> unit Machine.t
end = struct
  open Machine.Syntax

  module Value = Primus.Value.Make(Machine)
  module Mems = Primus.Memory.Make(Machine)
  module Debug = Debug(Machine)
  module Ctxt = Context.Make(Machine)

  let id = Primus.Value.id

  let value s x = Map.find s.values (id x)
  let word x = SMT.word @@ Primus.Value.to_word x
  let to_formula x = function
    | None -> word x
    | Some x -> x

  let lift1 x r f =
    Machine.Local.get executor >>= fun s ->
    match value s x with
    | None -> Machine.return ()
    | Some x ->
      Machine.Local.put executor {
        s with
        values = Map.add_exn s.values (id r) (f x)
      }

  let lift2 x y r f =
    Machine.Local.get executor >>= fun s ->
    match value s x, value s y with
    | None,None -> Machine.return ()
    | fx,fy ->
      let x = to_formula x fx
      and y = to_formula y fy in
      Machine.Local.put executor {
        s with
        values = Map.add_exn s.values (id r) (f x y)
      }

  let lift3 x y z r f =
    Machine.Local.get executor >>= fun s ->
    match value s x, value s y, value s z with
    | None,None,None -> Machine.return ()
    | fx,fy,fz ->
      let x = to_formula x fx
      and y = to_formula y fy
      and z = to_formula z fz in
      Machine.Local.put executor {
        s with
        values = Map.add_exn s.values (id r) (f x y z)
      }

  let on_binop ((op,x,y),z) = lift2 x y z @@ SMT.binop op
  let on_unop ((op,x),z) = lift1 x z @@ SMT.unop op
  let on_extract ((hi,lo,x),z) = lift1 x z @@ SMT.extract hi lo
  let on_concat ((x,y),z) = lift2 x y z @@ SMT.concat
  let on_ite ((c,x,y),z) = lift3 c x y z @@ SMT.ite
  let on_cast ((c,w,x),z) = lift1 x z @@ SMT.cast c w

  let set_input origin x =
    let id = Primus.Value.id x in
    let size = Input.size origin  in
    let x = SMT.var (Input.to_symbol origin) size in
    Machine.Local.update executor ~f:(fun s -> {
          inputs = Set.add s.inputs origin;
          values = Map.set s.values id x;
        })

  let on_memory_input (p,x) =
    Mems.memory >>= fun desc ->
    set_input (Input.ptr desc p) x

  let on_env_input (v,x) = set_input (Input.var v) x

  let inputs =
    Machine.Local.get executor >>| fun s ->
    Set.to_sequence s.inputs

  let value v =
    Machine.Local.get executor >>| fun s ->
    value s v

  let init () = Machine.sequence Primus.Interpreter.[
      binop >>> on_binop;
      unop >>> on_unop;
      extract >>> on_extract;
      concat >>> on_concat;
      ite >>> on_ite;
      cast >>> on_cast;
      Primus.Memory.generated >>> on_memory_input;
      Primus.Env.generated >>> on_env_input;
    ]
end

module Paths(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Constraints = Context.Make(Machine)
  module Executor = Executor(Machine)
  module Value = Primus.Value.Make(Machine)

  let add_constraint x =
    Executor.value x >>= function
    | None -> Machine.return ()
    | Some v ->
      Constraints.add Context.Scope.path @@
      SMT.formula ~refute:(Value.is_zero x) v

  let init () =
    Primus.Interpreter.eval_cond >>> add_constraint
end

type edge = {
  blk : blk term;
  src : jmp term;
  dst : Jmp.dst;
}

type task = {
  constraints : Set.M(SMT.Formula).t;
  inputs : Set.M(Input).t;
  edge : edge option;
  hash : int;
}

type master = {
  self : Id.t option;
  ready : int;
  tasks : task Map.M(Int).t;
  forks : int Map.M(Tid).t;
  dests : int Map.M(Tid).t;
  ctxts : int Map.M(Int).t;
}

type worker = {
  ctxt : int;                   (* the hash of the trace *)
  task : task;
}

let master = Primus.Machine.State.declare
    ~uuid:"8d2b41d4-4852-40e9-917a-33f1f5af01a1"
    ~name:"symbolic-executor-master" @@ fun _ -> {
    self = None;
    tasks = Int.Map.empty;
    forks = Tid.Map.empty;      (* queued or finished forks *)
    dests = Tid.Map.empty;      (* queued of finished dests *)
    ctxts = Int.Map.empty;      (* evaluated contexts *)
    ready = 0;
  }

let worker = Primus.Machine.State.declare
    ~uuid:"92f3042c-ba8f-465a-9d57-4c1b9ec7c186"
    ~name:"symbolic-executor-worker" @@ fun _ -> {
    ctxt = Hashtbl.hash 0;
    task = {
      hash = Hashtbl.hash 0;
      edge = None;
      constraints = Set.empty (module SMT.Formula);
      inputs = Set.empty (module Input);
    };
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

let pp_constraints ppf constrs =
  Set.iter constrs ~f:(Format.fprintf ppf "%a@\n" SMT.pp_formula)

let pp_task ppf {constraints=cs; edge; hash} =
  Format.fprintf ppf "%08x: %a s.t.@\n%a"
    hash
    pp_edge_option edge
    pp_constraints cs

let forker ctxt : Primus.component =
  let cutoff = Extension.Configuration.get ctxt cutoff in
  let module Forker(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    module Executor = Executor(Machine)
    module Eval = Primus.Interpreter.Make(Machine)
    module Value = Primus.Value.Make(Machine)
    module Visited = Bap_primus_track_visited.Set.Make(Machine)
    module Constraints = Context.Make(Machine)
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

    let compute_edge ~taken pos = match branch_point pos with
      | None -> None
      | Some (blk,src) ->
        let dst = other_dst ~taken blk src in
        Some {blk;src;dst}

    let append xs x = match Map.max_elt xs with
      | None -> Map.add_exn xs 0 x
      | Some (k,_) -> Map.add_exn xs (k+1) x

    let push_task task s =
      let s = {
        s with ctxts = incr s.ctxts task.hash;
               tasks = append s.tasks task;
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

    let on_cond constr =
      let taken = Value.is_one constr in
      Executor.value constr >>= function
      | None -> Machine.return ()
      | Some constr ->
        Machine.Local.get worker >>= fun self ->
        Eval.pos >>= fun pos ->
        let edge = compute_edge ~taken pos in
        Visited.all >>= fun visited ->
        Machine.Global.get master >>= fun s ->
        let known = obtained_knowledge visited s self.ctxt edge in
        if known > cutoff ||
           Set.mem self.task.constraints
             (SMT.formula ~refute:(not taken) constr)
        then Machine.return ()
        else
          Constraints.get Context.Scope.user >>= fun cs ->
          let constr = SMT.formula ~refute:taken constr in
          let constraints =
            let (++) = Set.union in
            Set.add (cs ++ self.task.constraints) constr in
          Executor.inputs >>= fun inputs ->
          let inputs =
            Seq.fold inputs ~init:self.task.inputs ~f:Set.add in
          let task = {inputs; constraints; edge; hash=self.ctxt} in
          Machine.Global.update master ~f:(push_task task)

    let pop_task () =
      let rec pop visited s ts =
        match Map.max_elt ts with
        | None -> None
        | Some (k,t) ->
          let ts = Map.remove ts k in
          if obtained_knowledge visited s t.hash t.edge > cutoff
          then pop visited s ts
          else match SMT.check (Set.to_list t.constraints) with
            | None -> pop visited s ts
            | Some m -> Some (m,t, ts) in
      Machine.Global.get master >>= fun s ->
      Visited.all >>= fun visited ->
      match pop visited s s.tasks with
      | None ->
        report_progress ~stage:(s.ready-1) ~total:s.ready ();
        Machine.Global.put master {
          s with tasks = Int.Map.empty
        } >>| fun () -> None
      | Some (m,t,ts) ->
        let queued = Map.length s.tasks in
        let left = Map.length ts in
        let processed = queued - left in
        report_progress
          ~stage:(s.ready + processed - 1)
          ~total:(s.ready + queued) ();
        Machine.Global.put master {
          s with tasks = ts;
                 ready = s.ready + processed;
        } >>| fun () ->
        Some (m,t)

    let exec_task model task =
      Debug.msg "SAT" >>= fun () ->
      Machine.Local.update worker ~f:(fun t ->
          {t with task}) >>= fun () ->
      let string_of_input = Input.to_symbol in
      let inputs = Set.to_sequence task.inputs in
      let module Input = Input.Make(Machine) in
      Machine.Seq.iter inputs ~f:(fun input ->
          match SMT.Model.get model input with
          | None -> Machine.return ()
          | Some value ->
            Debug.msg "%s = %a"
              (string_of_input input)
              SMT.pp_value value >>= fun () ->
            Value.of_word (SMT.Value.to_word value) >>= fun value ->
            Executor.set_input input value >>= fun () ->
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
        | Some (m,t)  ->
          Debug.msg "We have some tasks" >>= fun () ->
          Machine.fork () >>= fun () ->
          Machine.current () >>= fun client_pid ->
          if Id.equal master_pid client_pid
          then
            Debug.msg "Client is finished, master is resumed" >>= fun () ->
            run_master system
          else
            Debug.msg "Forked a new machine" >>= fun () ->
            exec_task m t

    let update_hash t =
      Machine.Local.update worker ~f:(fun s -> {
            s with
            ctxt = Tid.hash t lxor s.ctxt
          })

    let init () = Machine.sequence Primus.Interpreter.[
        eval_cond >>> on_cond;
        enter_term >>> update_hash;
        Primus.System.start >>> run_master;
      ]
  end in (module Forker)

let sexp_of_assert_failure (name,model,inputs) =
  Sexp.List (
    Atom name ::
    List.filter_map inputs ~f:(fun input ->
        match SMT.Model.get model input with
        | None -> None
        | Some value -> Option.some @@ Sexp.List [
            Sexp.Atom (Input.to_symbol input);
            Sexp.Atom (SMT.Value.to_string value)
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
  module Constraints = Context.Make(Machine)
  module Debug = Debug(Machine)

  let addr_size arch_addr_size = function
    | [addr_size; _data_size] -> Ok (to_int addr_size)
    | [] | [_] -> Ok arch_addr_size
    | _ ->  Error "symbolic-memory: expects 3 to 5 arguments"

  let data_size = function
    | [_addr_size; data_size] -> to_int data_size
    | _ -> 8

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

  let generator width = function
    | [_; x] ->
      let x = Word.to_int_exn @@ Primus.Value.to_word x in
      Primus.Generator.static ~width x
    | _ -> Primus.Generator.static ~width 0

  let arch_addr_size =
    Machine.gets Project.arch >>|
    Arch.addr_size >>|
    Size.in_bits

  let create_value var rest =
    arch_addr_size >>= fun default_width ->
    let width = width default_width rest in
    Val.Symbol.of_value var >>= fun name ->
    let var = Var.create name (Imm width) in
    Env.has var >>= function
    | true -> Env.get var
    | false ->
      Env.add var (generator width rest) >>= fun () ->
      Env.get var

  let create_memory id lower upper rest =
    let lower = to_word lower and upper = to_word upper in
    let data_size = data_size rest in
    arch_addr_size >>= fun arch_addr_size ->
    Val.Symbol.of_value id >>= fun name ->
    Machine.Global.get memories >>= fun memories ->
    if Map.mem memories name
    then Machine.return id
    else match addr_size arch_addr_size rest with
      | Error err -> Lisp.failf "symbolic-memory: %s" err ()
      | Ok addr_size ->
        let bank = Bank.create name ~addr_size ~data_size in
        match Word.(to_int (upper - lower)) with
        | Error _ ->
          Lisp.failf "symbolic-memory: unable to create \
                      a symbolic memory region, it is too large" ()
        | Ok diff ->
          let len = diff + 1 in
          let memory = {bank; ptr=lower; len} in
          register_memory memory >>= fun () ->
          let width = Bank.data_size bank in
          let seed = Hashtbl.hash (Bank.name bank) in
          let generator = Primus.Generator.Random.lcg ~width seed in
          let region = Mems.allocate ~generator lower len in
          with_memory memory region >>| fun () ->
          id

  let assume assumptions =
    Machine.List.iter assumptions ~f:(fun x ->
        Executor.value x >>= function
        | Some x ->
          Constraints.add Context.Scope.user (SMT.formula x)
        | None ->
          (* should we raise on assert false? *)
          if Val.is_zero x then
            let false_ = SMT.word Word.b0 in
            Constraints.add Context.Scope.user (SMT.formula false_)
          else Machine.return ())
    >>= fun () ->
    Val.b1

  (* we can later add operators that manipulate the scopes
     in which the assertion is checked *)
  let assert_ name assertions =
    Machine.Observation.post failed_assertion ~f:(fun report ->
        Constraints.get Context.Scope.user >>= fun user ->
        Constraints.get Context.Scope.path >>= fun path ->
        let constraints = Set.to_list @@ Set.union user path in
        Machine.List.fold assertions ~init:constraints
          ~f:(fun constraints assertion ->
              Executor.value assertion >>| function
              | None -> constraints
              | Some expr ->

                SMT.formula ~refute:true expr :: constraints) >>|
        SMT.check >>= function
        | None -> Machine.return ()
        | Some model ->
          Val.Symbol.of_value name >>= fun name ->
          Debug.msg "%s doesn't hold!" name >>= fun () ->
          Executor.inputs >>| Seq.to_list >>= fun inputs ->
          report (name,model,inputs)) >>= fun () ->
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
  Primus.Components.register_generic "symbolic-path-constraints"
    ~package:"bap"
    ~desc:"tracks the path constraint and records them in the \
           machine's symbolic context"
    (module Paths) ;
  Ok ()
